//===--- SemaPortCosmo.cpp - AST Rewriting for -fportcosmo
//------------------===//
//
// Licensed under ISC License.
// See https://www.isc.org/licenses/ for license information.
// SPDX-License-Identifier: ISC License
//
//===-------------------------------------------------------------------------------===//
//
//  This file implements AST Rewriting when compiling with -fportcosmo.
//
//===-------------------------------------------------------------------------------===//

#include "CheckExprLifetime.h"
#include "TreeTransform.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/ASTLambda.h"
#include "clang/AST/CXXInheritance.h"
#include "clang/AST/CharUnits.h"
#include "clang/AST/DeclObjC.h"
#include "clang/AST/DynamicRecursiveASTVisitor.h"
#include "clang/AST/EvaluatedExprVisitor.h"
#include "clang/AST/ExprCXX.h"
#include "clang/AST/ExprObjC.h"
#include "clang/AST/IgnoreExpr.h"
#include "clang/AST/StmtCXX.h"
#include "clang/AST/StmtObjC.h"
#include "clang/AST/TypeLoc.h"
#include "clang/AST/TypeOrdering.h"
#include "clang/Basic/TargetInfo.h"
#include "clang/Lex/Preprocessor.h"
#include "clang/Sema/EnterExpressionEvaluationContext.h"
#include "clang/Sema/Initialization.h"
#include "clang/Sema/Lookup.h"
#include "clang/Sema/Ownership.h"
#include "clang/Sema/Scope.h"
#include "clang/Sema/ScopeInfo.h"
#include "clang/Sema/SemaCUDA.h"
#include "clang/Sema/SemaObjC.h"
#include "clang/Sema/SemaOpenMP.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/STLForwardCompat.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringExtras.h"
#include <sstream>

using namespace clang;
using namespace sema;

struct PortCosmoSwitchToIf : TreeTransform<PortCosmoSwitchToIf> {

  using CmpExpr2Label = std::pair<Expr *, Stmt *>;
  SmallVector<CmpExpr2Label> c2ls;
  VarDecl *swCondVarDecl;
  ExprResult swCondVarRHS;
  LabelStmt *swDefaultLabel;
  LabelStmt *swEndLabel;

  unsigned int switchDepth;
  unsigned int forDepth;
  unsigned int whileDepth;
  unsigned int doWhileDepth;

  PortCosmoSwitchToIf(Sema &SemaRef) : TreeTransform(SemaRef) {
    swCondVarDecl = nullptr;
    swEndLabel = nullptr;
    swDefaultLabel = nullptr;
    switchDepth = 0;
    forDepth = 0;
    whileDepth = 0;
    doWhileDepth = 0;
  }
  bool AlwaysRebuild() { return true; }
  bool ReplacingOriginal() { return false; }

  LabelStmt *createCaseLabel(CaseStmt *S, StmtResult SubStmt) const {
    std::stringstream caseNameStream;
    caseNameStream << "__portcosmo_ifsw_case_";
    caseNameStream << S->getCaseLoc().getHashValue();
    std::string caseName = caseNameStream.str();
    IdentifierInfo &caseInfo = SemaRef.PP.getIdentifierTable().get(caseName);
    LabelDecl *swCaseLabelDecl =
        LabelDecl::Create(SemaRef.getASTContext(), SemaRef.CurContext,
                          S->getCaseLoc(), &caseInfo);
    return new (SemaRef.getASTContext())
        LabelStmt(S->getCaseLoc(), swCaseLabelDecl, SubStmt.get());
  }

  LabelStmt *createDefaultLabel(DefaultStmt *S, StmtResult SubStmt) const {
    std::stringstream defaultNameStream;
    defaultNameStream << "__portcosmo_ifsw_dflt_";
    defaultNameStream << S->getColonLoc().getHashValue();
    std::string defaultName = defaultNameStream.str();
    IdentifierInfo &defaultInfo =
        SemaRef.PP.getIdentifierTable().get(defaultName);
    LabelDecl *swDefaultLabelDecl =
        LabelDecl::Create(SemaRef.getASTContext(), SemaRef.CurContext,
                          S->getColonLoc(), &defaultInfo);
    return new (SemaRef.getASTContext())
        LabelStmt(S->getColonLoc(), swDefaultLabelDecl, SubStmt.get());
  }

  LabelStmt *createEndLabel(SwitchStmt *S) const {
    std::stringstream endNameStream;
    endNameStream << "__portcosmo_ifsw_end_";
    endNameStream << S->getEndLoc().getHashValue();
    std::string endName = endNameStream.str();
    IdentifierInfo &endInfo = SemaRef.PP.getIdentifierTable().get(endName);
    LabelDecl *swEndLabelDecl = LabelDecl::Create(
        SemaRef.getASTContext(), SemaRef.CurContext, S->getEndLoc(), &endInfo);
    NullStmt *tmp = new (SemaRef.getASTContext()) NullStmt(S->getEndLoc());
    return new (SemaRef.getASTContext())
        LabelStmt(S->getEndLoc(), swEndLabelDecl, tmp);
  }

  VarDecl *createTempCondVarDecl(SwitchStmt *S, QualType CondType) const {
    std::stringstream LHSNameStream;
    LHSNameStream << "__portcosmo_ifsw_";
    LHSNameStream << S->getBeginLoc().getHashValue();
    std::string LHSName = LHSNameStream.str();
    const IdentifierInfo &varInfo =
        SemaRef.PP.getIdentifierTable().get(LHSName);
    return VarDecl::Create(
        SemaRef.getASTContext(), SemaRef.CurContext, S->getBeginLoc(),
        S->getLParenLoc(), &varInfo, CondType,
        SemaRef.getASTContext().CreateTypeSourceInfo(CondType), SC_Auto);
  }

  /// Transforms
  StmtResult TransformForStmt(ForStmt *S) {
    forDepth += 1;
    StmtResult R = TreeTransform<PortCosmoSwitchToIf>::TransformForStmt(S);
    forDepth -= 1;
    return R;
  }

  StmtResult TransformWhileStmt(WhileStmt *S) {
    whileDepth += 1;
    StmtResult R = TreeTransform<PortCosmoSwitchToIf>::TransformWhileStmt(S);
    whileDepth -= 1;
    return R;
  }

  StmtResult TransformDoStmt(DoStmt *S) {
    doWhileDepth += 1;
    StmtResult R = TreeTransform<PortCosmoSwitchToIf>::TransformDoStmt(S);
    doWhileDepth -= 1;
    return R;
  }

  StmtResult TransformSwitchStmt(SwitchStmt *S) {
    if (switchDepth != 0 || !S->nonConstCaseExists()) {
      return TreeTransform<PortCosmoSwitchToIf>::TransformSwitchStmt(S);
    }
    switchDepth += 1;

    StmtResult Init = getDerived().TransformStmt(S->getInit());
    if (Init.isInvalid())
      return StmtError();

    Sema::ConditionResult Cond =
        TransformCondition(S->getSwitchLoc(), S->getConditionVariable(),
                           S->getCond(), Sema::ConditionKind::Switch);
    if (Cond.isInvalid())
      return StmtError();

    // Cond is valid, so second has to exist
    assert(Cond.get().second != nullptr);
    swCondVarRHS = Cond.get().second;
    QualType CondType = swCondVarRHS.get()->getType();

    if (Cond.get().first) {
      swCondVarDecl = Cond.get().first;
    } else {
      swCondVarDecl = createTempCondVarDecl(S, CondType);
      swCondVarDecl->setImplicit();
      swCondVarDecl->setInit(Cond.get().second);
    }

    swEndLabel = createEndLabel(S);

    StmtResult Body = getDerived().TransformStmt(S->getBody());
    if (Body.isInvalid())
      return StmtError();

    std::vector<Stmt *> rparts;
    DeclGroupRef DGRef = DeclGroupRef::Create(SemaRef.getASTContext(),
                                              cast<Decl *>(&swCondVarDecl), 1);
    DeclStmt *cvarinit = new (SemaRef.getASTContext())
        DeclStmt(DGRef, S->getBeginLoc(), S->getBeginLoc());

    rparts.push_back(cvarinit);
    for (auto &p : c2ls) {
      LabelStmt *lst = cast<LabelStmt>(p.second);
      GotoStmt *targ = new (SemaRef.getASTContext())
          GotoStmt(lst->getDecl(), S->getBeginLoc(), S->getBeginLoc());
      IfStmt *ifp = IfStmt::Create(
          SemaRef.getASTContext(), S->getBeginLoc(), IfStatementKind::Ordinary,
          nullptr, nullptr, p.first, S->getBeginLoc(), S->getBeginLoc(), targ);
      rparts.push_back(ifp);
    }
    if (swDefaultLabel) {
      GotoStmt *targ = new (SemaRef.getASTContext()) GotoStmt(
          swDefaultLabel->getDecl(), S->getBeginLoc(), S->getBeginLoc());
      rparts.push_back(targ);
    }
    GotoStmt *etarg = new (SemaRef.getASTContext())
        GotoStmt(swEndLabel->getDecl(), S->getBeginLoc(), S->getBeginLoc());
    rparts.push_back(etarg);
    rparts.push_back(Body.get());
    rparts.push_back(swEndLabel);

    return RebuildCompoundStmt(S->getBeginLoc(), rparts, S->getEndLoc(), false);
  }

  StmtResult TransformCaseStmt(CaseStmt *S) {
    ExprResult LHS, RHS;
    ExprResult caseAsCompare;

    DeclRefExpr *swCVDE = DeclRefExpr::Create(
        SemaRef.getASTContext(), swCondVarDecl->getQualifierLoc(),
        SourceLocation(), swCondVarDecl, false, S->getColonLoc(),
        swCondVarDecl->getType(), ExprValueKind::VK_LValue);
    LHS = getDerived().TransformExpr(S->getLHS());
    if (LHS.isInvalid())
      return StmtError();

    if (S->caseStmtIsGNURange()) {
      RHS = getDerived().TransformExpr(S->getRHS());
      if (RHS.isInvalid())
        return StmtError();
      // create the comparison expression (range might be ill-defined)
      ExprResult ER0 = RebuildBinaryOperator(
          S->getCaseLoc(), BinaryOperatorKind::BO_LE, LHS.get(), swCVDE);
      ExprResult ER1 = RebuildBinaryOperator(
          S->getColonLoc(), BinaryOperatorKind::BO_GE, RHS.get(), swCVDE);
      caseAsCompare = RebuildBinaryOperator(S->getEllipsisLoc(),
                                            BinaryOperatorKind::BO_LAnd,
                                            ER0.get(), ER1.get());
    } else {
      // create the comparison expression (this is just an equality statement)
      caseAsCompare = RebuildBinaryOperator(
          S->getCaseLoc(), BinaryOperatorKind::BO_EQ, LHS.get(), swCVDE);
    }

    if (caseAsCompare.isInvalid()) {
      return StmtError();
    }

    StmtResult SubStmt = getDerived().TransformStmt(S->getSubStmt());
    if (SubStmt.isInvalid())
      return StmtError();

    StmtResult curCaseLabel = createCaseLabel(S, SubStmt);
    if (curCaseLabel.isInvalid()) {
      return StmtError();
    }
    c2ls.push_back(std::make_pair<Expr *, Stmt *>(caseAsCompare.get(),
                                                  curCaseLabel.get()));
    return curCaseLabel;
  }

  StmtResult TransformDefaultStmt(DefaultStmt *S) {
    StmtResult SubStmt = getDerived().TransformStmt(S->getSubStmt());
    if (SubStmt.isInvalid())
      return StmtError();

    swDefaultLabel = createDefaultLabel(S, SubStmt);
    return swDefaultLabel;
  }

  StmtResult TransformBreakStmt(BreakStmt *S) {
    if (forDepth || whileDepth || doWhileDepth) {
      return S;
    }
    return RebuildGotoStmt(S->getBeginLoc(), S->getEndLoc(),
                           swEndLabel->getDecl());
  }

  StmtResult TransformAttributedStmt(AttributedStmt *S, StmtDiscardKind SDK) {
    StmtResult SubStmt = getDerived().TransformStmt(S->getSubStmt(), SDK);
    if (SubStmt.isInvalid())
      return StmtError();

    bool AttrsChanged = false;
    SmallVector<const Attr *, 1> Attrs;

    for (const auto *I : S->getAttrs()) {
      const Attr *R = TransformStmtAttr(S->getSubStmt(), SubStmt.get(), I);
      if (!R) {
        continue;
      }
      if (isa<FallThroughAttr>(R)) {
        AttrsChanged = true;
        continue;
      }
      AttrsChanged |= (I != R);
      Attrs.push_back(R);
    }

    if (SubStmt.get() == S->getSubStmt() && !AttrsChanged)
      return S;

    if (Attrs.empty())
      return SubStmt;

    return RebuildAttributedStmt(S->getAttrLoc(), Attrs, SubStmt.get());
  }
};

StmtResult Sema::RewriteSwitchToIfStmt(SourceLocation SwitchLoc, Stmt *Switch,
                                       Stmt *BodyStmt,
                                       bool CaseListIsIncomplete) {
  SwitchStmt *SS = cast<SwitchStmt>(Switch);
  PortCosmoSwitchToIf mod(*this);
  StmtResult ifswitch = mod.TransformSwitchStmt(SS);
  if (ifswitch.isInvalid()) {
    return StmtError();
  }
  LLVM_DEBUG({
    llvm::dbgs() << "Transformed AST\n";
    ifswitch.get()->dump();
  });
  return ifswitch;
}

static StmtResult createInitStructMemcpy(VarDecl *dst, VarDecl *src,
                                         VarDecl *flag, Sema &SemaRef) {
  DeclRefExpr *dstRHS = DeclRefExpr::Create(
      SemaRef.getASTContext(), dst->getQualifierLoc(), SourceLocation(),
      cast<ValueDecl>(dst), false, dst->getBeginLoc(), dst->getType(),
      ExprValueKind::VK_LValue);
  DeclRefExpr *srcRHS = DeclRefExpr::Create(
      SemaRef.getASTContext(), dst->getQualifierLoc(), SourceLocation(),
      cast<ValueDecl>(src), false, dst->getBeginLoc(), src->getType(),
      ExprValueKind::VK_LValue);
  DeclRefExpr *flagRHS = DeclRefExpr::Create(
      SemaRef.getASTContext(), dst->getQualifierLoc(), SourceLocation(),
      cast<ValueDecl>(flag), false, dst->getBeginLoc(), flag->getType(),
      ExprValueKind::VK_LValue);
  //
  QualType SizeType = SemaRef.Context.getSizeType();
  llvm::APInt Size(
      SemaRef.Context.getTypeSize(SizeType),
      SemaRef.Context.getTypeSizeInChars(src->getType()).getQuantity());
  //
  const QualType flagType = SemaRef.Context.UnsignedShortTy;
  llvm::APInt flagOne(SemaRef.Context.getTypeSize(flagType), 1);
  Expr *One = IntegerLiteral::Create(SemaRef.Context, flagOne, flagType,
                                     SourceLocation());
  // flag != 1
  Expr *flag_NE_1 = BinaryOperator::Create(
      SemaRef.Context, flagRHS, One, BO_NE, flagType, VK_PRValue, OK_Ordinary,
      SourceLocation(), SemaRef.CurFPFeatureOverrides());
  // flag = 1
  Expr *flag_SET_1 = BinaryOperator::Create(
      SemaRef.Context, flagRHS, One, BO_Assign, flagType, VK_LValue,
      OK_Ordinary, SourceLocation(), SemaRef.CurFPFeatureOverrides());
  // &src
  Expr *From;
  if (src->getType()->isArrayType()) {
    From = srcRHS;
  } else {
    From = UnaryOperator::Create(SemaRef.Context, srcRHS, UO_AddrOf,
                                 SemaRef.Context.getPointerType(src->getType()),
                                 VK_PRValue, OK_Ordinary, dst->getBeginLoc(),
                                 false, SemaRef.CurFPFeatureOverrides());
  }
  // &dst
  Expr *To;
  if (dst->getType()->isArrayType()) {
    To = dstRHS;
  } else {
    To = UnaryOperator::Create(SemaRef.Context, dstRHS, UO_AddrOf,
                               SemaRef.Context.getPointerType(dst->getType()),
                               VK_LValue, OK_Ordinary, dst->getBeginLoc(),
                               false, SemaRef.CurFPFeatureOverrides());
  }
  // memcpy
  StringRef MemCpyName = "__builtin_memcpy";
  LookupResult R(SemaRef, &SemaRef.Context.Idents.get(MemCpyName),
                 dst->getBeginLoc(), Sema::LookupOrdinaryName);
  SemaRef.LookupName(R, SemaRef.TUScope, true);
  FunctionDecl *MemCpy = R.getAsSingle<FunctionDecl>();
  if (!MemCpy)
    return StmtError();
  ExprResult MemCpyRef =
      SemaRef.BuildDeclRefExpr(MemCpy, SemaRef.Context.BuiltinFnTy, VK_PRValue,
                               dst->getBeginLoc(), nullptr);
  assert(MemCpyRef.isUsable() && "Builtin reference cannot fail");
  // memcpy(&dst, &src, sizeof(src))
  Expr *CallArgs[] = {To, From,
                      IntegerLiteral::Create(SemaRef.Context, Size, SizeType,
                                             dst->getBeginLoc())};
  ExprResult Call =
      SemaRef.BuildCallExpr(nullptr, MemCpyRef.get(), dst->getBeginLoc(),
                            CallArgs, dst->getBeginLoc());
  if (Call.isInvalid()) {
    return StmtError();
  }
  // { flag = 1; memcpy(dst, src, sizeof(src)); }
  Stmt *ifParts[] = {flag_SET_1, Call.get()};
  CompoundStmt *ifPartsCombi = CompoundStmt::Create(
      SemaRef.Context, ifParts, SemaRef.CurFPFeatureOverrides(),
      dst->getBeginLoc(), dst->getEndLoc());
  // if (flag != 1) { flag = 1; memcpy(dst, src, sizeof(src)); }
  StmtResult result = IfStmt::Create(
      SemaRef.Context, dst->getBeginLoc(), IfStatementKind::Ordinary, nullptr,
      nullptr, flag_NE_1, dst->getEndLoc(), dst->getEndLoc(),
      cast<Stmt>(ifPartsCombi));
  return result;
}

StmtResult Sema::RewriteStaticDeclStmt(Stmt *S) {
  DeclStmt *DS = cast<DeclStmt>(S);
  DeclGroupRef DG = DS->getDeclGroup();
  std::stringstream nameStream;
  std::vector<Stmt *> rparts;
  const QualType flagType = Context.UnsignedShortTy;
  llvm::APInt flagZero(Context.getTypeSize(flagType), 0);
  VarDecl *vd = nullptr;
  QualType varType;
  DeclGroupRef dgr;
  DeclStmt *dcr = nullptr;
  DeclContext *DC = nullptr;
  unsigned int declCount = 0;

  auto addDecl = [&](VarDecl *v) {
    v->setDeclContext(DC);
    DC->addDecl(v);
    dgr = DeclGroupRef::Create(Context, cast<Decl *>(&v), 1);
    dcr = new (Context) DeclStmt(dgr, v->getBeginLoc(), v->getEndLoc());
    rparts.push_back(dcr);
  };

  for (auto it = DG.begin(); it != DG.end(); ++it) {
    if (!isa<VarDecl>(*it)) {
      continue;
    }
    vd = cast<VarDecl>(*it);
    varType = vd->getType();
    DC = vd->getDeclContext();
    nameStream.str("");
    nameStream.clear();
    if (vd->isStaticLocal() &&
        !vd->getInit()->isConstantInitializer(Context, false)) {
      // a local, not-static temporary with the data
      nameStream << "__pc_init_";
      nameStream << std::string(vd->getName()) << "_";
      nameStream << declCount;
      const IdentifierInfo &varInfo =
          SemaRef.PP.getIdentifierTable().get(nameStream.str());
      VarDecl *vlocal = VarDecl::Create(
          Context, CurContext, vd->getBeginLoc(), vd->getEndLoc(), &varInfo,
          varType, Context.CreateTypeSourceInfo(varType), SC_Auto);
      vlocal->setInit(vd->getInit());
      addDecl(vlocal);
      // static local flag set to zero
      nameStream << "_f";
      const IdentifierInfo &flagInfo =
          PP.getIdentifierTable().get(nameStream.str());
      VarDecl *vflag = VarDecl::Create(
          Context, CurContext, vd->getBeginLoc(), vd->getEndLoc(), &flagInfo,
          flagType, Context.CreateTypeSourceInfo(flagType), SC_Static);
      vflag->setInit(IntegerLiteral::Create(Context, flagZero, flagType,
                                            vd->getBeginLoc()));
      addDecl(vflag);
      // remove the initializer from original decl (and const if any)
      varType.removeLocalConst();
      vd->setType(varType);
      vd->setInit(nullptr);
      // if (flag != 1) { flag = 1; memcpy(&dst, &src, sizeof(src)); }
      StmtResult ifsinit = createInitStructMemcpy(vd, vlocal, vflag, *this);
      if (ifsinit.isInvalid()) {
        return StmtError();
      }
      rparts.push_back(ifsinit.get());
    }
    declCount += 1;
  }
  return ActOnCompoundStmt(DS->getBeginLoc(), DS->getEndLoc(), rparts, false);
}
