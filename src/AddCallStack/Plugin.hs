{-# LANGUAGE OverloadedRecordDot #-}

module AddCallStack.Plugin (plugin) where

import Control.Monad.IO.Class (liftIO)
import Data.Maybe (catMaybes)
import GHC (Anno, GRHSs, GenLocated (L), GhcPass, GhcPs, HsBind, HsBindLR (FunBind), HsContext, HsDecl (SigD, ValD), HsLocalBinds, HsLocalBindsLR (HsValBinds), HsModule (hsmodDecls), HsParsedModule (HsParsedModule, hpm_module), HsSigType (HsSig), HsType (HsQualTy, HsTyVar, hst_ctxt), HsValBindsLR (ValBinds), HsWildCardBndrs (hswc_body), IdP, LHsDecl, LHsExpr, LHsLocalBinds, LHsSigType, LHsSigWcType, LHsType, LIdP, LMatch, LSig, Located, MapXRec (mapXRec), Match, MatchGroup (MG), NoEpAnns (NoEpAnns), NoExtField (NoExtField), PromotionFlag (NotPromoted), Sig (TypeSig), UnXRec (unXRec), XRec, XSigD, fun_matches, getLoc, grhssLocalBinds, m_grhss, mg_alts, mkModuleName, noLoc, putMsgM, reLoc, sig_body, typecheckModule, unLoc, wrapXRec)
import GHC.Plugins (CommandLineOption, Hsc, ModSummary, ParsedResult (ParsedResult, parsedResultModule), Plugin (parsedResultAction), defaultPlugin, mkClsOcc, mkRdrQual, mkRdrUnqual, putMsg, putMsgS, typeSize)
import GHC.Tc.Errors.Types (HsTypeOrSigType (HsType))
import Language.Haskell.Syntax.Module.Name (ModuleName)

type Updator a s = (a -> a) -> s -> s

subLoc :: Located a -> a -> Located a
subLoc before x = L (getLoc before) x

updateLoc :: (a -> a) -> Located a -> Located a
updateLoc f before = subLoc before (f (unLoc before))

updateParsedResult :: Updator HsParsedModule ParsedResult
updateParsedResult f pr = pr{parsedResultModule = f pr.parsedResultModule}

updateParsedModule :: Updator (Located (HsModule GhcPs)) HsParsedModule
updateParsedModule f pm = pm{hpm_module = f pm.hpm_module}

updateLocHsModule :: Updator (HsModule GhcPs) (Located (HsModule GhcPs))
updateLocHsModule = updateLoc

updateHsModule :: Updator [LHsDecl GhcPs] (HsModule GhcPs)
updateHsModule f hsm = hsm{hsmodDecls = f hsm.hsmodDecls}

updateLhsModDecls :: Updator (LHsDecl GhcPs) [LHsDecl GhcPs]
updateLhsModDecls = map

updateLhsModDecl :: Updator (HsDecl GhcPs) (LHsDecl GhcPs)
updateLhsModDecl f lhsdecl = fmap f lhsdecl

-- f :: a->a = ... parses as a pattern binding,
-- f :: a->a; f = ... parses as a signature binding
updateHsDecl :: Updator (LHsSigWcType GhcPs) (HsDecl GhcPs)
updateHsDecl f (SigD xsigd sig) = SigD xsigd (updateSig f sig)
updateHsDecl f (ValD xvald bind) = ValD xvald (updateHsBind f bind)
updateHsDecl _ decl = decl

-- | Updates an ordinary type signature with a given function, ignoring other signatures
updateSig :: Updator (LHsSigWcType GhcPs) (Sig GhcPs)
updateSig f (TypeSig xtypsig lidps lhssigwctype) = TypeSig xtypsig lidps (f lhssigwctype)
updateSig _ sig = sig

updateLhsSigWcType :: Updator (LHsSigType GhcPs) (LHsSigWcType GhcPs)
updateLhsSigWcType f lhsSigWcType = lhsSigWcType{hswc_body = f lhsSigWcType.hswc_body}

updateLhsSigType :: Updator (HsSigType GhcPs) (LHsSigType GhcPs)
updateLhsSigType = fmap

updateHsSigType :: Updator (LHsType GhcPs) (HsSigType GhcPs)
updateHsSigType f sigtype@HsSig{} = sigtype{sig_body = f sigtype.sig_body}

updateLhsType :: Updator (HsType GhcPs) (LHsType GhcPs)
updateLhsType f lhsType = fmap f lhsType

-- TODO: Investigate updating other types
updateHsType :: HsType GhcPs -> HsType GhcPs
updateHsType typ@(HsQualTy xqual ctxt body)
  | containsHasCallStack (unLoc ctxt) = typ
  | otherwise = typ{hst_ctxt = fmap addHasCallStack ctxt}
updateHsType typ = typ

addHasCallStack :: HsContext GhcPs -> HsContext GhcPs
addHasCallStack hsctxt = mkHasCallStack : hsctxt

mkHasCallStack :: LHsType GhcPs
mkHasCallStack = reLoc (noLoc (HsTyVar [] NotPromoted lIdp))

renamedGhcStackImport :: ModuleName
renamedGhcStackImport = mkModuleName "AutoImported.GHC.Stack"

idp :: IdP GhcPs
idp = mkRdrQual renamedGhcStackImport (mkClsOcc "HasCallStack")

lIdp :: LIdP GhcPs
lIdp = reLoc (noLoc (idp))

-- | Updates function-like binding
updateHsBind :: Updator (LHsSigWcType GhcPs) (HsBind GhcPs)
updateHsBind f bind@FunBind{} = bind{fun_matches = updateMatchGroup f bind.fun_matches}
updateHsBind _ bind = bind

updateMatchGroup :: Updator (LHsSigWcType GhcPs) (MatchGroup GhcPs (LHsExpr GhcPs))
updateMatchGroup f mg = mg{mg_alts = updateLLMatch f (mg_alts mg)}

updateLLMatch :: Updator (LHsSigWcType GhcPs) (XRec GhcPs [LMatch GhcPs (LHsExpr GhcPs)])
updateLLMatch f lmatches = fmap (updateLMatches f) lmatches

updateLMatches :: Updator (LHsSigWcType GhcPs) [LMatch GhcPs (LHsExpr GhcPs)]
updateLMatches f lms = map (fmap (updateMatch f)) lms

updateMatch :: Updator (LHsSigWcType GhcPs) (Match GhcPs (LHsExpr GhcPs))
updateMatch f m = m{m_grhss = updateGrhss f (m_grhss m)}

updateGrhss :: Updator (LHsSigWcType GhcPs) (GRHSs GhcPs (LHsExpr GhcPs))
updateGrhss f grhss = grhss{grhssLocalBinds = updateLocalBinds f (grhssLocalBinds grhss)}

updateLocalBinds :: Updator (LHsSigWcType GhcPs) (HsLocalBinds GhcPs)
updateLocalBinds f (HsValBinds xHsValBinds hsValBindsLR) = HsValBinds xHsValBinds (updateHsValBindsLR f hsValBindsLR)
updateLocalBinds _ binds = binds

updateHsValBindsLR :: Updator (LHsSigWcType GhcPs) (HsValBindsLR GhcPs GhcPs)
updateHsValBindsLR f (ValBinds xValBinds lHsBindsLR lSigs) = ValBinds xValBinds lHsBindsLR (updateLSigs f lSigs)
updateHsValBindsLR _ binds = binds

updateLSigs :: Updator (LHsSigWcType GhcPs) [LSig GhcPs]
updateLSigs f = map (updateLSig f)

updateLSig :: Updator (LHsSigWcType GhcPs) (LSig GhcPs)
updateLSig f = fmap (updateSig f)

hasCallStack :: HsType GhcPs -> Bool
hasCallStack (HsTyVar _ _ lidp) = unLoc lidp == (mkRdrUnqual $ mkClsOcc "HasCallStack")
hasCallStack _ = False

containsHasCallStack :: HsContext GhcPs -> Bool
containsHasCallStack = any (hasCallStack . unLoc)

plugin :: Plugin
plugin = defaultPlugin{parsedResultAction = commandLinePlugin}

isSignature :: HsDecl p -> Bool
isSignature (SigD _ _) = True
isSignature _ = False

extractDeclFromLhs :: LHsDecl (GhcPass p) -> HsDecl (GhcPass p)
extractDeclFromLhs lhsdecl = unLoc lhsdecl

extractSig :: HsDecl p -> Maybe (Sig p)
extractSig (SigD _ sig) = Just sig
extractSig _ = Nothing

extractTypeSig :: Sig p -> Maybe [LIdP p]
extractTypeSig (TypeSig _ lidps _) = Just lidps
extractTypeSig _ = Nothing

addAllHasCallStack :: ParsedResult -> ParsedResult
addAllHasCallStack = updateParsedResult (updateParsedModule (updateLocHsModule (updateHsModule (updateLhsModDecls (updateLhsModDecl (updateHsDecl (updateLhsSigWcType (updateLhsSigType (updateHsSigType (updateLhsType updateHsType))))))))))

-- | Adds the HasCallStack to the parsed function bindings
commandLinePlugin :: [CommandLineOption] -> ModSummary -> ParsedResult -> Hsc ParsedResult
commandLinePlugin _ modSummary parsedResult = return (addAllHasCallStack parsedResult)
