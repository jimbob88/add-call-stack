module AddCallStack.Plugin (plugin) where

import Control.Monad.IO.Class (liftIO)
import Data.Maybe (catMaybes)
import GHC (GhcPass, HsDecl (SigD), HsModule (hsmodDecls), HsParsedModule (hpm_module), LHsDecl, LIdP, Sig (TypeSig), UnXRec (unXRec), XSigD, putMsgM, typecheckModule, unLoc)
import GHC.Plugins (CommandLineOption, Hsc, ModSummary, ParsedResult (parsedResultModule), Plugin (parsedResultAction), defaultPlugin, putMsg, putMsgS, typeSize)

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

-- | Adds the HasCallStack to the parsed function bindings
commandLinePlugin :: [CommandLineOption] -> ModSummary -> ParsedResult -> Hsc ParsedResult
commandLinePlugin _ modSummary parsedResult = do
  let mdl = parsedResultModule parsedResult
  let hpm = hpm_module mdl
  let hs_mdl = unLoc hpm
  let dcls = hsmodDecls hs_mdl
  let hsdcls = map extractDeclFromLhs dcls
  let sigdcls = filter isSignature hsdcls
  let sigs = catMaybes (map extractSig sigdcls)
  let typeSigs = catMaybes (map extractTypeSig sigs)

  liftIO $ do
    putStrLn "Declarations:"
    print (length dcls)
    putStrLn "Signature Declarations:"
    print (length sigdcls)
    putStrLn "Type Signatures:"
    print (length typeSigs)

  return parsedResult
