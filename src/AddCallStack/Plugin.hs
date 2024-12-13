module AddCallStack.Plugin (plugin) where

import GHC.Plugins (CommandLineOption, Hsc, ModSummary, ParsedResult, Plugin (parsedResultAction), defaultPlugin)

plugin :: Plugin
plugin = defaultPlugin{parsedResultAction = commandLinePlugin}

commandLinePlugin :: [CommandLineOption] -> ModSummary -> ParsedResult -> Hsc ParsedResult
commandLinePlugin = undefined
