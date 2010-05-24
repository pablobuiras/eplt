module Configure.Options where

import System.Console.GetOpt
import Control.Monad
import Laws
import Data.IORef
import Configure.Load
import Commands
import System.Exit
import System.Environment
import Formula
import Version

data Options = Options { optLaws :: IO LawBank,
                         optPostQed :: Formula -> Command,
                         optGustavoMode :: Bool }

startOptions = Options { optLaws = loadLawsFrom "default.laws",
                         optPostQed = const Nop,
                         optGustavoMode = False }

options :: [OptDescr (Options -> IO Options)]
options =
    [ Option "l" ["load"]
             (ReqArg
              (\arg opt -> return opt { optLaws = loadLawsFrom arg })
              "FILE")
             "Specify laws file.",

      Option "e" ["extend-lawbank"]
             (NoArg
              (\opt -> return opt { optPostQed = \f -> case f of
                                                         lhs :== rhs -> AddLaw (lhs,rhs)
                                                         _ -> AddLaw (f,FTrue) }))
             "Enable automatic adding of theorems to the law bank.",

      Option "g" ["gustavo-mode"]
             (NoArg
              (\opt -> return opt { optGustavoMode = True }))
             "Enable Gustavo's favourite mode.",

      Option "v" ["version"]
             (NoArg
              (\_ -> do putStrLn version
                        exitWith ExitSuccess))
             "Print version information.",

      Option "h" ["help"]
             (NoArg
              (\_ -> do prg <-getProgName
                        putStrLn (usageInfo prg options)
                        exitWith ExitSuccess))
             "Print this message."
    ]