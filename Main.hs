module Main where

import Control.Lens
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State
import Data.Binary.Get (runGet)
import qualified Data.ByteString.Lazy as BSL
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map as M
import Network.Socket (withSocketsDo)
import System.Environment (getArgs)
import System.Exit (ExitCode(ExitFailure), exitWith)
import Text.PrettyPrint.GenericPretty (Out, pp)

import Masque.Monte
import Masque.AST
import Masque.Deserialize
import Masque.Eval
import Masque.Lexer
import Masque.Parser

-- | Debugging

debug :: Out a => a -> Monte ()
debug = liftIO . pp

showEnv :: String -> Monte ()
showEnv s = do
    envs <- use $ envStack . each . unEnv . to M.keys
    debug (s, "Current environment names:", envs)

-- | Script evaluation

loadNode :: BSL.ByteString -> IO Expr
loadNode bs = do
  let (expr, _) = runGet (runStateT getExpr emptyContext) bs
  putStrLn "Loaded and optimized AST:"
  pp expr
  return expr

optimize :: Expr -> Expr
optimize e = e  -- TODO. see Optimize.hs

runAST :: Env -> NonEmpty Env -> BSL.ByteString -> IO (Either Err Obj, MonteState, ())
runAST prelude envs bs = do
    node <- loadNode bs
    runMonte (eval node) prelude envs

runFile :: Env -> NonEmpty Env -> FilePath -> IO (Either Err Obj, MonteState, ())
runFile prelude envs path = do
    bs <- BSL.readFile path
    if monteMagic `BSL.isPrefixOf` bs
      then runAST prelude envs (BSL.drop (BSL.length monteMagic) bs)
      else fail "bad magic"


main :: IO ()
main = do
  [fileName] <- getArgs
  parseFile fileName

main2 :: IO ()
main2 = withSocketsDo $ do
    [fileName] <- getArgs
    loadAndRunFile fileName

loadAndRunFile :: String -> IO ()
loadAndRunFile fileName = do
    let coreEnv = finalize coreScope :| []
    (preludeOrErr, _, _) <- runFile (Env M.empty) coreEnv "prelude.mast"
    prelude <- case preludeOrErr of
        Right p  -> return p
        Left err -> print err >> exitWith (ExitFailure 1)
    result <- runFile (mapToScope prelude) coreEnv fileName
    print $ result ^. _1
