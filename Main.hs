module Main where

import Control.Lens
import Control.Monad (forM)
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

import Masque.Monte (Monte, runMonte, MonteState(..), Err,
                     Env(..), unEnv, envStack,
                     Obj)
import Masque.AST (Expr)
import Masque.Deserialize (monteMagic, emptyContext, getExpr)
import Masque.Eval (mapToScope, coreScope, finalize, eval)
import Masque.Parser (parseFile, parseSource)
import Masque.Desugar (desugar)

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

runAST :: Env -> NonEmpty Env -> Expr -> IO (Either Err Obj, MonteState, ())
runAST prelude envs expr = do
    runMonte (eval expr) prelude envs

runSource :: String -> IO ()
runSource s = do
  _ <- forM (map desugar $ parseSource s) runExpr
  return ()

runExpr :: Expr -> IO ()
runExpr e = let coreEnv = finalize coreScope :| [] in do
  answer <- runAST (Env M.empty) coreEnv e
  case answer of
    ((Right obj), _, _) -> print obj
    _ -> print answer

runFile :: Env -> NonEmpty Env -> FilePath -> IO (Either Err Obj, MonteState, ())
runFile prelude envs path = do
    bs <- BSL.readFile path
    if monteMagic `BSL.isPrefixOf` bs then do
      node <- loadNode (BSL.drop (BSL.length monteMagic) bs)
      runAST prelude envs node
    else fail "bad magic"

main :: IO ()
main = withSocketsDo $ do
  [fileName] <- getArgs
  case fileName of
    mt | mt `endswith` ".mt" -> do
           e <- parseFile mt
           print e
    mast | mast `endswith` ".mast" -> loadAndRunFile mast
    _ -> error "not .mt nor .mast"
  where
    endswith :: String -> String -> Bool
    endswith haystack needle = (reverse haystack) `startswith` (reverse needle)
    startswith [] [] = True
    startswith (c:h) (c':n) | c == c' = startswith h n
    startswith _ _ = False

loadAndRunFile :: String -> IO ()
loadAndRunFile fileName = do
    let coreEnv = finalize coreScope :| []
    (preludeOrErr, _, _) <- runFile (Env M.empty) coreEnv "prelude.mast"
    prelude <- case preludeOrErr of
        Right p  -> return p
        Left err -> print err >> exitWith (ExitFailure 1)
    result <- runFile (mapToScope prelude) coreEnv fileName
    print $ result ^. _1
