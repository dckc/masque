module Main where

import Control.Lens
import Control.Monad.IO.Class (liftIO)
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
import Masque.ASTIO
import Masque.Eval

-- | Debugging

debug :: Out a => a -> Monte ()
debug = liftIO . pp

showEnv :: String -> Monte ()
showEnv s = do
    envs <- use $ envStack . each . unEnv . to M.keys
    debug (s, "Current environment names:", envs)

-- | Script evaluation

loadNode :: BSL.ByteString -> IO Expr
loadNode bs = let node = optimize $ runGet getNode bs in do
    putStrLn "Loaded and optimized AST:"
    pp node
    return node

optimize :: Expr -> Expr
optimize e = e  -- TODO. see Optimize.hs

runAST :: Env -> NonEmpty Env -> BSL.ByteString -> IO (Either Err Obj, MonteState, ())
runAST prelude envs bs = do
    node <- loadNode bs
    runMonte (eval node) prelude envs

runFile :: Env -> NonEmpty Env -> FilePath -> IO (Either Err Obj, MonteState, ())
runFile prelude envs path = do
    bs <- BSL.readFile path
    runAST prelude envs bs

main :: IO ()
main = withSocketsDo $ do
    let coreEnv = finalize coreScope :| []
    (preludeOrErr, _, _) <- runFile (Env M.empty) coreEnv "prelude.mast"
    prelude <- case preludeOrErr of
        Right p  -> return p
        Left err -> print err >> exitWith (ExitFailure 1)
    [fileName] <- getArgs
    result <- runFile (mapToScope prelude) coreEnv fileName
    print $ result ^. _1
