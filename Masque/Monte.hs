{-# LANGUAGE TemplateHaskell #-}

module Masque.Monte where

import Control.Applicative((<|>))
import Control.Lens(Lens', lens, makeLenses, makePrisms,
                    (%=), (.=), (?=),
                    use, at, view, preuse, traverse, ix, preview)
import Control.Monad(liftM)
import Control.Monad.Error.Class(catchError)
import Control.Monad.Error.Lens(catching)
import Control.Monad.IO.Class(liftIO)
import Control.Monad.Trans.Either(EitherT, left, bracketEitherT, runEitherT)
import Control.Monad.Trans.RWS(RWST, runRWST)
import Data.IORef(IORef, newIORef, readIORef)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import Data.Unique as UUU

-- TODO:
-- type NamedArg = (String, Obj)
-- type Message = (String, [Obj], [NamedArg])

data Err = Unknown
         | Unsettled
         | BadName String (S.Set String)
         | BadWrite String
         | Ejecting Unique Obj
           -- TODO: refactor to use Message
         | Refused Obj String [Obj] (S.Set String)
    deriving (Show)

             -- TODO: refactor to use Message
data Vat = Vat { _unVat :: [(Obj, String, [Obj])] }
    deriving (Show)

data MonteState = MS { _envStack :: NonEmpty Env
                     , _vat :: Vat }

type Monte = EitherT Err (RWST Env () MonteState IO)

data Obj = NullObj
           -- Data
         | BoolObj Bool
         | CharObj Char
         | DoubleObj Double
         | IntObj Integer
         | StrObj String
         | ConstListObj (Seq.Seq Obj)
         | ConstMapObj [(Obj, Obj)]
           -- Control, Reference
         | EjectorObj Unique
         | BindingObj -- TODO
           -- Runtime and User Objects
         | OpaqueObj Unique String Receiver
           deriving (Show)

data Receiver = Receiver MessageHandler
type MessageHandler = String -- verb
                      -> [Obj] -- args
                      -- TODO -> [(String, Obj)] -- named args
                      -> Monte Obj

instance Show Receiver where
  show _ = "<methods, matchers>"

instance Show Unique where
    show _ = "<unique>"

data Binding = DefBind Obj
             | VarBind { _bSlot, _bGuard :: IORef Obj }

instance Show Binding where
    show (DefBind o) = "DefBind " ++ show o
    show (VarBind _ _) = "VarBind ..."

newtype Env = Env { _unEnv :: M.Map String Binding }
    deriving (Show)

makeLenses ''Binding
makeLenses ''Env
makeLenses ''MonteState
makePrisms ''Err

runMonte :: Monte a -> Env -> NonEmpty Env -> IO (Either Err a, MonteState, ())
runMonte action prelude envs = runRWST (runEitherT action) r s
    where
    r = prelude
    s = MS envs (Vat [])

-- | Lenses on other data types
-- These are destined for various upstreams at some point.

first :: Lens' (NonEmpty a) a
first = lens (\(a :| _) -> a) (\(_ :| as) -> (:| as))

final :: Lens' (NonEmpty a) a
final = lens g s
    where
    g (a :| []) = a
    g (_ :| as) = last as
    s (a :| []) b = a :| [b]
    s (a :| as) b = a :| init as ++ [b]

-- | Evaluation helpers

scoped :: Monte a -> Monte a
scoped action =
    bracketEitherT push pop (const action)
    where
    push = envStack %= (Env M.empty NE.<|)
    -- Only works as long as the environment stack isn't overpopped during the
    -- scoped action. Shouldn't happen.
    pop _ = envStack %= (\(_ :| (a:as)) -> a :| as)

stashingScope :: NonEmpty Env -> Monte a -> Monte a
stashingScope es action = bracketEitherT open (envStack .=) (const action)
    where
    open = do
        stashed <- use envStack
        envStack .= es
        return stashed

varBinding :: String -> Obj -> Monte ()
varBinding name obj = do
    slotRef <- liftIO $ newIORef obj
    -- XXX use an actual guard next time!
    guardRef <- liftIO $ newIORef NullObj
    envStack . first . unEnv . at name ?= VarBind slotRef guardRef

defBinding :: String -> Obj -> Monte ()
defBinding name obj = envStack . first . unEnv . at name ?= DefBind obj

newEjector :: Monte Obj
newEjector = do
    u <- liftIO newUnique
    return $ EjectorObj u

fire :: Obj -> Obj -> Monte ()
fire (EjectorObj u) payload = left $ Ejecting u payload
fire _ _ = left Unknown

catchEjector :: Unique -> Monte Obj -> Monte Obj
catchEjector u action = catchError action $ \err ->
    case err of
        Ejecting u' obj | u == u' -> return obj
        _                         -> left err

withEjector :: (Obj -> Monte Obj) -> Monte Obj
withEjector action = do
    ej@(EjectorObj u) <- newEjector
    catchEjector u $ action ej

getNames :: Monte (S.Set String)
getNames = do
    env <- view id
    envs <- use envStack
    return $ S.unions (map (M.keysSet . _unEnv) (env : NE.toList envs))

 -- TODO: shouldn't we be able to move this to Objects.hs?
builtins :: [String]
builtins =
    [ "__booleanFlow"
    , "__equalizer"
    , "__loop"
    , "boolean"
    , "connectTo"
    , "stdout"
    , "throw"
    , "traceln"
    ]


getBinding :: String -> Monte Binding
getBinding name = do
    names <- getNames
    binding <- preuse $ envStack . traverse . unEnv . ix name
    let availableNames = S.unions [S.fromList builtins, names]
    maybe (left $ BadName name availableNames) return binding

bindToObj :: Binding -> Monte Obj
bindToObj (DefBind o) = return o
bindToObj (VarBind ref _) = liftIO $ readIORef ref

getName :: String -> Monte Obj
getName name =
  -- TODO: if name `elem` builtins then return (BuiltinObj name) else
  do
    userBinding <- catching _BadName (liftM Just (getBinding name)) (\_ -> return Nothing)
    preludeBinding <- preview $ unEnv . ix name
    let binding = userBinding <|> preludeBinding
    case binding of
        Just b  -> bindToObj b
        Nothing -> left $ BadName name S.empty
