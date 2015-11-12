{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.Error.Class
import Control.Monad.Error.Lens
import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import Control.Monad.Trans.RWS
import Data.Binary.Get
import Data.Bits
import qualified Data.ByteString.Lazy as BSL
import Data.Data.Lens
import Data.Foldable (toList)
import Data.IORef
import Data.List
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import Data.Unique
import Network.Socket
import System.Environment
import System.Exit
import System.IO
import Text.PrettyPrint.GenericPretty

import Masque.AST
import Masque.ASTIO
import Masque.Objects

instance Show Unique where
    show _ = "<unique>"

data Err = Unknown
         | Unsettled
         | BadName String (S.Set String)
         | BadWrite String
         | Ejecting Unique Obj
         | Refused Obj String [Obj] (S.Set String)
    deriving (Show)

data Vat = Vat { _unVat :: [(Obj, String, [Obj])] }
    deriving (Show)

data MonteState = MS { _envStack :: NonEmpty Env
                     , _vat :: Vat }

makeLenses ''Binding
makeLenses ''Env
makeLenses ''MonteState
makePrisms ''Err

type Monte = EitherT Err (RWST Env () MonteState IO)

-- | Lenses

_DoubleObj :: Prism' Obj Double
_DoubleObj = prism' DoubleObj $ \o -> case o of
    DoubleObj d -> Just d
    IntObj i    -> Just $ fromIntegral i
    _           -> Nothing

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

-- | Name analysis

namesBound :: Patt -> [String]
namesBound (BindPatt n) = [n]
namesBound (FinalPatt n _) = [n]
namesBound (IgnorePatt _) = []
namesBound (ListPatt ps) = concatMap namesBound ps
namesBound (VarPatt n _) = [n]
namesBound (ViaPatt _ p) = namesBound p

namesUsed :: Expr -> [String]
namesUsed expr = mapMaybe f $ universe expr
    where f (NounExpr n)     = Just n
          f (AssignExpr n _) = Just n
          f _            = Nothing

-- XXX this is such a horrible hack
nameUsed :: Expr -> String -> Bool
nameUsed _ "_flexList" = True
nameUsed _ "_flexMap" = True
nameUsed _ "_listIterator" = True
nameUsed node name = name `elem` namesUsed node

patternUnused :: Patt -> Expr -> Bool
patternUnused pattern expr =
    all (not . nameUsed expr) (namesBound pattern)

optimize :: Expr -> Expr
optimize e = e

-- | Debugging

debug :: Out a => a -> Monte ()
debug = liftIO . pp

showEnv :: String -> Monte ()
showEnv s = do
    envs <- use $ envStack . each . unEnv . to M.keys
    debug (s, "Current environment names:", envs)

-- | Object equality

sameEver :: Obj -> Obj -> Bool
sameEver NullObj NullObj = True
sameEver (BoolObj x) (BoolObj y) = x == y
sameEver (IntObj x) (IntObj y) = x == y
sameEver (UserObj x _ _ _ _) (UserObj y _ _ _ _) = x == y
sameEver _ _ = False

-- | Object message passing

cmp :: Ord a => a -> a -> Obj
cmp l r = IntObj c
    where c = subtract 1 . fromIntegral . fromEnum $ compare l r

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

callBool :: Bool -> String -> [Obj] -> Maybe Obj
callBool b "not" [] = Just . BoolObj $ not b
callBool b "pick" [l, r] = Just $ if b then l else r
callBool _ _ _ = Nothing

callDouble :: Double -> String -> [Obj] -> Maybe Obj
callDouble d "abs" [] = Just . DoubleObj $ abs d
callDouble d "add" [preview _DoubleObj -> Just d'] = Just . DoubleObj $ d + d'
callDouble d "multiply" [preview _DoubleObj -> Just d'] = Just . DoubleObj $ d * d'
callDouble d "negate" [] = Just . DoubleObj $ negate d
callDouble d "op__cmp" [preview _DoubleObj -> Just d'] = Just $ cmp d d'
callDouble d "sqrt" [] = Just . DoubleObj $ sqrt d
callDouble d "subtract" [preview _DoubleObj -> Just d'] = Just . DoubleObj $ d - d'
callDouble _ _ _ = Nothing

callInt :: Integer -> String -> [Obj] -> Maybe Obj
callInt i "op__cmp" [DoubleObj d] = Just $ cmp (realToFrac i) d
callInt i "op__cmp" [IntObj j] = Just $ cmp i j
callInt i "aboveZero" [] = Just . BoolObj $ i > 0
callInt i "add" [IntObj j] = Just . IntObj $ i + j
callInt i "and" [IntObj j] = Just . IntObj $ i .&. j
callInt i "atLeastZero" [] = Just . BoolObj $ i >= 0
callInt i "atMostZero" [] = Just . BoolObj $ i <= 0
callInt i "approxDivide" [DoubleObj d] = Just . DoubleObj $ realToFrac i / d
callInt i "belowZero" [] = Just . BoolObj $ i < 0
callInt i "floorDivide" [IntObj j] = Just . IntObj $ i `div` j
callInt i "isZero" [] = Just . BoolObj $ i == 0
callInt i "multiply" [DoubleObj d] = Just . DoubleObj $ d * realToFrac i
callInt i "multiply" [IntObj j] = Just . IntObj $ i * j
callInt i "pow" [IntObj j] = Just . IntObj $ i ^ j
callInt i "subtract" [IntObj j] = Just . IntObj $ i - j
callInt _ _ _ = Nothing

callStr :: String -> String -> [Obj] -> Maybe Obj
callStr s "add" [CharObj c] = Just . StrObj $ s ++ [c]
callStr s "add" [StrObj t] = Just . StrObj $ s ++ t
callStr s "get" [IntObj i] | i' < length s = Just . CharObj $ s !! i'
    where i' = fromIntegral i
callStr s "multiply" [IntObj i] =
    Just . StrObj . concat $ replicate (fromIntegral i) s
callStr s "size" [] = Just . IntObj $ genericLength s
callStr _ _ _ = Nothing

call :: Obj -> String -> [Obj] -> Monte Obj
call o@(BoolObj b) verb args =
    maybe (left (Refused o verb args S.empty)) return $ callBool b verb args
call o@(DoubleObj d) verb args =
    maybe (left (Refused o verb args S.empty)) return $ callDouble d verb args
call o@(IntObj i) verb args =
    maybe (left (Refused o verb args S.empty)) return $ callInt i verb args
call o@(StrObj s) verb args =
    maybe (left (Refused o verb args S.empty)) return $ callStr s verb args

call (EjectorObj u) "run" [obj] = left $ Ejecting u obj

call (RefObj ref) verb args = do
    target <- liftIO $ readIORef ref
    maybe (left Unsettled) (\o -> call o verb args) target

call (ResObj ref) "resolve" [obj] = do
    target <- liftIO $ readIORef ref
    when (isJust target) $ left Unknown
    liftIO . writeIORef ref $ Just obj
    return NullObj

call f@(FountObj _ ref) "flowTo" [drain] = do
    -- XXX what to do if the fount already has a drain?
    liftIO $ writeIORef ref drain
    case drain of
        NullObj -> return NullObj
        _       -> call drain "flowingFrom" [f]

call d@(DrainObj _) "flowingFrom" [_] = return d
call (DrainObj h) "receive" [StrObj s] = do
    -- XXX blocks?
    liftIO $ hPutStr h s
    return NullObj

call (BuiltinObj "__booleanFlow") "failureList" [IntObj 0] =
    return . ConstListObj . Seq.singleton $ BoolObj False
call (BuiltinObj "__equalizer") "sameEver" [x, y] =
    return . BoolObj $ sameEver x y
call (BuiltinObj "__loop") "run" [iterable, consumer] = do
    iterator <- call iterable "_makeIterator" []
    -- Note that `forever` does not loop endlessly in EitherT, but only until
    -- the state of the EitherT is Left. As a result, the Left state caused by
    -- a firing ejector will break free of `forever` and return control to the
    -- surrounding combinator. `withEjector` will catch the ejector that was
    -- passed into the loop, which is conveniently the intended way to cleanly
    -- exit the loop.
    -- Also note `void`; we deliberately discard the result here, since it is
    -- usually merely a string notifying us that the iterator is exhausted,
    -- and we will return null anyway. ~ C.
    void . withEjector $ \ej -> forever $ do
        ConstListObj objs <- call iterator "next" [ej]
        let (key:value:_) = toList objs
        call consumer "run" [key, value]
    return NullObj
call (BuiltinObj "boolean") "coerce" [obj@(BoolObj _), _] = return obj
call (BuiltinObj "boolean") "coerce" [obj, ej] = fire ej obj >> return NullObj
call (BuiltinObj "connectTo") "run" [StrObj host, IntObj port] = do
    -- XXX needs to happen after the turn is finished
    -- XXX blocks
    addrInfo <- liftIO $ getAddrInfo Nothing (Just host) Nothing
    case addrInfo ^? _head . to addrAddress of
        Just (SockAddrInet _ ip) -> liftIO $ do
            s <- socket AF_INET Stream defaultProtocol
            -- XXX blocks
            connect s $ SockAddrInet (fromIntegral port) ip
            h <- socketToHandle s ReadWriteMode
            ref <- newIORef NullObj
            return . ConstListObj . Seq.fromList $ [FountObj h ref, DrainObj h]
        _            -> left Unknown
call (BuiltinObj "stdout") "print" [StrObj s] = do
    liftIO $ putStr s
    return NullObj
call (BuiltinObj "throw") "eject" [ej, payload] = do
    fire ej payload
    return NullObj
call (BuiltinObj "traceln") "run" args = do
    liftIO $ print args
    return NullObj

call clo@(ConstListObj _) "_makeIterator" [] = do
    listIterator <- getName "_listIterator"
    call listIterator "run" [clo]
call (ConstListObj objs) "asMap" [] = let ints = map IntObj [0..] in
    return . ConstMapObj $ zip ints (toList objs)
call clo@(ConstListObj _) "diverge" [] = do
    flexList <- getName "_flexList"
    call flexList "run" [clo]
call (ConstListObj objs) "get" [IntObj i]
    | i' < Seq.length objs = return $ Seq.index objs i'
    where i' = fromIntegral i
call (ConstListObj objs) "multiply" [IntObj i] =
    return . ConstListObj . join $ Seq.replicate i' objs
    where i' = fromIntegral i
call (ConstListObj objs) "size" [] =
    return . IntObj . fromIntegral $ Seq.length objs
call (ConstListObj objs) "with" [obj] = return . ConstListObj $ objs |> obj

call (ConstMapObj pairs) "with" [k, v] = return . ConstMapObj $ pairs ++ [(k, v)]

call o@(UserObj _ _ env methodMap matchers) verb args =
    stashingScope (env :| []) $ callMethod methods
    where
    methods = methodMap ^. ix verb

    callMethod ((Method _ _ p todo1 n todo2):ms) = scoped $ do
        success <- unify (ConstListObj $ Seq.fromList args) (ListPatt p)
        if success then eval n else callMethod ms
    callMethod [] = callMatcher matchers

    -- XXX This function is kind of a mess; bracket?
    callMatcher ((Matcher p n):ms) = flip catchError (\_ -> callMatcher ms) $ scoped $ do
        void $ withEjector $ \ej -> do
            unifyEject (ConstListObj $ Seq.fromList [StrObj verb, ConstListObj $ Seq.fromList args]) ej p
            return NullObj
        eval n
    callMatcher [] = left $ Refused o verb args (M.keysSet methodMap)

call o v as = left $ Refused o v as S.empty

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
getName name = if name `elem` builtins then return (BuiltinObj name) else do
    userBinding <- catching _BadName (liftM Just (getBinding name)) (\_ -> return Nothing)
    preludeBinding <- preview $ unEnv . ix name
    let binding = userBinding <|> preludeBinding
    case binding of
        Just b  -> bindToObj b
        Nothing -> left $ BadName name S.empty

resolve :: Obj -> Monte Obj
resolve (RefObj ref) = do
    mobj <- liftIO . readIORef $ ref
    maybe (left Unsettled) resolve mobj
resolve obj = return obj

-- | Unification and evaluation

unifyEject :: Obj -> Obj -> Patt -> Monte ()
unifyEject _  _ (BindPatt _) = undefined
unifyEject obj _ (FinalPatt n Nothing) = defBinding n obj
unifyEject obj ej (FinalPatt n (Just g)) = do
    g' <- eval g
    obj' <- call g' "coerce" [obj, ej]
    defBinding n obj'
unifyEject _ _ (IgnorePatt Nothing) = return ()
unifyEject obj ej (IgnorePatt (Just g)) = do
    g' <- eval g
    void $ call g' "coerce" [obj, ej]
unifyEject (ConstListObj os) ej (ListPatt ps)
    | Seq.length os == length ps = forM_ (zip os' ps) $ \(o, p) -> unifyEject o ej p
    where os' = toList os
unifyEject _ ej (ListPatt _) = fire ej NullObj
-- XXX need to generate slots here
unifyEject obj _ (VarPatt n _) = varBinding n obj
unifyEject obj ej (ViaPatt expr p) = do
    examiner <- eval expr
    examined <- call examiner "run" [obj, ej]
    unifyEject examined ej p

unify :: Obj -> Patt -> Monte Bool
unify _  (BindPatt _) = undefined
unify obj (FinalPatt n Nothing) = defBinding n obj >> return True
unify obj (FinalPatt n (Just g)) = do
    g' <- eval g
    obj' <- call g' "coerce" [obj, NullObj]
    defBinding n obj'
    return True
unify _ (IgnorePatt Nothing) = return True
unify obj (IgnorePatt (Just g)) = do
    g' <- eval g
    void $ call g' "coerce" [obj, NullObj]
    return True
unify (ConstListObj os) (ListPatt ps)
    | Seq.length os == length ps = do
        unified <- forM (zip os' ps) $ uncurry unify
        return $ and unified
    where os' = toList os
unify _ (ListPatt _) = return False
unify obj (VarPatt n _) = varBinding n obj >> return True
unify obj (ViaPatt expr p) = do
    examiner <- eval expr
    examined <- call examiner "run" [obj, NullObj]
    unify examined p

eval :: Expr -> Monte Obj
eval (CharExpr c) = return $ CharObj c
eval (DoubleExpr d) = return $ DoubleObj d
eval (IntExpr i) = return $ IntObj i
eval (StrExpr s) = return $ StrObj s
eval (AssignExpr name node) = do
    obj <- eval node
    binding <- getBinding name
    case binding of
        DefBind _ -> left $ BadWrite name
        -- XXX invoke guard here
        VarBind slotRef _ -> do
            liftIO $ writeIORef slotRef obj
            return obj
eval (BindingExpr _) = return BindingObj
eval (CallExpr o v as nas) = do
    o' <- eval o
    as' <- mapM eval as
    call o' v $ toList as'
eval (DefExpr p ej expr) = do
    rvalue <- eval expr
    ej' <- eval ej
    unifyEject rvalue ej' p
    return rvalue
eval (EscapeExpr p n _ _) = scoped $ do
    ej@(EjectorObj u) <- newEjector
    success <- unify ej p
    if success then catchEjector u $ eval n else left Unknown
eval (FinallyExpr node atLast) = bracketEitherT before after return
    where
    before = scoped $ eval node
    after obj = scoped $ eval atLast >> return obj
eval (IfExpr i t f) = do
    test <- eval i
    BoolObj b <- resolve test
    scoped $ eval $ if b then t else f
eval (HideExpr n) = scoped $ eval n
eval (NounExpr name) = getName name
eval (ObjectExpr _ p _ _ methods matchers) = mdo
    u <- liftIO newUnique
    let rv = UserObj u objName env methodMap matchers
    success <- unify rv p
    env <- uses envStack $ \es -> Env (M.unions (map _unEnv (NE.toList es)))
    if success then return rv else left Unknown
    where
    methodMap = M.fromListWith (++) methodList
    methodList = [(verb, [m]) | m@(Method _ verb _ _ _ _) <- methods ]
    objName = case p of
        FinalPatt name _ -> name
        _                -> "_"
eval (SequenceExpr ns) = do
    os <- mapM eval ns
    return $ if null os then NullObj else last os
eval (TryExpr n p h) = scoped $ catchError (eval n) $ \_ -> do
    success <- unify NullObj p
    if success then eval h else left Unknown
eval n = error $ "Couldn't evaluate node: " ++ show n

-- | Scope creation

coreScope :: M.Map String Obj
coreScope = M.fromList
    [ ("null", NullObj)
    , ("false", BoolObj False)
    , ("true", BoolObj True)
    ]

finalize :: M.Map String Obj -> Env
finalize scope = Env $ M.map DefBind scope

mapToScope :: Obj -> Env
mapToScope (ConstMapObj pairs) =
    Env $ M.fromList [(k, DefBind v) | (StrObj k, v) <- pairs]
mapToScope _ = error "mapToScope was misused"

-- | Script evaluation

loadNode :: BSL.ByteString -> IO Expr
loadNode bs = let node = optimize $ runGet getNode bs in do
    putStrLn "Loaded and optimized AST:"
    pp node
    return node

runMonte :: Monte a -> Env -> NonEmpty Env -> IO (Either Err a, MonteState, ())
runMonte action prelude envs = runRWST (runEitherT action) r s
    where
    r = prelude
    s = MS envs (Vat [])

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
