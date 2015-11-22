{-# LANGUAGE RecursiveDo #-}

module Masque.Eval where

import Control.Lens (uses, ix, (^.))
import Control.Monad (forM, forM_, void)
import Control.Monad.Error.Class (catchError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Either (left, bracketEitherT)
import Data.Foldable (toList)
import Data.IORef (writeIORef)
import Data.Maybe (fromMaybe)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import qualified Data.Sequence as Seq
import Data.Unique (newUnique)

import Masque.AST (Expr(..), Patt(..),
                   Method(..), Matcher(..))
import Masque.Monte (Monte, Obj(..), Binding(..), Err(..), Env(..),
                     MessageHandler,
                     envStack, varBinding,
                     getBinding, defBinding, scoped, stashingScope, getName,
                     newEjector, catchEjector, withEjector, fire)
import Masque.Objects (call)

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
    call o' v as'
eval (DefExpr p ej expr) = do
    rvalue <- eval expr
    ej' <- case ej of
      Just ex -> eval ex
      _ -> return NullObj
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
    if b then scoped $ eval t
      else case f of
      (Just f') -> scoped $ eval $ f'
      Nothing -> return NullObj
eval (HideExpr n) = scoped $ eval n
eval (NounExpr name) = getName name
{- @@@@
eval (ObjectExpr _ p _ _ methods matchers) = mdo
    u <- liftIO newUnique
    let rv = undefined -- @@ UserObj u objName env methodMap matchers
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
-}
eval n = error $ "Couldn't evaluate node: " ++ show n


callUserObj :: Obj -> Env
               -> (M.Map String [Method]) -> [Matcher]
               -> MessageHandler
callUserObj o env methodMap matchers verb args =
    stashingScope (env NE.:| []) $ callMethod methods
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

resolve :: Obj -> Monte Obj
-- resolve (RefObj ref) = do
--     mobj <- liftIO . readIORef $ ref
--    maybe (left Unsettled) resolve mobj
resolve obj = return obj

unifyEject :: Obj -> Obj -> Patt -> Monte ()
unifyEject _  _ (BindingPatt _) = undefined
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
unify _  (BindingPatt _) = undefined
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
