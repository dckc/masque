{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Masque.Objects where

import Control.Lens (Prism',
                     prism', preview, to, _head,
                     (|>), (^?))
import Control.Monad (join, void, forever)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Either (left)
import Data.Bits ( (.&.) )
import Data.Foldable (toList)
import Data.IORef as R
import Data.List
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import Network.Socket (getAddrInfo, addrAddress, socket,
                       defaultProtocol, connect, socketToHandle,
                       SockAddr(SockAddrInet), SocketType(Stream),
                       Family(AF_INET))
import System.IO (Handle, hPutStr, IOMode(ReadWriteMode))

import Masque.Monte(Monte, Obj(..), Err(..), MessageHandler,
                    getName, withEjector, fire)

-- | Lenses

_DoubleObj :: Prism' Obj Double
_DoubleObj = prism' DoubleObj $ \o -> case o of
    DoubleObj d -> Just d
    IntObj i    -> Just $ fromIntegral i
    _           -> Nothing

-- | Object equality

sameEver :: Obj -> Obj -> Bool
sameEver NullObj NullObj = True
sameEver (BoolObj x) (BoolObj y) = x == y
sameEver (IntObj x) (IntObj y) = x == y
sameEver (StrObj x) (StrObj y) = x == y
sameEver (DoubleObj x) (DoubleObj y) = x == y
sameEver (OpaqueObj x _ _) (OpaqueObj y _ _ ) = x == y
sameEver _ _ = False

-- | Object message passing

cmp :: Ord a => a -> a -> Obj
cmp l r = IntObj c
    where c = subtract 1 . fromIntegral . fromEnum $ compare l r

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


refused :: Obj -> String -> [Obj] -> Monte Obj
refused o verb args = (left (Refused o verb args S.empty))

callFountObj :: R.IORef Obj -> Obj -> MessageHandler
callFountObj ref f "flowTo" [drain] = do
    -- XXX what to do if the fount already has a drain?
    liftIO $ R.writeIORef ref drain
    case drain of
        NullObj -> return NullObj
        _       -> call drain "flowingFrom" [f]
callFountObj _ o verb args = refused o verb args -- nargs

callDrainObj :: Handle -> Obj -> MessageHandler
callDrainObj _ d "flowingFrom" [_] = return d
callDrainObj h _ "receive" [StrObj s] = do
    -- XXX blocks?
    liftIO $ hPutStr h s
    return NullObj
callDrainObj _ o verb args = refused o verb args -- nargs

callBuiltinObj :: String -> MessageHandler
callBuiltinObj "__booleanFlow" "failureList" [IntObj 0] =
    return . ConstListObj . Seq.singleton $ BoolObj False
callBuiltinObj "__equalizer" "sameEver" [x, y] =
    return . BoolObj $ sameEver x y
callBuiltinObj "__loop" "run" [iterable, consumer] = do
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
callBuiltinObj "boolean" "coerce" [obj@(BoolObj _), _] = return obj
callBuiltinObj "boolean" "coerce" [obj, ej] = fire ej obj >> return NullObj
callBuiltinObj "connectTo" "run" [StrObj host, IntObj port] = do
    -- XXX needs to happen after the turn is finished
    -- XXX blocks
    addrInfo <- liftIO $ getAddrInfo Nothing (Just host) Nothing
    case addrInfo ^? _head . to addrAddress of
        Just (SockAddrInet _ ip) -> liftIO $ do
            s <- socket AF_INET Stream defaultProtocol
            -- XXX blocks
            connect s $ SockAddrInet (fromIntegral port) ip
            h <- socketToHandle s ReadWriteMode
            ref <- R.newIORef NullObj
            return . ConstListObj . Seq.fromList $ [] -- @@@@FountObj h ref, DrainObj h]
        _            -> left Unknown
callBuiltinObj "stdout" "print" [StrObj s] = do
    liftIO $ putStr s
    return NullObj
callBuiltinObj "throw" "eject" [ej, payload] = do
    fire ej payload
    return NullObj
callBuiltinObj "traceln" "run" args = do
    liftIO $ print args
    return NullObj
callBuiltinObj n verb args = refused NullObj verb args -- NullObj XXX; nargs

--         | FountObj Handle (IORef Obj)
--         | DrainObj Handle
--         | UserObj Unique String Env (M.Map String [Method]) [Matcher]
