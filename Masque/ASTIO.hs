{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Masque.ASTIO where

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Binary.Get
import Data.Binary.IEEE754
import Data.Bits
import qualified Data.ByteString.Char8 as BSC
import Data.Data.Lens
import Data.List
import Data.Word
import Text.PrettyPrint.GenericPretty

import Masque.AST

instance Out Expr
instance Out NamedExpr
instance Out Method
instance Out Matcher
instance Out Patt
instance Out NamedPatt

instance Plated Expr where
    plate = uniplate

instance Plated Patt where
    plate = uniplate

-- | Deserialization

unshift :: Word8 -> Word8
unshift = subtract 32

getByte :: Get Word8
getByte = liftM unshift getWord8

getVarInt :: Get Integer
getVarInt = do
    b <- getByte
    let rv = toInteger $ b .&. 0x7f
    if b .&. 0x80 == 0x00
        then return rv
        else do
            vi <- getVarInt
            return $ rv .|. vi `shiftL` 7

getDouble :: Get Double
getDouble = do
    bs <- replicateM 8 getByte
    let w64 = foldl' (\w x -> w `shiftL` 8 .|. fromIntegral x) 0 bs
    return $ wordToDouble w64

zzd :: Integer -> Integer
zzd i = if i .&. 1 == 1 then (i `div` 2) `xor` (-1) else i `div` 2

getPatternList :: Get [Patt]
getPatternList = do
    7 <- getByte
    arity <- getVarInt
    replicateM (fromIntegral arity) getPattern

getMaybePattern :: Get (Maybe Patt)
getMaybePattern = do
    tag <- getByte
    case tag of
        -- null pattern
        0  -> return $ Nothing
        _ -> do
          p <- getPatternTag tag
          return $ Just p

getPattern :: Get Patt
getPattern = do
    tag <- getByte
    pat <- getPatternTag tag
    return pat

getPatternTag :: Word8 -> Get Patt
getPatternTag tag = do
    case tag of
        27 -> FinalPatt <$> getNoun <*> getMaybeNode
        28 -> IgnorePatt <$> getMaybeNode
        29 -> VarPatt <$> getNoun <*> getMaybeNode
        30 -> do
            -- Must be a tuple
            ps <- getPatternList
            -- Discard the tail
            _ <- getPattern
            return $ ListPatt ps
        31 -> ViaPatt <$> getNode <*> getPattern
        32 -> BindPatt <$> getNoun
        _  -> fail "Not a pattern"

getNoun :: Get String
getNoun = do
    node <- getNode
    case node of
        StrExpr s  -> return s
        NounExpr s -> return s
        _          -> fail "Not a string or noun"

getStr :: Get String
getStr = do
    node <- getNode
    case node of
        StrExpr s  -> return s
        _          -> fail "Not a string"

getMaybeNode :: Get (Maybe Expr)
getMaybeNode = do
    tag <- getByte
    case tag of
        0  -> return $ Nothing
        _ -> do
          e <- getNodeTag tag
          return $ Just e

getNode :: Get Expr
getNode = do
    tag <- getByte
    expr <- getNodeTag tag
    return expr

getNodeTag :: Word8 -> Get Expr
getNodeTag tag = do
    case tag of
        0  -> return $ NounExpr "null"
        3  -> do
            len <- getVarInt
            bs <- getByteString $ fromIntegral len
            return $ StrExpr $ BSC.unpack bs
        4  -> DoubleExpr <$> getDouble
        -- 5  -> CharNode
        6  -> do
            i <- getVarInt
            return $ IntExpr $ zzd i
        7  -> do
            arity <- getVarInt
            ps <- replicateM (fromIntegral arity) getNode
            return $ undefined -- TODO Tuple ps
        -- Literals contain a single literal node
        10 -> getNode
        11 -> NounExpr <$> getNoun
        12 -> BindingExpr <$> getNoun
        -- TODO
        -- 13 -> do
        --     Tuple ns <- getNode
        --     return $ SequenceExpr ns
        -- TODO: args, namedargs
        -- 14 -> CallExpr <$> getNode <*> getStr <*> [getNode] <*> []
        15 -> DefExpr <$> getPattern <*> getNode <*> getNode
        16 -> EscapeExpr <$> getPattern <*> getNode <*> getPattern <*> getNode
              -- TODO: ObjectExpr
        -- 17 -> ObjectExpr <$> getNode <*> getPattern <*> getNode <*> getNode
        -- 18 -> do
        --     s <- Script <$> getNode
        --    Tuple methods <- getNode
        --    Tuple matchers <- getNode
        --    return $ s methods matchers
        -- 19 -> do
        --    m <- Method <$> getNode <*> getNode
        --    ps <- getPatternList
        --    m ps <$> getNode <*> getNode
        -- 20 -> Matcher <$> getPattern <*> getNode
        21 -> AssignExpr <$> getNoun <*> getNode
        22 -> FinallyExpr <$> getNode <*> getNode
        23 -> TryExpr <$> getNode <*> getPattern <*> getNode
        24 -> HideExpr <$> getNode
        25 -> IfExpr <$> getNode <*> getNode <*> getNode
        33 -> do
            StrExpr [c] <- getNode
            return $ CharExpr c
        x  -> fail $ "Not a node: " ++ show x
