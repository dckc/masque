module Masque.Deserialize where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State
import Data.Binary.Get
import Data.Binary.IEEE754
import Data.Bits
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import Data.Foldable
import Data.List (genericIndex)

import Masque.AST

monteMagic :: BSL.ByteString
monteMagic = BSL.pack $ map (toEnum . fromEnum) "Mont\xe0MASTx00"

data MASTContext = MASTContext {
  ctxExprs :: [Expr],
  ctxMethods :: [Method],
  ctxMatchers :: [Matcher],
  ctxPatts :: [Patt]
  }
                 deriving Show

emptyContext :: MASTContext
emptyContext = MASTContext {
  ctxExprs = [],
  ctxMethods = [],
  ctxMatchers = [],
  ctxPatts = []
  }

getVarInt :: StateT MASTContext Get Integer
getVarInt = do
    b <- lift getWord8
    let rv = toInteger $ b .&. 0x7f
    if b .&. 0x80 == 0x00
        then return rv
        else do
            vi <- getVarInt
            return $ rv .|. vi `shiftL` 7

getDouble :: StateT MASTContext Get Double
getDouble = do
    bs <- replicateM 8 (lift getWord8)
    let w64 = foldl' (\w x -> w `shiftL` 8 .|. fromIntegral x) 0 bs
    return $ wordToDouble w64

zzd :: Integer -> Integer
zzd i = if i .&. 1 == 1 then (i `div` 2) `xor` (-1) else i `div` 2

-- XXX UTF-8 decode
getStr :: StateT MASTContext Get String
getStr = do
    len <- getVarInt
    bs <- lift $ getByteString (fromIntegral len)
    return $ BSC.unpack bs

getNode :: (MASTContext -> [a]) -> StateT MASTContext Get a
getNode field = do
    i <- getVarInt
    gets (\ctx -> (field ctx) `genericIndex` i)

getExpr :: StateT MASTContext Get Expr
getExpr = getNode ctxExprs

getMaybeExpr :: StateT MASTContext Get (Maybe Expr)
getMaybeExpr = do
  e <- getExpr
  return $ case e of
    SequenceExpr [] -> Nothing -- see tag 'N' below
    _ -> Just e

getPatt :: StateT MASTContext Get Patt
getPatt = getNode ctxPatts

getNodes :: (MASTContext -> [a]) -> StateT MASTContext Get [a]
getNodes field = do
    i <- getVarInt
    replicateM (fromInteger i) (getNode field)

getExprs :: StateT MASTContext Get [Expr]
getExprs = getNodes ctxExprs

getPatts :: StateT MASTContext Get [Patt]
getPatts = getNodes ctxPatts

getNamedExprs :: StateT MASTContext Get [NamedExpr]
getNamedExprs = do
    i <- getVarInt
    replicateM (fromInteger i) $ do
        k <- getExpr
        v <- getExpr
        return $ NamedExpr k v

getNamedPatts :: StateT MASTContext Get [NamedPatt]
getNamedPatts = do
    i <- getVarInt
    replicateM (fromInteger i) $ do
        k <- getExpr
        p <- getPatt
        v <- getExpr
        return $ NamedPatt k p v

nextTag :: StateT MASTContext Get ()
nextTag = do
    tag <- toChar <$> lift getWord8
    case tag of
        -- Literals
        'L' -> do
            literalTag <- toChar <$> lift getWord8
            expr <- case literalTag of
                -- 'C' -> do ...
                'D' -> DoubleExpr <$> getDouble
                'I' -> do
                    i <- getVarInt
                    return $ IntExpr $ zzd i
                'N' -> return $ SequenceExpr [] -- "NullExpr" @@
                'S' -> StrExpr <$> getStr
                _ -> error "bad literal tag @@todo: show tag"
            modify (\ctx -> ctx { ctxExprs = (ctxExprs ctx) ++ [expr] })
        -- Patts
        'P' -> do
            pattTag <- toChar <$> lift getWord8
            patt <- case pattTag of
                'A' -> ViaPatt <$> getExpr <*> getPatt
                'B' -> BindingPatt <$> getStr
                'F' -> FinalPatt <$> getStr <*> getMaybeExpr
                'I' -> IgnorePatt <$> getMaybeExpr
                'L' -> ListPatt <$> getPatts
                'V' -> VarPatt <$> getStr <*> getMaybeExpr
                _ -> error "bad pattern tag @@todo: show tag"
            modify (\ctx -> ctx { ctxPatts = (ctxPatts ctx) ++ [patt]})
        'M' -> do
            method <- Method <$> getStr <*> getStr <*> getPatts <*> getNamedPatts <*> getExpr <*> getExpr
            modify (\ctx -> ctx { ctxMethods = (ctxMethods ctx) ++ [method]})
        'R' -> do
            matcher <- Matcher <$> getPatt <*> getExpr
            modify (\ctx -> ctx { ctxMatchers = (ctxMatchers ctx) ++ [matcher]})
        -- Exprs
        tag' -> do
            expr <- case tag' of
                'A' -> AssignExpr <$> getStr <*> getExpr
                'B' -> BindingExpr <$> getStr
                'C' -> CallExpr <$> getExpr <*> getStr <*> getExprs <*> getNamedExprs
                'D' -> DefExpr <$> getPatt <*> getMaybeExpr <*> getExpr
                'E' -> EscapeExpr <$> getPatt <*> getExpr <*> getPatt <*> getExpr
                'F' -> FinallyExpr <$> getExpr <*> getExpr
                'H' -> HideExpr <$> getExpr
                'I' -> IfExpr <$> getExpr <*> getExpr <*> getMaybeExpr
                'N' -> NounExpr <$> getStr
                'O' -> ObjectExpr <$> getStr <*> getPatt <*> getExpr <*> getExprs <*> (getNodes ctxMethods) <*> (getNodes ctxMatchers)
                'S' -> SequenceExpr <$> getExprs
                'Y' -> TryExpr <$> getExpr <*> getPatt <*> getExpr
                'e' -> EscapeOnlyExpr <$> getPatt <*> getExpr
                _ -> error "bad expr tag @@todo: show tag"
            modify (\ctx -> ctx { ctxExprs = (ctxExprs ctx) ++ [expr] })
    where
      toChar = toEnum . fromEnum
