{-# LANGUAGE FlexibleInstances #-}
-- Copyright (C) 2014 Google Inc. All rights reserved.
--
-- Licensed under the Apache License, Version 2.0 (the "License"); you may not
-- use this file except in compliance with the License. You may obtain a copy
-- of the License at
--
-- http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
-- WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
-- License for the specific language governing permissions and limitations
-- under the License.

module Test where


import Control.Monad
import Control.Applicative ((<$>), (<*>))
import Test.QuickCheck

import Masque.Monte (Obj(..))
import Masque.Objects (sameEver)
import Masque.Lexer (Token(..), Shape(..), Direction(..),
                     lexer, unlex, idStart, idPart)


instance Arbitrary Obj where
    arbitrary = oneof [ null, bools, ints ]
        where
        null = elements [ NullObj ]
        bools = elements [ BoolObj False, BoolObj True ]
        ints = do
            i <- choose (-5, 5)
            return $ IntObj i

propSameEverIdentical x = sameEver x x

newtype BalancedTokens = BalancedTokens [Token]
                       deriving (Show)
unTokens :: BalancedTokens -> [Token]
unTokens (BalancedTokens toks) = toks

instance Arbitrary BalancedTokens where
  arbitrary = BalancedTokens <$> (sized rest)
    where
      rest :: Int -> Gen [Token]
      rest 0 = return []
      rest n | (n `mod` 10) == 0 = brackets $ rest (n - 1)
      rest n | (n `mod` 13) == 0 = mkComment
      rest n = (:) <$> token <*> (rest (n - 1))
      brackets :: Gen [Token] -> Gen [Token]
      brackets toks = (++) <$> bra <*> ((++) <$> toks <*> ket) where
        s :: Gen Shape
        s = arbitrary
        sort :: Gen (Shape -> Direction -> Token)
        sort = elements [TokBracket, TokDollarBracket, TokAtBracket]
        bra = do { s' <- s; sort' <- sort; return [(sort' s' Open)]}
        ket = do { s' <- s; sort' <- sort; return [(sort' s' Close)]}

      mkComment :: Gen [Token]
      mkComment = (++) <$> (listOf1 com) <*> (listOf1 nl) where
        com = TokComment <$> listOf (suchThat (return 'C') (\c -> (c > '\n')))
        nl = TokNewLine <$> arbitrary

      genid :: Gen String
      genid = (:) <$> (suchThat (return 'i') idStart) <*> (listOf (suchThat (return 'd') idPart))

      token = oneof [
          return KW_as
        , return KW_bind , return KW_break
        , return KW_catch , return KW_continue
        , return KW_def
        , return KW_else , return KW_escape , return KW_exit , return KW_extends , return KW_exports
        , return KW_finally , return KW_fn , return KW_for
        , return KW_guards
        , return KW_if , return KW_implements , return KW_imports , return KW_in , return KW_interface
        , return KW_match , return KW_meta , return KW_method
        , return KW_object
        , return KW_pass , return KW_pragma
        , return KW_return
        , return KW_switch
        , return KW_to , return KW_try
        , return KW_var , return KW_via
        , return KW_when , return KW_while
                           
        , liftM TokIDENTIFIER genid
        , liftM TokDOLLAR_IDENT genid
        , liftM TokAT_IDENT genid
          
        , liftM TokChar arbitrary
        , liftM TokInt arbitrary
        , liftM TokDouble arbitrary
        , liftM TokString arbitrary
          
          -- TODO     , liftM TokQUASI_TEXT arbitrary
          
        , return TokAssign
        , liftM TokVERB_ASSIGN genid
          
        , return TokAdd , return TokSubtract
        , return TokShiftLeft , return TokShiftRight
        , return TokXor
        , return TokMod
        , return TokMultiply , return TokFloorDivide , return TokApproxDivide
        , return TokPow
        , return TokAnd , return TokOr
                          
        , return TokComplement
        , return TokRangeIncl , return TokRangeExcl
        , return TokAsBigAs , return TokLEq , return TokGEq , return TokEq , return TokNotEq
        , return TokMatchBind , return TokNotMatchBind
        , return TokLogicalAnd , return TokLogicalOr , return TokButNot
                                                       
        , return TokSemi , return TokComma , return TokColon
        , return TokStringNoun
        , return TokSuchThat , return TokIgnore
        , return TokCall , return TokSend
        , return TokArrow , return TokFatArrow
                            
        , liftM TokNewLine arbitrary
        ]

instance Arbitrary Shape where
  arbitrary = elements $ enumFromTo minBound maxBound

instance Arbitrary Direction where
  arbitrary = elements $ enumFromTo minBound maxBound

propLexInverseUnlex :: BalancedTokens -> Bool
propLexInverseUnlex btoks = lexer [] (concatMap unlex (unTokens btoks)) == (unTokens btoks)


main = do
    print "propFail..."
    quickCheck propLexInverseUnlex
    print "sameEver..."
    quickCheck propSameEverIdentical
