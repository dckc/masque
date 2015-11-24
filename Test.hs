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
import Data.Char (isPrint)
import Test.QuickCheck

import Masque.Monte (Obj(..))
import Masque.Objects (sameEver)
import Masque.Lexer (Token(..), Shape(..), Direction(..),
                     lexer, unlex, idStart, idPart,
                     keywords, operators, punctuation)


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
      rest n | (n `mod` 11) == 0 = wrap $ rest (n - 1)
      rest n | (n `mod` 23) == 0 = mkComment -- always followed by newline
      rest n = (:) <$> token <*> (rest (n - 1))

      wrap :: Gen [Token] -> Gen [Token]
      wrap toks = do
        sort <- elements [TokBracket Round,
                          TokBracket Square,
                          -- TokDollarBracket Curly -- TODO
                          -- TokAtBracket Curly
                          -- TokBracket Quasi,
                          TokBracket Curly]
        toks' <- toks
        return ([(sort Open)] ++ toks' ++ [(sort Close)])

      mkComment :: Gen [Token]
      mkComment = do
        com <- listOf (suchThat arbitrary isPrint) :: Gen String
        indent <- liftM getPositive arbitrary :: Gen Int
        return [TokComment com, TokNewLine indent]

      genid :: Gen String
      genid = (:) <$> (suchThat arbitrary idStart) <*> (listOf (suchThat arbitrary idPart))

      noEscapes :: Gen String
      noEscapes = suchThat arbitrary (\s ->
                                 (not ('"' `elem` s)
                                  || ('\\' `elem` s)
                                  || ('@' `elem` s)
                                  || ('$' `elem` s)
                                  || ('`' `elem` s)))

      token = oneof (
        [return t |
         (_, t) <- (keywords ++ operators ++ punctuation)]
        ++ [
          liftM TokIDENTIFIER genid
        , liftM TokDOLLAR_IDENT genid
        , liftM TokAT_IDENT genid
          
        , liftM TokChar arbitrary
        , liftM TokInt $ liftM getPositive arbitrary
        , liftM TokDouble $ liftM getPositive arbitrary
          -- TODO: string quoting stuff
        , liftM TokString noEscapes
          
          -- TODO     , liftM TokQUASI_TEXT arbitrary
          
        , TokVERB_ASSIGN <$> genid
          
        , liftM TokNewLine $ liftM getPositive arbitrary
        ])

instance Arbitrary Shape where
  arbitrary = elements $ enumFromTo minBound maxBound

instance Arbitrary Direction where
  arbitrary = elements $ enumFromTo minBound maxBound

propLexInverseUnlex :: BalancedTokens -> Bool
propLexInverseUnlex btoks =
  lexer [] (concatMap unlex (unTokens btoks)) == (unTokens btoks)


main = do
    print "propLexInverseUnlex..."
    verboseCheck propLexInverseUnlex
    print "sameEver..."
    quickCheck propSameEverIdentical
