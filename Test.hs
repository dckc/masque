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
import Data.Char (isPrint, isUpper, isLower, isDigit)
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


balancedTokens :: Gen [Token]
balancedTokens = sized step where
  step :: Int -> Gen [Token]
  step 0 = return []
  step n
    | n > 0 = frequency [(10, left), (10, right), (2, wrap), (1, comment)]
    where
      left = do
        ts <- step (n - 1)
        t <- nonBracket
        return $ t:ts
      right = do
        ts <- step (n - 1)
        t <- nonBracket
        return $ ts ++ [t]
      wrap = do
        ts <- step (n - 1)
        bra <- openBracket
        let ket = closeFor bra
        return $ [bra] ++ ts ++ [ket]
      comment = do
        ts <- step (n - 1)
        com <- genComment
        return $ com ++ ts

nonBracket :: Gen Token
nonBracket = oneof (
        [return t |
         (_, t) <- (keywords ++ operators ++ punctuation)]
        ++
        [ctor <$> genid |
         ctor <- [TokIDENTIFIER, TokDOLLAR_IDENT, TokAT_IDENT, TokVERB_ASSIGN]]
        ++ [
          liftM TokChar arbitrary
        , liftM TokInt $ liftM getPositive arbitrary
        , liftM TokDouble $ liftM getPositive arbitrary
          -- TODO: string quoting stuff
        , liftM TokString noEscapes
          -- TODO     , liftM TokQUASI_TEXT arbitrary
        , liftM TokNewLine $ liftM getPositive arbitrary
        ])
  where
    genid :: Gen String
    genid =
      (:) <$> (suchThat arbitrary idStart) <*> (listOf (suchThat arbitrary idPart))
    noEscapes :: Gen String
    noEscapes = listOf (suchThat arbitrary
                        (\c -> (isPrint c) &&
                               (not (elem c "\"\\@$`"))))

openBracket :: Gen Token
openBracket = elements [TokBracket Round Open,
                        TokBracket Square Open,
                        TokDollarBracket Curly Open,
                        TokAtBracket Curly Open,
                        -- TODO TokBracket Quasi Open,
                        TokBracket Curly Open]

closeFor :: Token -> Token
closeFor (TokBracket shape Open) = (TokBracket shape Close)
closeFor (TokDollarBracket shape Open) = (TokBracket shape Close)
closeFor (TokAtBracket shape Open) = (TokBracket shape Close)
closeFor _ = (TokBracket Curly Close)

genComment :: Gen [Token]
genComment = do
  csize <- choose (0, 120)
  text <- vectorOf csize (frequency [
                             (10, (suchThat arbitrary isUpper)),
                             (10, (suchThat arbitrary isDigit)),
                             (60, (suchThat arbitrary isLower)),
                             (15, return ' '),
                             (5, (suchThat arbitrary isPrint))])
  indent <- choose (0, 120)
  return [TokComment text, TokNewLine indent]

propSameEverIdentical x = sameEver x x

prop_lexUnlex1 = forAll nonBracket $ \t ->
  lexer [] (unlex t) == [t]

prop_lexUnlex = forAll balancedTokens $ \ts ->
  lexer [] (concatMap unlex ts) == ts

prop_lexDouble = forAll (liftM getPositive arbitrary) $ \x ->
  (lexer [] (unlex $ TokDouble x)) == [TokDouble x]

main = do
    print "lexDouble..."
    quickCheck prop_lexDouble
    print "lexUnlex1..."
    quickCheck prop_lexUnlex1
    print "propLexInverseUnlex..."
    verboseCheck prop_lexUnlex
    print "sameEver..."
    quickCheck propSameEverIdentical
