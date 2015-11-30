module TestData where

import Text.JSON

import qualified Masque.Lex2 as ML2

type TagTok = (String, JSValue)

data LexerTestCase =
  LexerTestCase { input :: String, expected :: [TagTok]}
  deriving (Eq, Show)

data LexerTestFunc =
  LexerTestFunc { func :: String, cases :: [LexerTestCase] }
  deriving (Eq, Show)

toJSO :: [(String, JSValue)] -> JSValue
toJSO = JSObject . toJSObject

instance JSON LexerTestCase where
  showJSON tc =
    toJSO [("input", showJSON $ input tc),
           ("expected", showJSON $ expected tc)]
  readJSON (JSObject o) = do
    i <- valFromObj "input" o
    ex <- valFromObj "expected" o
    return LexerTestCase { input = i, expected = ex }
  readJSON _ = Error "?"

instance JSON LexerTestFunc where
  showJSON tf =
    toJSO [("func", showJSON $ func tf),
           ("cases", showJSON $ cases tf)]
  readJSON (JSObject o) = do
    f <- valFromObj "func" o
    cs <- valFromObj "cases" o
    return LexerTestFunc { func = f, cases = cs }
  readJSON _ = Error "?"

lexerTestData :: IO (Result [LexerTestFunc])
lexerTestData = do
  text <- readFile "testsuite/lexer.json"
  return $ lexerTestCases text

lexerTestCases :: String -> Result [LexerTestFunc]
lexerTestCases text = decode text

lexerTestResults fs = concatMap testFunc fs
  where
    testFunc fcs = concatMap (testCase $ func fcs) (cases fcs)
    testCase f c = let
      actual = ML2.lexString (input c)
      in
        if actual == Right (expected c)
        then []
        else [(f, c, actual)]
