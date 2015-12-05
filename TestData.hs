module TestData where

import Text.JSON
import qualified Text.Parsec as P

import qualified Masque.Lex2 as ML2

data LexerTestCase =
  LexerTestCase { input :: String, expected :: [ML2.TagTok]}
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


type Failure = (String, -- test function
                LexerTestCase, -- input, expected
                Either P.ParseError [ML2.TagTok]) -- actual

lexerTestResults :: [LexerTestFunc] -> [Failure]
lexerTestResults testData = concatMap testFunc testData
  where
    testFunc fcs = concatMap (testCase $ func fcs) (cases fcs)
    testCase f c = let
      actual = if f == "test_holes"
               then ML2.lexStringIn ['`'] (input c)
               else ML2.lexString (input c)
      in
        if actual == Right (expected c)
        then []
        else [(f, c, actual)]


-- | Usage: readFile "testsuite/lexer.json" >>= runLexerTests
runLexerTests :: String -> IO ()
runLexerTests jsonTestData = case decode jsonTestData of
  (Ok testData) -> runTests testData
  (Error msg) -> putStrLn msg
  where
    runTests testData = do
      _ <- mapM printFailure (lexerTestResults testData)
      return ()

    printFailure (f, c, actual) = do
      putStrLn ""
      putStrLn $ "FAILURE in: " ++ f
      putStrLn $ "     input:" ++ (input c)
      putStrLn $ "  expected: " ++ (showJSValue (showJSON $ expected c) "")
      printResult actual

    printResult (Right toks) = do
      putStrLn $ "    actual: " ++ (showJSValue (showJSON $ toks) "")
    printResult (Left err) = do
      putStrLn $ "  ERROR: " ++ (show err)

