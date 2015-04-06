{-# LANGUAGE OverloadedStrings #-}
import Test.Tasty
import Test.Tasty.HUnit
import IrcParser
import Data.Attoparsec.Text

testParse = parseOnly parseIrc

main = defaultMain tests
tests = testGroup "Tests" [parserTests]

parserTests = testGroup "Parser tests"
    [ testCase "Command only"
        $ testParse "asdf" @?= (Right $ Message "" "asdf" [])
    , testCase "Command plus param" $
        testParse "foo bar" @?= (Right $ Message "" "foo" ["bar"])
    , testCase "Command plus message" $
        testParse "MSG :hey there" @?= (Right $ Message "" "MSG" ["hey there"])
    , testCase "Command param plus message" $
        testParse "MSG msg :hey there" @?= (Right $ Message "" "MSG" ["msg", "hey there"])
    , testCase "Name plus message" $
        testParse ":tolsun MSG :hey there" @?= (Right $ Message "tolsun" "MSG" ["hey there"])
    ]
