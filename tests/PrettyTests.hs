{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module PrettyTests (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH

import Nix.Expr
import Nix.Pretty

case_indented_antiquotation :: Assertion
case_indented_antiquotation = do
    assertPretty (mkIndentedStr 0 "echo $foo") "''echo $foo''"
    assertPretty (mkIndentedStr 0 "echo ${foo}") "''echo ''${foo}''"

case_string_antiquotation :: Assertion
case_string_antiquotation = do
    -- TODO: plain $ doesn't need to be escaped here either
    assertPretty (mkStr "echo $foo") "\"echo \\$foo\""
    assertPretty (mkStr "echo ${foo}") "\"echo \\${foo}\""

case_function_params :: Assertion
case_function_params =
    assertPretty (mkFunction (mkParamset [] True) (mkInt 3)) "{ ... }:\n  3"

tests :: TestTree
tests = $testGroupGenerator

--------------------------------------------------------------------------------
assertPretty :: NExpr -> String -> Assertion
assertPretty e s = assertEqual ("When pretty-printing " ++ show e) s . show $ prettyNix e
