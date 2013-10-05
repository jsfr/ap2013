import Test.HUnit
import APLisp

test1 :: Test
test1 = TestCase $ assertBool "Plus" $
        interpret "(+ 1 2)" ==
        Right (IntVal 3)

test2 :: Test
test2 = TestCase $ assertBool "Minus" $
        interpret "(- 1 2)" ==
        Right (IntVal (-1))

test3 :: Test
test3 = TestCase $ assertBool "Multiplication" $
        interpret "(* 2 2)" ==
        Right (IntVal 4)

test4 :: Test
test4 = TestCase $ assertBool "Division" $
        interpret "(/ 2 2)" ==
        Right (IntVal 1)

test5 :: Test
test5 = TestCase $ assertBool "Division by zero" $
        interpret "(/ 2 0)" ==
        Left "Error: Divison by zero"

test6 :: Test
test6 = TestCase $ assertBool "If True" $
        interpret "(if (= 0 0) (+ 1 2) (- 1 2))" ==
        Right (IntVal 3)

test7 :: Test
test7 = TestCase $ assertBool "If False" $
        interpret "(if (!= 0 0) (+ 1 2) (- 1 2))" ==
        Right (IntVal (-1))

test8 :: Test
test8 = TestCase $ assertBool "Cons" $
        interpret "(cons 1 (list 2 3))" ==
        Right (ListExp [IntVal 1, IntVal 2, IntVal 3])

test9 :: Test
test9 = TestCase $ assertBool "Car" $
        interpret "(car (list 1 2 3))" ==
        Right (IntVal 1)

test10 :: Test
test10 = TestCase $ assertBool "Cdr" $
         interpret "(cdr (list 1 2 3))" ==
         Right (IntVal 3)

test11 :: Test
test11 = TestCase $ assertBool "Let" $
         interpret "(let ((x 2) (y (+ 1 2))) (+ x y))" ==
         Right (IntVal 5)

test12 :: Test
test12 = TestCase $ assertBool "Function of list" $
         interpret "(let ((f (lambda (x y) (+ x y)))) (funcall f (list 1 2)))" ==
         Right (IntVal 3)

test13 :: Test
test13 = TestCase $ assertBool "Function of singleton" $
         interpret "(let ((f (lambda (y) (+ y y)))) (funcall f 1))" ==
         Right (IntVal 2)

test14 :: Test
test14 = TestCase $ assertBool "Function of list, too few arguments" $
         interpret "(let ((f (lambda (x y) (+ x y)))) (funcall f (list 1)))" ==
         Left "Error: Function call failed"

tests :: Test
tests = TestList [TestLabel "Arithmetics" $ TestList [test1, test2, test3, test4, test5],
                  TestLabel "If" $ TestList [test6, test7],
                  TestLabel "Lists" $ TestList [test8, test9, test10],
                  TestLabel "Local scopes" $ TestList [test11, test12, test13, test14]]

main :: IO Counts
main = runTestTT tests