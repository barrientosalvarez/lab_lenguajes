module Test where

import Postfix
import EAB.Semantic as EAB
import EAB.Sintax as EABSin

import Test.QuickCheck

-- Postfix

-- Stackss
s1 :: Stack Int
s1 = Top 4 (Top 2 (Top 3 (Top 2 Mt)))

s2 :: Stack Int
s2 = Mt

s3 :: Stack Int
s3 = Top 5 (Top 8 (Top 2 (Top 2 (Top 1 (Top 4 Mt)))))

s4 :: Stack Int
s4 = Top 4 (Top 3 (Top 0 Mt))

s5 :: Stack Int
s5 = Mt

-- Tests
t1_Postfix :: Bool
t1_Postfix = Postfix.eval s1 (Postfix.parser $ Postfix.lexer "postfix 4 add mul sub") == -16

t2_Postfix :: Bool
t2_Postfix = Postfix.eval s2 (Postfix.parser $ Postfix.lexer "postfix 0 1 2 add 3 mul 9 div 4 add 5 eq") == 1

t3_Postfix :: Bool
t3_Postfix = Postfix.eval s3 (Postfix.parser $ Postfix.lexer "postfix 6 pop mul swap sel gt") == 0

t4_Postfix :: Bool
t4_Postfix = Postfix.eval s4 (Postfix.parser $ Postfix.lexer "postfix 3 sel 4 add 5 swap sub 6 7 8 3 nget pop pop lt") == 1

t5_Postfix :: Bool
t5_Postfix = Postfix.eval s5 (Postfix.parser $ Postfix.lexer "postfix 0 4 5 gt 6 7 sel 5 8 rem") == 5

-- EAB

t1_EAB :: Bool
t1_EAB = EAB.eval  (EABSin.parser $ EABSin.lexer "let x = 5 in x * (let y = 4 in 36 + y end) end") == Left 200

t2_EAB :: Bool

t2_EAB = EAB.eval  (EABSin.parser $ EABSin.lexer "10 * let x = let v = 4 in v * 2 end in let z = x + 1 in 3 + let h = z in (h * 4) end end end") == Left 390

t3_EAB :: Bool
t3_EAB = EAB.eval  (EABSin.parser $ EABSin.lexer "(let v = 4 in v * 2 + 4 end) * 3 + let h = 12 in (h * 4) end") == Left 84

t4_EAB :: Bool
t4_EAB = EAB.eval  (EABSin.parser $ EABSin.lexer "not true or false and false")  == Right False

t5_EAB :: Bool
t5_EAB = EAB.eval  (EABSin.parser $ EABSin.lexer "not true or false and false or true and true or true")  == Right True



main = do
    putStrLn "Test Postfix"
    quickCheck t1_Postfix
    quickCheck t2_Postfix
    quickCheck t3_Postfix
    quickCheck t4_Postfix
    quickCheck t5_Postfix
    putStrLn ""
    putStrLn "Test EAB"
    quickCheck t1_EAB
    quickCheck t2_EAB
    quickCheck t3_EAB
    quickCheck t4_EAB
    quickCheck t5_EAB
    
