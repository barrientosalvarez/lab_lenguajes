module Postfix where

-- Creacion del tipo de dato Stack
data Stack a = Mt |Top Int (Stack Int) deriving(Show)

-- Creacion del tipo de dato Token
type N = Int
data Token = TokenNum N
            | TokenAdd
            | TokenDiv
            | TokenEq
            | TokenGt
            | TokenLt
            | TokenMul
            | TokenNget
            | TokenPop
            | TokenRem
            | TokenSel
            | TokenSub
            | TokenSwap
            deriving(Show)

-- FUNCIONES PRINCIPALES

-- LEXER Y AUXLEXER
lexer :: String -> [Token]
lexer s = auxLexer (words s)

auxLexer :: [String] -> [Token]
auxLexer [] = []
auxLexer ("(profix": n)=[]
auxLexer ("(profix": n : xs) = auxLexer (n : xs)
auxLexer ("TokenAdd" : xs) = TokenAdd : auxLexer xs
auxLexer ("TokenDiv" : xs) = TokenDiv : auxLexer xs
auxLexer ("TokenEq" : xs) = TokenEq : auxLexer xs
auxLexer ("TokenGt" : xs) = TokenGt : auxLexer xs
auxLexer ("TokenLt" : xs) = TokenLt : auxLexer xs
auxLexer ("TokenMul" : xs) = TokenMul : auxLexer xs
auxLexer ("TokenNget" : xs) = TokenNget : auxLexer xs
auxLexer ("TokenPop" : xs) = TokenPop : auxLexer xs
auxLexer ("TokenRem" : xs) = TokenRem : auxLexer xs
auxLexer ("TokenSel" : xs) = TokenSel : auxLexer xs
auxLexer ("TokenSub" : xs) = TokenSub : auxLexer xs
auxLexer ("TokenSwap" : xs) = TokenSwap : auxLexer xs
auxLexer ("TokenAdd)" : xs) = TokenAdd : auxLexer xs
auxLexer ("TokenDiv)" : xs) = TokenDiv : auxLexer xs
auxLexer ("TokenEq)" : xs) = TokenEq : auxLexer xs
auxLexer ("TokenGt)" : xs) = TokenGt : auxLexer xs
auxLexer ("TokenLt)" : xs) = TokenLt : auxLexer xs
auxLexer ("TokenMul)" : xs) = TokenMul : auxLexer xs
auxLexer ("TokenNget)" : xs) = TokenNget : auxLexer xs
auxLexer ("TokenPop)" : xs) = TokenPop : auxLexer xs
auxLexer ("TokenRem)" : xs) = TokenRem : auxLexer xs
auxLexer ("TokenSel)" : xs) = TokenSel : auxLexer xs
auxLexer ("TokenSub)" : xs) = TokenSub : auxLexer xs
auxLexer ("TokenSwap)" : xs) = TokenSwap : auxLexer xs
auxLexer (n : xs) = TokenNum (read n) : auxLexer xs



-- Parser
--parser :: [Token] -> Pgm



-- AUXILIARES

-- Encuentra el tamaño de un Stack
size :: Stack Int -> Int
size Mt = 0
size (Top n(p))=1+size p

-- Elimina el top de un stack
pop :: Stack Int -> Stack Int
pop (Top n (Top m (p)))=(Top m(p))


-- Elimina los dos elementos de arriba de un stack
doublePop :: Stack Int -> Stack Int
doublePop (Top n (Top m (p)))= p

-- Regresa el elemento de arriba de un stack
peek :: Stack Int -> Int
peek (Top n(p))=n


-- Agrega un nuevo elemento al stack
append :: Stack Int -> Int -> Stack Int
append p n = Top n (p)

-- Intercambia los dos elementos de arriba de un stack
swap :: Stack Int -> Stack Int
swap (Top n(Top m (p)))=(Top m (Top n (p)))


-- EJEMPLOS
a1 :: Stack Int
a1= Top 4(Top 3 (Top 2 (Top 1 Mt)))
