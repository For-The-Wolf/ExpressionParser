{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module ExpressionParser.Expr where
    
import Control.Monad (ap)

data Operator = Multiply | Divide | Add | Subtract deriving(Eq)
data Variable = X|Y|Z deriving (Eq)
data Expr a =  Val Double | Var a | Node Operator (Expr a) (Expr a) deriving (Eq, Functor)
data NestedList a = Item a | NestedList [NestedList a] deriving Show
data Func a = Func String a (Expr a)

class DisplayVariable a where
    displayVariable :: a -> String

instance DisplayVariable String where
    displayVariable = id 

instance DisplayVariable Variable where
    displayVariable = show

instance DisplayVariable Integer where
    displayVariable = show

instance Monad (Expr) where
    (>>=) = sub
    return = Var :: a -> Expr a

instance Applicative Expr where
    (<*>) = ap
    pure = return
  

instance Show Variable where
    show X = "x"
    show Y = "y"
    show Z = "z"

instance (Show a, DisplayVariable a) => Show (Expr a) where
    show (Val x) = show x 
    show (Var x) = displayVariable x
    show (Node operator l r) = "("++(show l) ++ show operator ++ (show r) ++ ")"

instance Num (Expr a) where
    (+) = add
    (*) = multiply
    (-) = minus
    negate a = minus 0 a
    fromInteger i = Val (fromIntegral i)
    abs a = a
    signum a = a

instance Fractional (Expr a) where
    (/) = divide
    recip a = divide (Val 1) a
    fromRational r = Val(fromRational r)

instance Show Operator where
    show Multiply = "*"
    show Divide = "/"
    show Add = "+"
    show Subtract = "-"

instance (Show a, DisplayVariable a) => Show (Func a) where
    show (Func name var expr) = name ++ "(" ++ (displayVariable var) ++ ") = " ++ (show expr)

-- Operations on expressions

applyOp :: Operator -> Double -> Double -> Maybe Double
applyOp Divide l 0 = Nothing
applyOp Divide l r = Just (l / r)
applyOp Multiply l r = Just(l * r)
applyOp Add l r = Just (l + r)
applyOp Subtract l r = Just (l - r)

eval_expr :: (Expr a) -> Maybe Double
eval_expr (Val x) = Just x
eval_expr (Var x) = Nothing
eval_expr (Node operator l r) = do n <- eval_expr l
                                   m <- eval_expr r
                                   applyOp operator n m

get_val :: Maybe Double -> Double
get_val Nothing = 0
get_val (Just x) = x



replace :: (Eq a) => a -> Expr a -> a -> Expr a
replace new_var expr var
    | var == new_var = expr
    | otherwise = (Var var)

sub :: Expr a -> (a -> Expr b) -> Expr b
sub (Val x) _ = Val x 
sub (Var x) function = function x
sub (Node operation l r) function = Node operation (sub l function) (sub r function)

simplify :: (Eq a) => Expr a -> Expr a
simplify expr 
    | expr == simpleExpr = expr
    | otherwise = simplify (simpleExpr)
    where
        simpleExpr = stepSimplify expr

stepSimplify :: (Eq a) => Expr a -> Expr a
stepSimplify (Val a) = Val a
stepSimplify (Var a) = Var a
stepSimplify (Node op (Val a) (Val b)) = Val (get_val (eval_expr (Node op (Val a) (Val b))))
stepSimplify (Node Multiply (Val 0) _) = Val 0
stepSimplify (Node Multiply _ (Val 0)) = Val 0
stepSimplify (Node Add a (Val 0)) = stepSimplify a
stepSimplify (Node Add (Val 0) a) = stepSimplify a
stepSimplify (Node Subtract a (Val 0)) = stepSimplify (a)
stepSimplify (Node Multiply (Val 1) a) = stepSimplify (a)
stepSimplify (Node Multiply a (Val 1)) = stepSimplify (a)
stepSimplify (Node Divide a (Val 1)) = stepSimplify (a)
stepSimplify (Node Add a b) | a == b = (Node Multiply 2 (stepSimplify a))
--stepSimplify (Node Add (multiply a b) (multiply a' b')) | a == a' = (Node multiply a (add b b'))
stepSimplify (Node op a b) = Node op (stepSimplify a) (stepSimplify b)

differentiate :: (Eq a) => Expr a -> a -> Expr a
differentiate expr var = simplify (p_diff expr var)

p_diff :: (Eq a) => Expr a -> a -> Expr a
p_diff (Val _) _ = Val 0
p_diff (Var varx) vary 
    | varx == vary = Val 1
    | otherwise = Val 0
p_diff (Node Multiply l r) var = ((p_diff r var)*l) + (r * (p_diff l var))
p_diff (Node Divide l r) var = (((p_diff l var) * r) - (l*(p_diff r var) )) / (r * r)
p_diff (Node op l r) var = Node op (p_diff l var) (p_diff r var)


multiply :: Expr a -> Expr a -> Expr a
multiply l r = Node Multiply l r

divide :: Expr a -> Expr a -> Expr a
divide l r = Node Divide l r

add :: Expr a -> Expr a -> Expr a
add l r = Node Add l r

minus :: Expr a -> Expr a -> Expr a
minus l r = Node Subtract l r


--Operations on functions

--fSub :: (Eq a) => Func a -> Expr a -> Func a
--fSub (Func name var expr) toSub = (Func name var (expr >>= (replace var toSub)))

subNum :: Func String -> Double -> Func String
subNum (Func name var expr) num = (Func name (show num) (simplify (expr >>= (replace var (Val num)))))


dF :: (Eq a) => Func a -> Func a
dF (Func name var expr) = Func (name ++ "'") var (differentiate expr var)

--Parse string to expression

is_num :: String -> Bool 
is_num a
    | a == "1" = True
    | a == "2" = True
    | a == "3" = True
    | a == "4" = True
    | a == "5" = True
    | a == "6" = True
    | a == "7" = True
    | a == "8" = True
    | a == "9" = True
    | a == "0" = True
    | a == "." = True
    |otherwise = False

getNext :: String -> String
getNext str = next str ""

getLeft :: String -> String
getLeft str = left str ""

getSplit :: String -> [String]
getSplit str = splitStr str []

next :: String -> String -> String
next "" store = store
next str store 
    | is_num [head str] = next (tail str) (store ++ [head str])
    | [head str] == " " = next (tail str) store
    | store == "" = [head str]
    | otherwise = store

left :: String -> String -> String
left "" store = ""
left str store 
    | is_num [head str] = left (tail str) (store ++ [head str])
    | [head str] == " " = left (tail str) store
    | store == "" = tail str
    | otherwise = str

splitStr :: String -> [String] -> [String]
splitStr "" list = list
splitStr str list = splitStr (getLeft str) (list ++ [(getNext str)])

get_last :: [String] -> [String] -> Int -> [String]
get_last [] last _  = last
get_last str last count
    | (head str) == "(" = get_last (tail str) last (count + 1)
    | (head str) == ")" = get_last (tail str) last (count - 1)
    | count > 0 = (get_last (tail str) last count)
    | otherwise = (str)

first_int ::[String] -> [NestedList String] -> [NestedList String]
first_int [] result = result
first_int list result
    | (head list) == "(" =  result
    | (head list) == ")" =  result
    | otherwise = first_int (tail list) (result ++ [Item (head list)])

next_int ::[String] -> [String]
next_int [] = []
next_int list 
    | (head list) == "(" = list
    | (head list) == ")" = list
    | otherwise = next_int (tail list) 

nestedList :: [String] -> [NestedList String] -> (NestedList String)
nestedList [] result = NestedList result
nestedList str result
    | (head str) == "(" = nestedList (get_last (tail str) [] 1) (result ++ [nestedList (tail str) []])
    | (head str) == ")" = NestedList result
    | otherwise = nestedList (next_int str) (result ++ first_int str [])

toNestedList :: [String] -> (NestedList String)
toNestedList string_expr = nestedList string_expr []

stringToNestedList :: String -> (NestedList String)
stringToNestedList string = toNestedList (getSplit string)

getItem :: String -> Expr String
getItem (item) 
    | (reads item :: [(Double,String)]) ==  [] = Var item
    | otherwise = Val (read item :: Double) 

getOperator :: NestedList String -> Operator
getOperator (Item a)
    | (a == "*") = Multiply
    | (a == "/") = Divide
    | (a == "-") = Subtract
    | (a == "+") = Add

toExpr :: [NestedList String] -> Expr String
toExpr [Item a] = getItem a
toExpr [NestedList a] = toExpr a
toExpr (a : b : xs) = Node (getOperator b) (toExpr [a]) (toExpr xs)

str2Expr :: String -> Expr String
str2Expr str = simplify (toExpr [stringToNestedList str])

getDouble :: IO Double
getDouble = readLn

-- Further string parsing 

isFunc :: String -> Bool
isFunc "" = False
isFunc s
    | (head s) == '=' = True
    | otherwise = isFunc (tail s)

getExpr :: [String] -> [String]
getExpr s
    | (head s) == "=" = tail s
    | otherwise = getExpr (tail s)

getFunc :: [String] -> [String]
getFunc s = reverse (getExpr (reverse s))


stepName :: [String] -> String -> String
stepName remaining name
    | (head remaining) == "(" = name
    | otherwise = stepName (tail remaining) (name ++ (head remaining))

getName :: [String] -> String
getName lst = stepName lst ""
--(1,2,3)
stepVar :: [String] -> String -> Bool -> String
stepVar remaining  done isInBracket 
    | (head remaining) == ")" = done
    | (head remaining) == "(" = stepVar (tail remaining) done True
    | isInBracket = stepVar (tail remaining) (done ++ (head remaining)) isInBracket
    | otherwise = stepVar (tail remaining) done isInBracket

getVar :: [String] -> String
getVar lst = stepVar lst "" False

str2FuncName :: String -> String
str2FuncName string = getName (getFunc (getSplit string))

str2FuncVar :: String -> String
str2FuncVar string = getVar (getFunc (getSplit string))

str2FuncExpr :: String -> Expr String
str2FuncExpr string = toExpr [toNestedList (getExpr (getSplit string))]

str2Func :: String -> Func String
str2Func string = Func (str2FuncName string) (str2FuncVar string) (str2FuncExpr string)

getVarName ::Func String -> String
getVarName (Func _ var _) = var
