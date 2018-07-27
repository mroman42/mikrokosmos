module Libraries
  ( stdlibraries
  , stdmap
  , stdquery
  , code
  , name
  , description
  )
where

import qualified Data.Map as M
import Control.Arrow

stdlibraries :: String
stdlibraries = unlines $ map code stdfunctions

stdquery :: String -> Maybe Function
stdquery = flip M.lookup stdmap

stdmap :: M.Map String Function
stdmap = M.fromList $ map (name &&& id) stdfunctions

data Function = Function
  { name :: String
  , code :: String
  , description :: String
  }
  deriving (Show)

stdfunctions :: [Function]
stdfunctions = 
  [ Function "id" "id = \\x.x" "Identity function. Returns its argument unchanged."
  , Function "const" "const = \\x.\\y.x" "Binay function evaluating to its first argument."
  , Function "compose" "compose = \\f.\\g.\\x.f (g x)" "Function composition."
  , Function "true" "true = \\a.\\b.a" "Boolean 'true', using Church encoding."
  , Function "false" "false = \\a.\\b.b" "Boolean 'false', using Church encoding."
  , Function "and" "and = \\p.\\q.p q p" "Boolean conjunction."
  , Function "or" "or = \\p.\\q.p p q" "Boolean disjunction."
  , Function "not" "not = \\b.b false true" "Boolean negation."
  , Function "implies" "implies = \\a.\\b.or (not a) b" "Boolean implication."
  , Function "ifelse" "ifelse = (\\x.x)" "Identity function, can be used as a boolean case analysis."
  , Function "succ" "succ = \\n.\\f.\\x.f (n f x)" "Return the successor of a natural number."
  , Function "0" "0 = \\f.\\x.x" "The natural number 0, using Church encoding."
  , Function "plus" "plus = \\m.\\n.\\f.\\x.m f (n f x)" "Adds two natural numbers."
  , Function "mult" "mult = \\m.\\n.\\f.\\x.m (n f) x" "Multiplies two natural numbers."
  , Function "pred" "pred = \\n.\\f.\\x.n (\\g.(\\h.h (g f))) (\\u.x) (\\u.u)" "Predecessor of a natural number"
  , Function "minus" "minus = \\m.\\n.(n pred) m" "Substracts two natural numbers"
  , Function "iszero" "iszero = \\n.(n (\\x.false) true)" "Returns true if the natural number is zero."
  , Function "leq" "leq = \\m.\\n.(iszero (minus m n))" "Returns true if the first argument is a natural number lesser or equal than the second."
  , Function "geq" "geq = \\m.\\n.(iszero (minus n m))" "Returns true if the first argument is a natural number greater or equal than the second."
  , Function "lt" "lt = \\m.\\n.not (geq m n)" "Returns true if the first argument is a natural number lesser than the second."
  , Function "gt" "gt = \\m.\\n.not (leq m n)" "Returns true if the first argument is a natural number greater than the second."
  , Function "eq" "eq = \\m.\\n.(and (leq m n) (leq n m))" "Returns true if the two natural numbers are equal."
  , Function "S" "S = \\x.\\y.\\z. x z (y z)" "S combinator. Substitution operator."
  , Function "K" "K = \\x.\\y.x" "K combinator. Constant operator."
  , Function "I" "I = S K K" "I combinator. Identity operator."
  , Function "C" "C = \\f.\\x.\\y.f y x" "C combinator."
  , Function "B" "B = \\f.\\g.\\x.f (g x)" "B combinator."
  , Function "W" "W = \\x.\\y.(y y)" "W combinator."
  , Function "Y" "Y != \\f.(\\x.f (x x))(\\x.f (x x))" "Y combinator. Fixed-point combinator."
  , Function "tuple" "tuple = \\x.\\y.\\z.z x y" "Untyped tuple constructor. Takes a and b and returns the tuple (a,b)."
  , Function "first" "first = \\p.p true" "Untyped tuple projection. Returns the first element of a tuple."
  , Function "second" "second = \\p.p false" "Untyped tuple projection. Returns the second element of a tuple."
  , Function "cons" "cons = \\h.\\t.\\c.\\n.(c h (t c n))" "List constructor. Appends an element to the head of the list."
  , Function "nil" "nil = \\c.\\n.n" "List constructor. Creates the empty list."
  , Function "foldr" "foldr = \\o.\\n.\\l.(l o n)" "List folding. Combines the elements of the list using a binary operation."
  , Function "fold" "fold = \\o.\\n.\\l.(l o n)" "List folding. Combines the elements of the list using a binary operation."
  , Function "head" "head = fold const nil" "Returns the head of a list."
  , Function "tail" "tail = \\l.first (l (\\a.\\b.tuple (second b) (cons a (second b))) (tuple nil nil))" "Extracts the head of a list."
  , Function "take" "take = \\n.\\l.first (n (\\t.tuple (cons (head (second t)) (first t)) (tail (second t))) (tuple nil l))" "Extracts all the elements except the head of a list"
  , Function "sum" "sum = (foldr plus 0)" "Adds a list of natural numbers."
  , Function "prod" "prod = (foldr mult (succ 0))" "Multiplies a list of natural numbers."
  , Function "length" "length = (foldr (\\h.\\t.succ t) 0)" "Returns the length of a list."
  , Function "map" "map = (\\f.(foldr (\\h.\\t.cons (f h) t) nil))" "Maps a function over a list. Returns a list obtained by applying the function to each element of the original list."
  , Function "filter" "filter = \\p.(foldr (\\h.\\t.((p h) (cons h t) t)) nil)" "Filters a list with a predicate. Returns a list containing only the elements that satisfy the predicate."
  , Function "node" "node = \\x.\\l.\\r.\\f.\\n.(f x (l f n) (r f n))" "Binary tree constructor. Creates a tree whose root is given by the first argument and whose left and right subtrees are given by the second and third arguments, respectively."
  , Function "omega" "omega := (\\x.(x x))(\\x.(x x))" "Omega combinator. An example of a non-reducible lambda calculus expression."
  , Function "fix" "fix := (\\f.(\\x.f (x x)) (\\x.f (x x)))" "Fixed-point combinator. Given f, returns an element x such that f x = x."
  , Function "fact" "fact := fix (\\f.\\n.iszero n (succ 0) (mult n (f (pred n))))" "Factorial of a natural number."
  , Function "fib" "fib := fix (\\f.\\n.iszero n (succ 0) (plus (f (pred n)) (f (pred (pred n)))))" "Returns the n-th Fibonacci number."
  , Function "naturals" "naturals := fix (compose (cons 0) (map (plus 1)))" "Infinite list of the natural numbers."
  , Function "infinity" "infinity := fix succ" "Fixed point of the successor function on the natural numbers."
  , Function "division" "division := fix (\\d.\\q.\\a.\\b. lt a b (tuple q a) (d (succ q) (minus a b) b)) 0" "Integer division between natural numbers. Returns the quotient and the modulo."
  , Function "mu" "mu := \\p.fix (\\f.\\n.(p n) n (f (succ n))) 0" "Göedel mu-operator. Finds the first natural number satisfying a certain predicate."
  , Function "div" "div := \\a.\\b.first (division a b)" "Quotient of the division between two natural numbers."
  , Function "mod" "mod := \\a.\\b.second (division a b)" "Modulo of the division between two natural numbers."
  , Function "pair" "pair = \\x.\\y.(x,y)" "Pair constructor of the simply-typed lambda calculus."
  , Function "fst" "fst = \\x.FST x" "First projection of a typed pair."
  , Function "snd" "snd = \\x.SND x" "Second projection of a typed pair."
  , Function "inl" "inl = \\x.INL x" "First injection of a typed sum."
  , Function "inr" "inr = \\x.INR x" "Second injection of a typed sum."
  , Function "caseof" "caseof = \\x.\\y.\\z.CASE x OF (\\a.y a);(\\a.z a)" "Case analysis of a sum type."
  , Function "unit" "unit = UNIT" "The only element of the unit type."
  , Function "abort" "abort = \\x.ABORT x" "Takes an element of the empty type and creates an element of any given type."
  , Function "absurd" "absurd = \\x.ABSURD x" "Takes an element of the empty type and creates an element of any given type."
  ] ++ natsdef
  where
    fnat :: Int -> Function
    fnat n = Function
      (show (n+1))
      (show (n+1) ++ " = succ " ++ show n)
      ("The natural number " ++ show (n+1) ++ ", using Church encoding.")
    
    natsdef :: [Function]
    natsdef = map fnat [0..999]
     
