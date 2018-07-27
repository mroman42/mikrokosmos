module Libraries
  ( stdlibraries
  , stdmap
  , code
  , name
  , description
  )
where

import qualified Data.Map as M
import Control.Arrow

stdlibraries :: String
stdlibraries = unlines $ map code stdfunctions

stdmap :: M.Map String Function
stdmap = M.fromList $ map (name &&& id) stdfunctions

data Function = Function
  { code :: String
  , name :: String
  , description :: String
  }

stdfunctions :: [Function]
stdfunctions = 
  [ Function "id" "id = \\x.x" "Identity function, evaluates to its argument."
  , Function "const" "const = \\x.\\y.x" "Binay function evaluating to its first argument."
  , Function "compose" "compose = \\f.\\g.\\x.f (g x)" "Function composition."
  , Function "true" "true  = \\a.\\b.a" "Boolean 'true', using Church encoding."
  , Function "false" "false = \\a.\\b.b" "Boolean 'false', using Church encoding."
  , Function "and" "and = \\p.\\q.p q p" "Boolean conjunction."
  , Function "or" "or  = \\p.\\q.p p q" "Boolean disjunction."
  , Function "not" "not = \\b.b false true" "Boolean negation."
  , Function "implies" "implies = \\a.\\b.or (not a) b" "Boolean implication."
  , Function "ifelse" "ifelse = (\\x.x)" "Identity function, can be used as a boolean case analysis."
  , Function "succ" "succ = \\n.\\f.\\x.f (n f x)" "Return the successor of a natural number."
  , Function "0" "0 = \\f.\\x.x" "The natural number 0, using Church encoding."
  , Function "plus" "plus = \\m.\\n.\\f.\\x.m f (n f x)" "Adds two natural numbers."
  , Function "mult" "mult = \\m.\\n.\\f.\\x.m (n f) x" "Multiplies two natural numbers."
  , Function "pred" "pred  = \\n.\\f.\\x.n (\\g.(\\h.h (g f))) (\\u.x) (\\u.u)" "Predecessor of a natural number"
  , Function "minus" "minus = \\m.\\n.(n pred) m" "Substracts two natural numbers"
  , Function "iszero" "iszero = \\n.(n (\\x.false) true)" "Returns true if the natural number is zero."
  , Function "leq" "leq = \\m.\\n.(iszero (minus m n))" "Returns true if the first argument is a natural number lesser or equal than the second."
  , Function "geq" "geq = \\m.\\n.(iszero (minus n m))" "Returns true if the first argument is a natural number greater or equal than the second."
  , Function "lt" "lt = \\m.\\n.not (geq m n)" "Returns true if the first argument is a natural number lesser than the second."
  , Function "gt" "gt = \\m.\\n.not (leq m n)" "Returns true if the first argument is a natural number greater than the second."
  , Function "eq" "eq = \\m.\\n.(and (leq m n) (leq n m))" "Returns true if the two natural numbers are equal."
  , Function "1" "1 = succ 0" ""
  , Function "2" "2 = succ 1" ""
  , Function "3" "3 = succ 2" ""
  , Function "4" "4 = succ 3" ""
  , Function "5" "5 = succ 4" ""
  , Function "6" "6 = succ 5" ""
  , Function "7" "7 = succ 6" ""
  , Function "8" "8 = succ 7" ""
  , Function "9" "9 = succ 8" ""
  , Function "10" "10 = succ 9" ""
  , Function "11" "11 = succ 10" ""
  , Function "12" "12 = succ 11" ""
  , Function "13" "13 = succ 12" ""
  , Function "14" "14 = succ 13" ""
  , Function "15" "15 = succ 14" ""
  , Function "16" "16 = succ 15" ""
  , Function "17" "17 = succ 16" ""
  , Function "18" "18 = succ 17" ""
  , Function "19" "19 = succ 18" ""
  , Function "20" "20 = succ 19" ""
  , Function "21" "21 = succ 20" ""
  , Function "22" "22 = succ 21" ""
  , Function "23" "23 = succ 22" ""
  , Function "24" "24 = succ 23" ""
  , Function "25" "25 = succ 24" ""
  , Function "26" "26 = succ 25" ""
  , Function "27" "27 = succ 26" ""
  , Function "28" "28 = succ 27" ""
  , Function "29" "29 = succ 28" ""
  , Function "30" "30 = succ 29" ""
  , Function "31" "31 = succ 30" ""
  , Function "32" "32 = succ 31" ""
  , Function "33" "33 = succ 32" ""
  , Function "34" "34 = succ 33" ""
  , Function "35" "35 = succ 34" ""
  , Function "36" "36 = succ 35" ""
  , Function "37" "37 = succ 36" ""
  , Function "38" "38 = succ 37" ""
  , Function "39" "39 = succ 38" ""
  , Function "40" "40 = succ 39" ""
  , Function "41" "41 = succ 40" ""
  , Function "42" "42 = succ 41" ""
  , Function "43" "43 = succ 42" ""
  , Function "44" "44 = succ 43" ""
  , Function "45" "45 = succ 44" ""
  , Function "46" "46 = succ 45" ""
  , Function "47" "47 = succ 46" ""
  , Function "48" "48 = succ 47" ""
  , Function "49" "49 = succ 48" ""
  , Function "50" "50 = succ 49" ""
  , Function "51" "51 = succ 50" ""
  , Function "52" "52 = succ 51" ""
  , Function "53" "53 = succ 52" ""
  , Function "54" "54 = succ 53" ""
  , Function "55" "55 = succ 54" ""
  , Function "56" "56 = succ 55" ""
  , Function "57" "57 = succ 56" ""
  , Function "58" "58 = succ 57" ""
  , Function "59" "59 = succ 58" ""
  , Function "60" "60 = succ 59" ""
  , Function "61" "61 = succ 60" ""
  , Function "62" "62 = succ 61" ""
  , Function "63" "63 = succ 62" ""
  , Function "64" "64 = succ 63" ""
  , Function "65" "65 = succ 64" ""
  , Function "66" "66 = succ 65" ""
  , Function "67" "67 = succ 66" ""
  , Function "68" "68 = succ 67" ""
  , Function "69" "69 = succ 68" ""
  , Function "70" "70 = succ 69" ""
  , Function "71" "71 = succ 70" ""
  , Function "72" "72 = succ 71" ""
  , Function "73" "73 = succ 72" ""
  , Function "74" "74 = succ 73" ""
  , Function "75" "75 = succ 74" ""
  , Function "76" "76 = succ 75" ""
  , Function "77" "77 = succ 76" ""
  , Function "78" "78 = succ 77" ""
  , Function "79" "79 = succ 78" ""
  , Function "80" "80 = succ 79" ""
  , Function "81" "81 = succ 80" ""
  , Function "82" "82 = succ 81" ""
  , Function "83" "83 = succ 82" ""
  , Function "84" "84 = succ 83" ""
  , Function "85" "85 = succ 84" ""
  , Function "86" "86 = succ 85" ""
  , Function "87" "87 = succ 86" ""
  , Function "88" "88 = succ 87" ""
  , Function "89" "89 = succ 88" ""
  , Function "90" "90 = succ 89" ""
  , Function "91" "91 = succ 90" ""
  , Function "92" "92 = succ 91" ""
  , Function "93" "93 = succ 92" ""
  , Function "94" "94 = succ 93" ""
  , Function "95" "95 = succ 94" ""
  , Function "96" "96 = succ 95" ""
  , Function "97" "97 = succ 96" ""
  , Function "98" "98 = succ 97" ""
  , Function "99" "99 = succ 98" ""
  , Function "100" "100 = succ 99" ""
  , Function "S" "S = \\x.\\y.\\z. x z (y z)" "S combinator. Substitution operator."
  , Function "K" "K = \\x.\\y.x" "K combinator. Constant operator."
  , Function "I" "I = S K K" "I combinator. Identity operator."
  , Function "C" "C = \\f.\\x.\\y.f y x" "C combinator."
  , Function "B" "B = \\f.\\g.\\x.f (g x)" "B combinator."
  , Function "W" "W = \\x.\\y.(y y)" "W combinator."
  , Function "Y" "Y != \\f.(\\x.f (x x))(\\x.f (x x))" "Y combinator. Fixed-point combinator."
  , Function "tuple" "tuple  = \\x.\\y.\\z.z x y" "Untyped tuple constructor. Takes a and b and returns the tuple (a,b)."
  , Function "first" "first  = \\p.p true" "Untyped tuple projection. Returns the first element of a tuple."
  , Function "second" "second = \\p.p false" "Untyped tuple projection. Returns the second element of a tuple."
  , Function "cons" "cons = \\h.\\t.\\c.\\n.(c h (t c n))" "List constructor. Appends an element to the head of the list."
  , Function "nil" "nil = \\c.\\n.n" "List constructor. Creates the empty list."
  , Function "foldr" "foldr = \\o.\\n.\\l.(l o n)" "List folding. Combines the elements of the list using a binary operation."
  , Function "fold" "fold  = \\o.\\n.\\l.(l o n)" "List folding. Combines the elements of the list using a binary operation."
  , Function "head" "head = fold const nil" "Returns the head of a list."
  , Function "tail" "tail = \\l.first (l (\\a.\\b.tuple (second b) (cons a (second b))) (tuple nil nil))" "Extracts the head of a list."
  , Function "take" "take = \\n.\\l.first (n (\\t.tuple (cons (head (second t)) (first t)) (tail (second t))) (tuple nil l))" "Extracts all the elements except the head of a list"
  , Function "sum" "sum = (foldr plus 0)" "Adds a list of natural numbers."
  , Function "prod" "prod   = (foldr mult (succ 0))" "Multiplies a list of natural numbers."
  , Function "length" "length = (foldr (\\h.\\t.succ t) 0)" "Returns the length of a list."
  , Function "map" "map    = (\\f.(foldr (\\h.\\t.cons (f h) t) nil))" "Maps a function over a list. Returns a list obtained by applying the function to each element of the original list."
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
  ]
