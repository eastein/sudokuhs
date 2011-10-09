-- Eric Stein
-- CS534 Project
-- Constraint Net and Matrix Code

module CSPL (Variable, CNet, CMatrix, complete, sudokuNet, createNet, assign, consistent, incomplete, choices, backTracking, finalState, look, procCosts, assignments, checkDeadline, nvars) where
import Helpers
import Maybe

-- Name, Domain
data Variable = Variable [Char] [[Char]]

instance Show Variable where
  show (Variable n d) =
    n ++ " has domain: " ++ (show d) ++ "\n"

data CNet = CNet [Variable] [CMatrix]

instance Show CNet where
  show (CNet v c) =
    "Constraint Net\nVariables:\n" ++ concat (map show v) ++ "Constraints\n" ++ concat (map show c)

-- names of two variables
-- constraint matrix
data CMatrix = CMatrix [Char] [Char] [([Char], [([Char], Bool)])]

instance Show CMatrix where
  show (CMatrix l r g) =
    l ++ "/" ++ r ++ " permitted:\n" ++ (show g) ++ "\n"

-- get the variable name from a variable
name :: Variable -> [Char]
name (Variable k v) = k

names :: CMatrix -> [[Char]]
names (CMatrix a b c) = [a, b]

-- unary variable. for use with unary constraints.
unary = (Variable "0" ["a"])

-- get the domain of a variable
domain :: Variable -> [[Char]]
domain (Variable n d) = d

--number of variables in a net
nvars :: CNet -> Int
nvars (CNet v m) = length v

graph :: CMatrix -> [([Char], [([Char], Bool)])]
graph (CMatrix l r g) = g


--sudoku specific code begin--
--
stringnums n = map show (take n naturals)

--create a fully assigned and constrained sudoku network from a parsed input
sudokuNet (n, nq, d) = let v = sequenceVars nq in 
		   foldl (\net (var, val) -> assign net var val) (addNEQs (CNet (unary:v) (map (\x -> createMatrix unary x) v)) ((rowConstraints nq) ++ (colConstraints nq) ++ (blockConstraints n))) (assignments nq d)

--create a list of variable assignments from a parsed input
assignments n i = let nums = take n naturals
			     in
			     filter
			     (\(var, val) -> not (val == "0"))
			     (foldl (++) [] (zipWith (\row rn -> zipWith (\element cn -> ((show rn) ++ "-" ++ (show cn), element)
									 ) row nums
						     ) i nums))

--create variables in sequence
sequenceVars :: Int -> [Variable]
sequenceVars n = let nums = stringnums n
		     x = take (n * n) (cycle nums)
		     y = qsort (\m -> m) x
		in 
		createVars (zipWith (\a b -> a ++ "-" ++ b) x y) nums

--given a list of variable names and a domain, create vars with that domain
createVars :: [[Char]] -> [[Char]] -> [Variable]
createVars v d = map (\vn -> (Variable vn d)) v

--create row constraints
rowConstraints :: Int -> [([Char], [Char])]
rowConstraints n = let nums = stringnums n in
	foldl (++) [] (map (\a -> cartesianSelf a) (map (\r -> (map (\c -> r ++ "-" ++ c) nums)) nums))

--create column constraints
colConstraints :: Int -> [([Char], [Char])]
colConstraints n = let nums = stringnums n in
	foldl (++) [] (map (\a -> cartesianSelf a) (map (\c -> (map (\r -> r ++ "-" ++ c) nums)) nums))

--create block constraints
blockConstraints :: Int -> [([Char], [Char])]
blockConstraints n = let bstarts = take (n * n) (cycle (map toInteger (take n (iterate (\x -> x + n) 0))))
		         sseq = take n naturals in
		         foldl (++) [] (map (\a -> cartesianSelf a) (zipWith (\a b -> let r = map show (take (n * n) (cycle (map (\x -> x + a) sseq)))
									                  c = map show (qsort (\i -> i) (take (n * n) (cycle (map (\x -> x + b) sseq))))
					   						  in
											  zipWith (\xc yc -> xc ++ "-" ++ yc) r c
									     ) (sort bstarts) bstarts))

--produce a distinct cartesian product of the list as pairs
cartesianSelf :: [a] -> [(a, a)]
cartesianSelf (f:r) = (map (\o -> (f,o)) r) ++ (cartesianSelf r)
cartesianSelf [] = []

--given a network and a list of pairs of variables, add binary not equal constraints if no constraint exists
addNEQs :: CNet -> [([Char], [Char])] -> CNet
addNEQs n l = foldl (\n (v1, v2) -> addNEQ n (getVariable n v1) (getVariable n v2)) n l
--sudoku specific code end--

--schedule specific code begin--
-- given parsed input file, create variables.
createVariables :: [[[Char]]] -> [Variable]
createVariables p = (map (\vc -> (Variable vc (p!!1))) (map (\x -> (Helpers.tokenize x)!!0) (p!!0)))

createNet p = addBinaryNotSimul p (addBinaryNotEquals p (addBinaryEquals p (addUnaryExclusive p (addUnaryInclusive p (CNet (unary:(createVariables p)) (map (\x -> createMatrix unary x) (createVariables p)))))))

--add unary inclusive constraints to a net
addUnaryInclusive :: [[[Char]]] -> CNet -> CNet
addUnaryInclusive p n = mapMatch n (\m -> isOn m "0") (\m -> (foldl (\x d -> case(d) of (av, ki) ->  knock x "0" av ki ) m (map (\c -> ((head (Helpers.tokenize c)), zip (cycle ["a"]) (diff (p!!1) (tail (Helpers.tokenize c))))) (p!!3))))

--add unary exclusive constraints to a net
addUnaryExclusive :: [[String]] -> CNet -> CNet
addUnaryExclusive p n = mapMatch n (\m -> isOn m "0") (\m -> (foldl (\x d -> case(d) of (av, ki) ->  knock x "0" av ki ) m (map (\c -> ((head (Helpers.tokenize c)), zip (cycle ["a"]) (tail (Helpers.tokenize c)))) (p!!4))))

--add binary equals constraints to a net
addBinaryEquals :: [[String]] -> CNet -> CNet
addBinaryEquals p n = mapMatch (foldl (\ne pc -> addEmpty ne (getVariable ne (pc!!0)) (getVariable ne (pc!!1))) n (map Helpers.tokenize (p!!5))) (\m -> True) (\m -> (foldl (\x d -> case(d) of (l, r, kl) -> knock x l r kl) m (map (\pr -> ((pr!!0), (pr!!1), filter (\pc -> case (pc) of (p1, p2) -> not (p1 == p2)) (concat (map (\x -> zip (cycle [x]) (domain (getVariable n (pr!!0)))) (domain (getVariable n (pr!!1))) )))) (map Helpers.tokenize (p!!5)) )))

--add binary not equals constraints to a net
addBinaryNotEquals :: [[String]] -> CNet -> CNet
addBinaryNotEquals p n = mapMatch (foldl (\ne pc -> addEmpty ne (getVariable ne (pc!!0)) (getVariable ne (pc!!1))) n (map Helpers.tokenize (p!!6))) (\m -> True) (\m -> (foldl (\x d -> case(d) of (l, r, kl) -> knock x l r kl) m (map (\pr -> ((pr!!0), (pr!!1), filter (\pc -> case (pc) of (p1, p2) -> (p1 == p2)) (concat (map (\x -> zip (cycle [x]) (domain (getVariable n (pr!!0)))) (domain (getVariable n (pr!!1))) )))) (map Helpers.tokenize (p!!6)) )))

--add binary not simultaneous constraints to a net
addBinaryNotSimul p n = mapMatch (foldl (\ne pc -> addEmpty ne (getVariable ne (pc!!0)) (getVariable ne (pc!!1))) n (map Helpers.tokenize (p!!7))) (\m -> True) (\m -> (foldl (\x d -> case(d) of (l, r, kl) -> knock x l r kl) m (map (\pr -> ((pr!!0), (pr!!1), [((pr!!2), (pr!!3)), ((pr!!3), (pr!!2))])) (map Helpers.tokenize (p!!7)))))
--schedule specific code end--


-- create a nonconstraining constraint matrix
createMatrix :: Variable -> Variable -> CMatrix
createMatrix l r = CMatrix (name l) (name r) (zip (domain l) (replicate (length (domain l)) (zip (domain r) (replicate (length (domain r)) True))))

--backtracking search for csps.  to add heuristics, order the arrays passed to each foldl [todo]
backTracking :: CNet -> (CNet -> Bool) -> CNet
backTracking ci oconst = let cn = ac3 ci in
		if (complete cn) && (oconst cn) then
			cn
		else
			foldl (\n v -> if (complete n) && (oconst n) then
						n
					else
						foldl (\ni vi -> if (complete ni) && (oconst n) then
							ni
						else
							(backTracking (assign ni v vi) oconst)) cn (lcv n v (choices n v))) cn (mrv cn (incomplete cn))

--extract the final state of the network
finalState :: CNet -> [([Char], [Char])]
finalState n = map (\f -> case (f) of (v, (d, t)) -> (v, d)) (map (\(CMatrix l r g) -> (r, (((filter (\b -> case (b) of (b1, b2) -> b2) ((map (\x -> case (x) of (t, dat) -> dat) g)!!0))!!0)))) (varConstraints n "0"))

checkDeadline :: (Num b, Ord b) => CNet -> b -> [([Char], b)] -> Bool
checkDeadline cn d vals = all (\x -> case (x) of (x1, x2) -> (x2 <= d)) (procCosts cn vals)

--calculate costs per processor
procCosts :: (Num b) => CNet -> [([Char], b)] -> [([Char], b)]
procCosts cn vals = let cv = (map (\s -> case (s) of (v, d) -> (d, (look v vals))) (finalState cn)) in
			map (\p -> foldl (\pair1 pair2 -> case (pair1) of (n1, c1) -> case (pair2) of (n2, c2) -> (n1, c1+c2)) (p, 0) (filter (\ma -> case (ma) of (pr, cos) -> p == pr) cv)) (distinct (map (\x -> case (x) of (x1, x2) -> x1) cv))

-- check if the assignment is complete on the net
complete :: CNet -> Bool
complete (CNet v m) = all constraintComplete m

-- get the list of unassigned variables in a net
incomplete :: CNet -> [[Char]]
incomplete (CNet v m) = Helpers.distinct (concat (map ivars m))

-- enumerate the non-complete vars from a constraint
ivars :: CMatrix -> [[Char]]
ivars (CMatrix l r g) = concat [(if ((length (possible (CMatrix l r g) l)) > 1) then [l] else []), (if ((length (possible (CMatrix l r g) r)) > 1) then [r] else [])]

-- check if the assignment is consistent
consistent :: CNet -> Bool
consistent (CNet v m) = all constraintConsistent m

-- check if a constraint is completely assigned
constraintComplete :: CMatrix -> Bool
constraintComplete (CMatrix l r g) = ((length (possible (CMatrix l r g) l)) == 1) && ((length (possible (CMatrix l r g) r)) == 1)

-- check if a constraint is consistent
constraintConsistent :: CMatrix -> Bool
constraintConsistent (CMatrix l r g) = ((length (possible (CMatrix l r g) l)) > 0) && ((length (possible (CMatrix l r g) r)) > 0)

-- arc consistency
-- given a net, call the arc consistency algorithm on a fresh queue
ac3 :: CNet -> CNet
ac3 (CNet vr m) = ac3_ (CNet vr m) (foldl (++) [] (map (\p -> [(p!!0, p!!1), (p!!1, p!!0)]) (map names m)))

-- arc consistency: given a net and a queue, perform ac3 algorithm
ac3_ :: CNet -> [([Char], [Char])] -> CNet
ac3_ n ((v1, v2):r) = let (nn, t) = arcreduce n v1 v2 in
			if t then
				--add affected arcs to worklist, recurse
				ac3_ nn ((zip (map (\m -> otherVar m v1) (varConstraints n v1)) (cycle [v1]))++r)
			else
				--this reduction made no difference
				ac3_ n r
ac3_ n []    = n

--reduce the domain on an arc
arcreduce :: CNet -> [Char] -> [Char] -> (CNet, Bool)
arcreduce (CNet vs ms) v1 v2 = let m = (getMatrix n v1 v2)
				   n = (CNet vs ms) 
				   rem = (possible m v1)
				   x = ((filter (\x -> (name x) == v1) vs)!!0)
				   ovars = (filter (\x -> not ((name x) == v1)) vs)
				   nyvarns = (filter (\x -> not (x == v2)) (map name vs))
				   cdom = domain x
				   removed = (diff cdom rem)
				   in
				if (length removed > 0) then
					--reduce domain of variable
					((CNet ((Variable v1 rem):ovars) (rdomain ms v1 removed ovars)), True)
				else
					--domain requires no reduction
					(n, False)

--given a list of matrices, a variable and the set of values to remove for that variable, knock out all options for those values in all matching matrices
rdomain :: [CMatrix] -> [Char] -> [[Char]] -> [Variable] -> [CMatrix]
rdomain ms v vals vars = (map (\m -> let ov = otherVar m v
					 od = domain (getVar vars ov)
					 c = (length od) * (length vals)
				         in
					 knock m v ov (zip (qsort (\m -> m) (take c (cycle vals))) (take c (cycle od)))) (filter (\m -> isOn m v) ms))
	++
	(filter (\m -> not (isOn m v)) ms)

mrv_ :: CNet -> [Char] -> Int
mrv_ n v = length (intersect (map (\m -> possible m v) (varConstraints n v)))

--take network and list of variables to assign - order them by mrv
mrv :: CNet -> [[Char]] -> [[Char]]
mrv n l = map (\x -> case (x) of (x1, x2) -> x2) (qsort (\x -> case (x) of (x1, x2) -> x1) (map (\v -> (mrv_ n v, v)) l))

--take network, variable, and value assignment: return number of assignments on other variables eliminated directly
lcv_ :: CNet -> [Char] -> [Char] -> Int
lcv_ n v val = sum (map (\m -> (length (possible m (otherVar m v))) - (  length (possible (knock m v (otherVar m v)    (map (\vo -> (val,vo)) (domain (getVariable n (otherVar m v))))     ) (otherVar m v))  )) (varConstraints n v))

lcv :: CNet -> [Char] -> [[Char]] -> [[Char]]
lcv n v l = map (\x -> case (x) of (x1, x2) -> x2) (qsort (\x -> case (x) of (x1, x2) -> x1) (map (\val -> (lcv_ n v val, val)) l))

-- get the possible assignments for a variable - does *some* consistency checking, but not much
choices :: CNet -> [Char] -> [[Char]]
choices (CNet vr m) v = intersect (map (\mt -> possible mt v) (filter (\x -> isOn x v) m))

-- list the remaining domain of the specified variable, according to a constraint
possible :: CMatrix -> [Char] -> [[Char]]
possible (CMatrix l r g) v | l == v = map (\c -> case (c) of (c1, c2) -> c1) (filter (\c -> case (c) of (c1, c2) -> c2) (map (\p -> case (p) of (p1, p2) -> (p1, (any (\pi -> case (pi) of (pi1, pi2) -> pi2) p2))) g))
                           | r == v = Helpers.distinct (map (\c -> case (c) of (c1, c2) -> c1) (filter (\p -> case (p) of (p1, p2) -> p2) (concat (map (\fl -> case (fl) of (fl1, fl2) -> fl2) g))))

-- take a constraint net and set one of the variables.
assign :: CNet -> [Char] -> [Char] -> CNet
assign (CNet v m) vr vd = (CNet v ((\u -> case (u) of (u1, u2) -> concat [(map (\mt -> knock mt vr (otherVar mt vr) (alist (getVariable (CNet v m) vr) (getVariable (CNet v m) (otherVar mt vr)) vd)) u1), u2]) (split (\x -> isOn x vr) m)))

alist v1 v2 v1v = concat (map (\nv -> zip (cycle [nv]) (domain v2)) (Helpers.diff (domain v1) [v1v]))
--(map (\mt -> knock mt vr (otherVar mt vr) (map (\nv -> zip (cycle [nv]) (domain (getVariable (CNet v m))) (otherVar mt vr)) (Helpers.diff (domain (getVariable (CNet v m) vr)) [vd]))) u1)
--, u2]) (split (\x -> isOn x vr) m)))
--(map (\nv -> zip (cycle [nv]) (domain (getVariable (CNet v m) (otherVar mt vr)))) (Helpers.diff (domain (getVariable (CNet v m) vr)) [vd]))
--lookUp 
lookUp :: CMatrix -> [Char] -> [Char] -> Bool
lookUp (CMatrix l r g) lv rv = look rv (look lv g)

--set a coordinate in the matrix
bitBang :: CMatrix -> [Char] -> [Char] -> Bool -> CMatrix
bitBang (CMatrix l r g) lv rv b = (CMatrix l r ((\sp -> case (sp) of (sp1, sp2) -> (concat [[(\x -> case (x) of (kl, kd) -> (kl, (\y -> case (y) of (match, rest) -> (rv,b):rest)   (split (\x -> case (x) of (y1, y2) -> y1 == rv) kd))) (sp1!!0)], sp2])) (split (\x -> case (x) of (y1, y2) -> y1 == lv) g)))

--falsify all the listed pairs of assignments for the two variables
knock :: CMatrix -> [Char] -> [Char] -> [([Char], [Char])] -> CMatrix
knock m vl vr lof = foldl (\m f -> case (f) of (dr, dl) -> bang m vl vr dr dl False) m lof

--with specification of variable names, mark a set of assignments
bang (CMatrix l r g) vl vr dr dl b | (vl == l && vr == r) = bitBang (CMatrix l r g) dr dl b
                                   | (vl == r && vr == l) = bitBang (CMatrix l r g) dl dr b
                                   | otherwise = (CMatrix l r g)

--given a constraint net and a predicate, map the process over all the predicated matrices
mapMatch :: CNet -> (CMatrix -> Bool) -> (CMatrix -> CMatrix) -> CNet
mapMatch (CNet vars matrices) pred proc = (\sp -> case (sp) of (sp1, sp2) -> (CNet vars (concat [map proc sp1,sp2]))) (split pred matrices)

-- given a predicate function, split a list into matching items and and non-matching items
split :: (a -> Bool) -> [a] -> ([a], [a])
split p l = (filter p l, filter (\x -> not (p x)) l)

-- produce a list of constraints that are affected by a variable
varConstraints :: CNet -> [Char] -> [CMatrix]
varConstraints (CNet c n) v = filter (\x -> (isOn x v)) n

--get a specific var
getVariable :: CNet -> [Char] -> Variable
getVariable (CNet c n) v = (filter (\x -> (name x) == v) c)!!0

getVar :: [Variable] -> [Char] -> Variable
getVar c v = (filter (\x -> (name x) == v) c)!!0

-- if one does not exist, add a non-constraining constraint on the two variables specified
addEmpty :: CNet -> Variable -> Variable -> CNet
addEmpty (CNet vars matrices) v1 v2 | (any (\x -> onBoth x (name v1) (name v2)) matrices) = (CNet vars matrices)
                                    | otherwise = (CNet vars ((createMatrix v1 v2):matrices))


-- if one does not exist, add a binary not equal constraint on the two variables specified 
addNEQ :: CNet -> Variable -> Variable -> CNet
addNEQ (CNet vars matrices) v1 v2 | (any (\x -> onBoth x (name v1) (name v2)) matrices) = (CNet vars matrices)
                                    | otherwise = (CNet vars ((knock (createMatrix v1 v2) (name v1) (name v2)  (map (\x -> (x, x)) (intersect [(domain v1), (domain v2)])) ):matrices))

-- add a variable to a constraint network
addVariable :: Variable -> CNet -> CNet
addVariable v (CNet vars matrices) = (CNet (v:vars) matrices)

-- checks if a matrix is on a specific variable
isOn :: CMatrix -> [Char] -> Bool
isOn x n = (any (\y -> (y == n)) (names x))

-- checks if a matrix is on two variables
onBoth :: CMatrix -> [Char] -> [Char] -> Bool
onBoth x n1 n2 = (isOn x n1) && (isOn x n2)

getMatrix :: CNet -> [Char] -> [Char] -> CMatrix
getMatrix (CNet v m) l r = (filter (\p -> onBoth p l r) m)!!0

-- given one var name, get the other from a constraint
otherVar :: CMatrix -> [Char] -> [Char]
otherVar (CMatrix l r g) v | v == l = r
                           | v == r = l

-- lookup, but without the maybe.  just do it.
look a b | isJust (lookup a b) = fromJust (lookup a b)
