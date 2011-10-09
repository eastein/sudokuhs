-- Eric Stein
-- CS534 Project
-- Main code
--
module Sudoku where
import CSPL
import Helpers

process s = let lines = (metasplit "#" (Helpers.splitl (filter (\x -> not (x == '\r')) s)))
		sz = (toint ((lines!!0)!!0))
		in
		(sz, sz * sz, map tokenize (lines!!1))


-- sort the states into rows and columns, pad to make sure the digits line up, and print
sudokuboard c d = let l = sort c
		      n = (biggest (map (\(n, e) -> length e) c)) + 1
		      pl = insert d "\n" (map (\(t, e) -> pad n e) l)
		      in
		      foldl (++) [] pl

biggest l = foldl (max) 0 l

-- if the board was solved, print it.
results a (n, nq, l) = if (complete a) then
		do
			putStr "Solved:\n"
			putStr (sudokuboard (finalState a) nq)
		else
		do
			putStr "Could not resolve the constraints.  End."

main = do
        putStr "Input file: "
        ifile <- getLine
        s <- readFile ifile

	putStr "\nprocessing...\n\n"
	--read in the file and run the backtracking algorithm; pass results to the results printer method
	(results (backTracking (sudokuNet (process s)) (\m -> True)) (process s))
	putStr "\n\n"
