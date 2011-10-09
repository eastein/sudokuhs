default: sudoku
	@echo "built all projects"

sudoku: CSPL.hs Sudoku.hs Helpers.hs
	ghc --make Sudoku
	echo '#!/bin/sh' > sudoku
	echo 'time ghci Sudoku' >> sudoku
	chmod +x sudoku

clean:
	rm -f *.o *.hi sudoku
