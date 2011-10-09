#HOWTO

This program should work with make and ghci on Unix/Linux.  To use:

* make sudoku
* ./sudoku
* enter 'main' to run the program
* type the filename of the input to read and return
 * tests are in the stests/ directory
* repeat the above 2 steps as needed to check multiple problems

#Approach

The adaptation of my general purpose CSPL solver was not very difficult.  I needed 2 new elements: code to parse sudoku initial state files into constraint nets & code to print out the final state data from backTracking() as a board.

pseudocode for these two elements:

    parse-input-file FILE {
      factor = firstline
      boardarray = tokenize(rest lines)
      net = create-unconstrained-network(grid-variables-generate(factor * factor))
      //generate binary not equal constraints over each row
      net = add-row-constraints(net, factor * factor)
      //generate binary not equal constraints over each column
      net = add-column-constraints(net, factor * factor)
      //generate binary not equal constraints over each block
      net = add-block-constraints(net, factor * factor)
      
      //at this point, the network is a generalize sudoku problem in CSP form, but has no initial state.
    
      //assign the variables set in the test file
      net = assign-values(net, boardarray)
      return net
    }

    print-the-board RESULTS dim {
      if (complete RESULTS) {
        //the backtracking algorithm was successful, print out the board!
    
        //example variable/value pair: ("1-1", "7")
        sort-by-first-item RESULTS
    
        print "Solved:\n"
    
        //the values can now be printed out in rows
        while RESULTS not empty {
          thisline = take dim RESULTS
          print thisline + "\n"
        }
      } else {
        print "Could not resolve the constraints.  End.\n"
      }
}
