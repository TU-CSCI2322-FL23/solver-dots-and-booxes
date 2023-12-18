# Dots and Booxes Game Solver
Created by: August, Liam, Choudhry, and Aashish

# File Descriptions:
Main.hs - Main function and IO functions  
DotsAndBoxes.hs - game itself  
Solver.hs - solver functions  
ReadAndShow.hs - contains all the read/show functions  
gamefiles - contains text files for different gamestates 

# Project Grade:         101/100
## Functionality               76/73
* Game mechanics:              20
  * Your game format is incredibly strict in that the *order of the lines* matters. This makes it
    very hard to create games to test with. 
* Exact game solver:           15
* Cut-off depth solver:        13
* Evaluation function:         2
* Avoiding unnecessary work:   3
* Command-line interface:      10
  * you don't output the move in the -m format by default. Outputs the verbose version.
* Move and verbose flags:      5
  * move flag works beautifully with verbose
  * interactive flag is great (+5)
  * verbose doesn't seem to affect other commands 
* Error-handling:              5
  * So close to perfect, but in main you call error on an invalid file!

## Design                      25/27
* Well-designed data types:    8
* Well-decomposed functions:   10
  * Overall very nice, only minor quibbles.
  * You seem very averse to local helper functions a lot of the time, which creates a bit of a flat
    decomposition. This is only really awkward around the reading/writing, especially when you have
    a plethora of very small functions that are used once.
  * readGame is easier if you let readMoves/etc return the empty list for empty string, or even
    special-case in an if: `mvs <- if null mvsStr then return [] else readMoves moveStr`
  * makeMove has two cases that are identical! You decomposed everything correctly and then left
    both in.
  * calculateScore could have one extra line and be checkWinner. At a minimum it should be a helper
    function.
  * main is a pyramid of DOOOM. I would have broken out everything after "Just game" into a seperate
    function.
  * findMoveFlag returns a list for some reason.
  * minimax should probably be in a where, that's kind of ugly.
* Good module decomposition:   2
  * Excellent!
* Good variable names:         2
* Efficient/idiomatic code:    5
