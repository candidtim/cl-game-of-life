# Conway's Game of Life in Common Lisp

An implementation of [Conway's Game of Life](https://en.wikipedia.org/wiki/Conway's_Game_of_Life)
in Common Lisp, mostly for fun and to have something to practice the Lisp
skills on.

At the moment not packaged in any way. To run it, load the `life.lisp` and
execute the `(play height width tick-duration)` function. Modify the `play`
function implementation to inject different figures at start.

Or run `./main` to play with some default parameters.
