# Conway's Game of Life in Common Lisp

An implementation of [Conway's Game of Life](https://en.wikipedia.org/wiki/Conway's_Game_of_Life)
in Common Lisp, mostly for fun and to have something to practice the Lisp skills on.


## Usage

Run with [just](https://github.com/casey/just):

    just run [HEIGHT=40] [WIDTH=80] [TICK=0] [FIGURE=gosper-glider-gun] [MAXGEN=10000]

or just load it:

    (ql:quickload "cgl")
    (cgl/main:main 40 80 "gosper-glider-gun"
                   :tick-duration-seconds 0
                   :max-generation 10000)
