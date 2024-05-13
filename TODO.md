To do:

* Parse command-line arguments
* Accept figures as arguments, or allow injecting them otherwise
* Print from a separate thread
* Move test code to proper tests
* Fix figure parsing (doesn't support empty lines withing figyres)
* Distinguish between survided and recently born cells
* Stitch together the top-bottom and left-right edges (toroidal array)
  * Alternative strategies to deal with edges?
    * Stop spawning new life outside, but consider already live cells?
