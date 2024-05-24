To do:

* UI with ncurses (cl-charms)
  * Let user inject figures
  * Distinguish between survided and recently born cells
* Fix figure parsing (doesn't support empty lines withing figyres)
* Stitch together the top-bottom and left-right edges (toroidal array)
  * Alternative strategies to deal with edges?
    * Stop spawning new life outside, but consider already live cells?
