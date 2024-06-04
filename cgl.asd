(defsystem "cgl"
  :description "Conway's Game of Life"
  :version "0.0.1"
  :author "candidtim"
  :license "Unlicense"
  :class :package-inferred-system
  :depends-on ("cgl/main"
               "alexandria"
               "bordeaux-threads"
               "trivial-signal"
               "exit-hooks")
  :pathname "src"
  :in-order-to ((test-op (test-op "cgl-tests"))))
