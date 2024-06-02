(defsystem "cgl"
  :description "Conway's Game of Life"
  :version "0.0.1"
  :author "candidtim"
  :license "Unlicense"
  :class :package-inferred-system
  :depends-on ("cgl/tui" "cgl/viz" "alexandria" "bordeaux-threads")
  :pathname "src"
  :in-order-to ((test-op (test-op "cgl-tests"))))
