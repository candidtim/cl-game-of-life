(defsystem "cgl-tests"
  :description "Tests for 'cgl'"
  :author "candidtim"
  :license "Unlicense"
  :class :package-inferred-system
  :depends-on ("cgl" "cgl-tests/life" "rove")
  :pathname "tests"
  :perform (test-op (op c) (symbol-call :rove :run c)))
