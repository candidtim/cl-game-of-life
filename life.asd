(defsystem "life"
  :version "0.0.1"
  :author "candidtim"
  :license "MIT"
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description "Conway's Game of Life"
  :in-order-to ((test-op (test-op "life-tests"))))

(defsystem "life-tests"
  :author ""
  :license ""
  :depends-on ("life"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Tests for `life'"
  :perform (test-op (op c) (symbol-call :rove :run c)))
