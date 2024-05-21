LISP ?= sbcl --noinform --non-interactive

all: run

run:
	$(LISP) \
		--eval '(asdf:load-system "cgl")' \
		--eval '(cgl/main:main (list 40 80) (list :tick-duration-seconds 0.05))'

test:
	$(LISP) --eval '(asdf:test-system "cgl")'


.PHONY: all run test
