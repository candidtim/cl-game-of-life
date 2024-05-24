LISP ?= sbcl --noinform --non-interactive

all: run

run:
	$(LISP) \
		--eval '(asdf:load-system "cgl")' \
		--eval '(cgl/main:main 40 80 "gosper-glider-gun" :tick-duration-seconds 0 :fps 60 :max-generation 10000)'

test:
	$(LISP) --eval '(asdf:test-system "cgl")'


.PHONY: all run test
