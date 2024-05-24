LISP := "sbcl --noinform --non-interactive"

default: run

run HEIGHT='40' WIDTH='80' TICK='0' FPS='60':
	{{ LISP }} \
		--eval '(asdf:load-system "cgl")' \
		--eval '(cgl/main:main (list {{HEIGHT}} {{WIDTH}}) (list :tick-duration-seconds {{TICK}} :fps {{FPS}} :generation-evolve-for 10000))'

test:
	{{ LISP }} --eval '(asdf:test-system "cgl")'
