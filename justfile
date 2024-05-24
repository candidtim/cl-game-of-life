LISP := "sbcl --noinform --non-interactive"

default: run

run HEIGHT='40' WIDTH='80' TICK='0' FIGURE='gosper-glider-gun' FPS='60' MAXGEN='10000':
	{{ LISP }} \
		--eval '(asdf:load-system "cgl")' \
		--eval '(cgl/main:main {{HEIGHT}} {{WIDTH}} "{{FIGURE}}" :tick-duration-seconds {{TICK}} :fps {{FPS}} :max-generation {{MAXGEN}})'

test:
	{{ LISP }} --eval '(asdf:test-system "cgl")'
