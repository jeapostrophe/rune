ALL: main

main.icns: icons.rkt
	racket -t $^ > $@

~/Dev/scm/plt/racket/Rune.app: main.icns
	raco setup rune

main: ~/Dev/scm/plt/racket/Rune.app
	raco make main.rkt
	open ~/Dev/scm/plt/racket/Rune.app
