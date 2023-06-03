#lang racket
#|
You need a Racket before you can launch a rocket, so the first thing you need to learn is how
to get and install DrRacket. (Which I've already done 🤓)

Now we'll be able to experiment. Racket is all about experimenting with expressions and
creating your programs from these experiments. After some quick demonstrations, we'll be ready
to write a game!
|#

;; I put a bunch of stuff in the interactions panel, works like a regular repl, no surprises
;; now here's the def panel:
(+ 3 (* 2 4)) ;; in this def panel, when you hit ctrl-r or F5 the result of what's in the def panel
;; is put into a refreshed interaction panel where you can continue to work with it. so you have
;; constanst interaction with your definitions and program
(define name "ladoblanco") ; after running this var is now avail in the interaction panel

'(hello world) ;; NOTE that we don't have to do (print '(hello world)). DrRacket does all that for me

;; racketeers using file names ending in .rkt. And a quick note from captain obvious, its called
;; the definitions panel, cuz we write down definitions  for vars and functions. So lets start
;; with our first game. in Chap2

;; CHAPTER CHECKPOINT
;; In this chapter we learned a few basics about racket and dr racket:
;;;; DrRacket is the PDE for Racket. It runs on most computing platforms. Both Racket and DrRacket
;;;;;; are available for free online at racket-lang.org as one package.
;;;; You can enter expressions and evaluate them in the interactions panel.
;;;; You can edit programs in the definitions panel and run them, and the interactions panel
;;;;;; shows what they compute to.