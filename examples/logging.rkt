#lang at-exp racket/base

(require 
  unreal
  orb-game-1
  orb-game-1/chat
  orb-game-1/runner/main
  )

;;(bootstrap-and-start-unreal)

(with-twitch-id "test1"
  (spawn))

;Testing regeneration...
(run-spell "test1"
           '(let ()
              (log! "Hello")
              (log! (find-all-nearby))
              (log! "Bye")
              )
           '())

(sleep 1)

(displayln 
 (with-twitch-id "test1" 
   (logs)))

(sleep 10000)