#lang at-exp racket/base

(require 
  unreal
  orb-game-1
  (only-in orb-game-1/chat with-twitch-id spawn logs)
  orb-game-1/runner/main)

(bootstrap-and-start-unreal)

(with-twitch-id "test1"
  (spawn))

;Should throw an error -- even though the unreal module is added to the namespace
(run-spell "test1"
           '(let loop ()
              (log! unreal-eval-js))
           '())

(sleep 1)

(displayln 
 (with-twitch-id "test1" 
   (logs)))

(sleep 10000)