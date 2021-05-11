#lang at-exp racket/base

(require 
  unreal
  orb-game-1
  orb-game-1/chat
  orb-game-1/runner/main)

(bootstrap-and-start-unreal)

(with-twitch-id "test1"
  (spawn))
(with-twitch-id "test2"
  (spawn))

;Spending mana means the spell should eventually stop
(run-spell "test1"
           '(let loop ()
              (color "red")
              (color "blue")
              (waste-mana 10)
              (loop))
           '())
(run-spell "test2"
           '(let loop ()
              (define (r) (random -1000 1000))
                (force (r) (r) (r))
                (loop))
           '())


(sleep 10000)