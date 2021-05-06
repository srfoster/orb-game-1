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

;Throwing an error should not interrupt other user's spells
(run-spell "test1"
  '(let loop ()
    (locate (with-name "adasdfaf"))   
    (loop))
  '())

(run-spell "test2"
  '(let loop ()
    (color "red")
    (color "blue")
    (loop))
  '())


(sleep 10000)