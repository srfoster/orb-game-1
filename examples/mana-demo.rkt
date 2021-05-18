#lang at-exp racket/base

(require 
  unreal
  orb-game-1
  orb-game-1/chat
  orb-game-1/runner/main
  )

(bootstrap-and-start-unreal)

(with-twitch-id "test1"
  (spawn))

;Testing regeneration...
(run-spell "test1"
           '(let ()
              (assert-mana = 10000)
              (waste-mana (my-mana)) ;Zero our mana.  Plus one to account for waste-mana taking 1 tick
              (assert-mana = 0)

              (let loop ()
                (waste-mana (my-mana))

                (color "blue")
                (assert-mana = 0)
                (color "red")
                (assert-mana = 0)
                (waste-mana 3)
                (loop)))
           '())

(sleep 10000)