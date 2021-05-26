#lang at-exp racket/base

(require 
  unreal
  orb-game-1
  orb-game-1/chat
  orb-game-1/runner/main)

;(bootstrap-and-start-unreal-voxels)

(with-twitch-id "test1"
  (spawn))

(with-twitch-id "test2"
  (spawn))


(run-spell "test2"
           '(let loop ()
              ;(waste-mana (- (my-mana) 1000))
              (color
               (cond 
                 [(> (my-mana) 5000) "#00FF00"]
                 [(> (my-mana) 1000) "#FFFF00"]
                 [else "#FF0000"]))
              (loop))
           '())

;Throwing an error should not interrupt other user's spells
(run-spell "test1"
           '(let loop ()
              ;(color "blue")
              (define foods (find-all-nearby)) 
              ;(color "red")
              (map  
               (lambda (food)
                 (eat food 100))
               foods)
              (loop))
           '())

(sleep 10000)