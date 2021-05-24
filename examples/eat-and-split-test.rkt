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

;Throwing an error should not interrupt other user's spells
(run-spell "test1"
           '(let loop ()
              (color "blue")
              (define foods (find-all-nearby)) 
              (color "red")
              (map  
               (lambda (food)
                 (eat food 10))
               foods)
              (loop))
           '())

(run-spell "test2"
           '(let loop ()
              (waste-mana (- (my-mana) 1000))
              (loop))
           '())


(sleep 10000)