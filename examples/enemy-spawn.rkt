#lang at-exp racket/base

(require orb-game-1/examples/enemy-spawn/main
         orb-game-1/runner/main
         racket/list)

;for each behavior, first in list is the spell id, followed by arguments
(define behavior-id-list '([198 200] 
                           [202]
                           #;[204 300 "#21FF02"]))

(displayln "Fetching behaviors from nexus")
(define behavior-list (map (lambda (id-and-args) 
                             (define id (first id-and-args))
                             (behavior (get-spell id) (rest id-and-args))) 
                           behavior-id-list))

(start-spawning behavior-list)