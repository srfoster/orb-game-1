#lang at-exp racket/base

;This implements a mana system in which orbs:
;  1) regenerate mana at some number per tick
;  2) will wait and regenerate until they have enough mana to run a function

(provide
 (except-out (all-from-out orb-game-1/mana-system)
             update-mana!)
 (rename-out [my-#%app #%app]))

(require orb-game-1/mana-system
         unreal
         (only-in unreal/libs/actors destroy-actor)
         racket/format)

(define mana-per-function-app (make-parameter 1))
(define-syntax-rule (my-#%app f args ...)
  (let step-loop ()
    (define mana-cost-function
      (hash-ref (mana-cost-list) f #f))
    
    (define mana-cost
      (if (not mana-cost-function) 0
          (min 0 (- (#%app mana-cost-function args ...)))))
    
    (print-status-line mana-cost (~v '(f args ...)))
    ;Special things can be free.
    ;  But usually, we yield (especially for user-defined functions)
    (when (not (member f (white-list)))
      ;When we yeild, a tick happens.  So regenerate mana here.
      (yield 'f)
      (when (is-dead? (orb))
        (raise-user-error "Spell target has died.  Stopping."))
      
      (update-mana! (orb) (mana-per-function-app)))
    
    ;Detect if WILL be out of mana, loop if so... 
    ; Write some better unit tests!!
    
    (if (< (+ mana-cost (current-mana (orb))) 0) 
        (let () 
          (when (out-of-mana? (orb))
            (color "#000000")
            #;(raise-user-error (~a "Out of mana, can't run: " 'f)))
          
          ;Loop back, so we can regenerate mana and try again
          (when (not (member f (white-list)))
            (step-loop)))
        (let ()
          (when (negative? mana-cost)
            (update-mana! (orb) mana-cost))
          
          (#%app f args ...)))
    
    ))

(module+ test 
  (my-#%app displayln "Hello world"))