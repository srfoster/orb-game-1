#lang at-exp racket/base

(provide
 (except-out (all-from-out orb-game-1/mana-system)
             update-mana!)
 (rename-out [my-#%app #%app]))

(require orb-game-1/mana-system
         unreal
         (only-in unreal/libs/actors destroy-actor scale)
         racket/format)

 (define-syntax-rule (my-#%app f args ...)
  (let step-loop ()
    (define mana-cost-function
      (hash-ref (mana-cost-list) f #f))
    
    (define mana-cost
       (if (not mana-cost-function) 0
          ; Makes sure we never gain mana, always negative mana:
           (min 0 (- (#%app mana-cost-function args ...)))))
    
;    (print-status-line mana-cost (~v '(f args ...)))
     
    ;Special things can be free.
    ;  But usually, we yield (especially for user-defined functions)
    (when (not (member f (white-list)))
      ;When we yeild, a tick happens.  So regenerate mana here.
      (yield 'f)
      ;(displayln `(f args ...))
      ;(displayln (current-mana (orb)))
      
      (define s (/ (current-mana (orb))
                   1000
                   ;(get-starting-mana) ;split overrides this...
                   ))

      (unreal-eval-js (scale (orb)
                             (vec s s s)))
      #;
      (when (is-dead? (orb)) ;Inefficient?  Sends a message to unreal on every function application
        (raise-user-error "Spell target has died.  Stopping."))
        )
    
    (update-mana! (orb) mana-cost)
    
    ;If orb has 0 or less mana, orb should die.
      ;Should not run function
      (if (eq? (current-mana (orb)) 0)
          (let ()
            (unreal-eval-js (destroy-actor (orb)))
          (raise-user-error "YOUR ORB DEAD"))
        (#%app f args ...))
    ))

(module+ test
 (my-#%app displayln "Hello World"))   