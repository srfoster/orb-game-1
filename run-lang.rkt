#lang racket/base

(require unreal
         unreal/libs/basic-types
         unreal/libs/actors
         (prefix-in unreal: orb-game-1/lang)
         racket/generator
         racket/format
         racket/match
         racket/list
         racket/function
         racket/string)

;Anything provided out of here is useable in spells.
;  Use caution!
(provide 
 generator
 
 displayln
 with-spawn
 self
 with-args
 args
; .meta .jesscxc uncomment `inputs` (syntax transformer for `inputs` >>> `args`) (then del this message)
; inputs
 (rename-out [my-#%app #%app])
 #%top
 #%module-begin
 #%top-interaction
 #%datum
 
 split 
 eat
 force
 force-to
 anchor
 de-anchor
 locate
 velocity
 random
 color
 
 (rename-out [unreal:distance distance]
             [unreal:red red]
             [unreal:blue blue]
             [unreal:orange orange]
             [unreal:green green]
             [unreal:with-name with-name])
 
 ;Syntax
 set!
 let
 define
 lambda
 if
 cond
 else
 when
 match
 match-define
 quote
 quasiquote
 unquote
 and 
 or
 
 ;Functions
 not
 equal?
 string=?
 string-contains?
 eq?
 >=
 <=
 =
 <
 >
 +
 -
 *
 /
 round
 hash
 hash-ref
 hash-keys
 hash-values
 hash-has-key?
 positive?
 negative?
 list
 shuffle
 length
 first
 second
 third
 last
 rest
 empty?
 list?
 list-ref
 filter 
 map
 findf
 foldl
 apply
 min
 max
 abs
 add1
 modulo
 string->number
 number->string
 void?
 +vec
 vec
 *vec
 
 ;Level functions
 (rename-out [unreal:blue-gate-location blue-gate-location]
             [unreal:red-gate-location red-gate-location])
 
 find-all-nearby
 
 ;Mana functions
 waste-mana
 my-mana
 assert-mana
 
 log!
 wait-for-ticks
 )

(define starting-mana (make-parameter 10000))
(define mana-per-function-app (make-parameter 1))

(define (white-list)
  ;These are free functions.  They should not cost mana and do not cost a tick
  (list 
   unreal:blue-gate-location 
   unreal:red-gate-location 
   not
   equal?
   string=?
   string-contains?
   eq?
   >=
   <=
   =
   <
   >
   +
   -
   *
   /
   round
   hash
   hash-ref
   hash-keys
   hash-values
   hash-has-key?
   positive?
   negative?
   list
   shuffle
   random
   length
   first
   second
   third
   last
   rest
   list-ref
   filter
   map
   findf
   foldl
   apply
   empty?
   list?
   min
   max
   abs
   add1
   modulo
   string->number
   number->string
   void?
   
   split
   eat
   unreal:distance
   log!
   self
   locate
   velocity
   find-all-nearby 
   
   +vec
   vec
   *vec
   
   my-mana
   assert-mana
   
   wait-for-ticks
   ))


(define (mana-cost-list)
  (hash split (lambda (m [dummy1 #f] [dummy2 #f]) m)
        eat   (lambda (target mana) mana)
        waste-mana (lambda (m) m)
        force      (lambda (x y z)
                     (define m 
                       (sqrt (+ (* x x) (* y y) (* z z)))) 
                     (/ m 20))
        force-to   (lambda (n f) (/ f 20))
        anchor     (thunk* 10)
        de-anchor  (thunk* 10)
        color      (thunk* 1)))

(define (waste-mana m)
  ;Do nothing.  The mana-cost-list defines the cost of this noop
  (void))

(define manas (hash))

(define (get-id-if-actor s)
  (if (hash? s)
      (let ()
        (when (not (hash-has-key? s 'id))
            (raise-user-error "Can't get-id-if-actor of a hash that isn't an actor" s))
        (hash-ref s 'id))
      s))

(define (update-mana! s amount)
  (define id (get-id-if-actor s))
  (set! manas (hash-update manas 
                           id
                           (curry + amount)))
  (when (< (hash-ref manas id) 0)
    (set! manas (hash-set manas id 0))))

(define (set-mana! s amount)
  (define i (get-id-if-actor s))
  (set! manas (hash-set manas i amount)))


(define spawn (make-parameter #f))
(define-syntax-rule (with-spawn m lines ...)
  (let ()
    (when (spawn)
      (error "You're not allowed to do that..."))
    
    (when (not (hash-has-key? manas (get-id-if-actor m)))
      (set-mana! m (starting-mana)))
    
    (parameterize ([spawn m])
      lines ...)))

; ORB MITOSIS 
; Takes amount of mana and code for the new baby orb. Mana is removed
; from momma orb.
(define (split mana 
               [code '(let () "Do nothing")] 
               [args '()])
  (define add-spawn!
    (dynamic-require 'orb-game-1/runner/main 
                     'add-spawn!))
  (define run-spell
    (dynamic-require 'orb-game-1/runner/main 
                     'run-spell))
  (define child-name
    ;Need to fix this! We will likely end up with
    ; naming collisions :(
    (~a "child" (random 10000)))
  (define ran-distance 50)
  (define spawned
    (unreal-eval-js 
     (parameterize ([unreal:orb-spawn-location 
                     (+vec (vec (random (- ran-distance) ran-distance) 
                                (random (- ran-distance) ran-distance) 
                                (random (- ran-distance) ran-distance)) 
                           (locate (self)))]) 
       (unreal:spawn child-name))))
  (add-spawn! child-name spawned)
  (parameterize ([starting-mana mana]) 
    (run-spell child-name code args)) 
  
  spawned)

(define (can-eat? a b)
  (and
   (has-mana? a)  
   (has-mana? b)
   (not (equal? (get-id-if-actor a)
                (get-id-if-actor b)))))

(define (current-code-for s)
  '(todo))

(define (eat target mana)
  (when (can-eat? (self) target)
    (define consumed-mana (current-mana target))
    (displayln (~a "Gonna eat: " consumed-mana))
    
    ;Subtract mana from other (maybe killing it) 
    
    (update-mana! target (- mana))
    (when #t #;(<= (current-mana target) 0)
      (unreal-eval-js (destroy-actor target))) 
    
    ;Add back lost mana from (eat mana ...)
    (update-mana! (self) mana)
    
    ;Add in mana from target
    (update-mana! (self) consumed-mana)
    
    ;Return code and args...
    (current-code-for target)))

(define (self)
  (spawn))

(define args (make-parameter #f))
(define-syntax-rule (with-args a lines ...)
  (parameterize ([args a])
    lines ...))
; .meta .jesscxc uncomment to add syntax transformer for `inputs` >>> `args` (then del this message)
; (define-syntax inputs (make-rename-transformer #'args))

(define (has-mana? s)
  (hash-has-key? manas (get-id-if-actor s)))

(define (has-mana? s)
  (hash-has-key? manas (get-id-if-actor s)))

(define (out-of-mana? s)
  (<= (current-mana s) 0))

(define (current-mana s)
  (hash-ref manas (get-id-if-actor s)))

(define (wait-for-ticks n)
  (for ([i (in-range 0 n)])
    (yield 'wait-for-ticks)))

(define (my-mana)
  (current-mana (spawn)))

(define (assert-mana f m)
  (when (not (f (my-mana) m))
    (raise-user-error (~a "Mana was not " (list f (my-mana) m)))))

(define (print-status-line mana-cost code)
  (display
   (~a " Mana: " (round (current-mana (spawn))) " (" (if (< mana-cost -1) "" "+") (round (+ mana-cost 1)) ")\033[1E"
       code "\033[2E")))

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
      (when (is-dead? (spawn))
        (raise-user-error "Spell target has died.  Stopping."))

      (update-mana! (spawn) (mana-per-function-app)))
    
    ;Detect if WILL be out of mana, loop if so... 
    ; Write some better unit tests!!
    
    (if (< (+ mana-cost (current-mana (spawn))) 0) 
        (let () 
          (when (out-of-mana? (spawn))
            (color "black")
            #;(raise-user-error (~a "Out of mana, can't run: " 'f)))
          
          ;Loop back, so we can regenerate mana and try again
          (when (not (member f (white-list)))
            (step-loop)))
        (let ()
          (when (negative? mana-cost)
            (update-mana! (spawn) mana-cost))
          
          (#%app f args ...)))
    
    ))

(define (force x y z)
  (unreal-eval-js ;Do something fancy with #%top?
   (unreal:force (spawn) x y z)))

(define (force-to name mag)
  (unreal-eval-js
   (unreal:force-to (spawn) name mag)))

(define (anchor name)
  (unreal-eval-js
   (unreal:anchor (spawn) name)))

(define (de-anchor)
  (unreal-eval-js
   (unreal:de-anchor (spawn))))

(define (locate obj)
  (unreal-eval-js
   (unreal:locate obj)))

(define (find-all-nearby)
  (unreal-eval-js
   (unreal:find-all-nearby (spawn))))

(define (velocity obj)
  (unreal-eval-js
   (unreal:velocity obj)))

(define (color col)
  (unreal-eval-js 
   (unreal:color (spawn) col)))

(define (is-dead? x)
  (unreal-eval-js 
   (unreal:is-dead? x)))

(define (log! something)
  (unreal:log! (spawn) something))
