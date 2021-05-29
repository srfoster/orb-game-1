#lang at-exp racket/base

(provide
 ;Mana functions
 waste-mana
 my-mana
 assert-mana
 with-spawn
 split 
 eat
 is-dead?
 orb

 mana-cost-list
 print-status-line
 white-list
 yield
 generator

 out-of-mana?

 update-mana!
 current-mana
 get-starting-mana

 (all-from-out orb-game-1/run-lang))

(require 
  unreal
  (except-in unreal/libs/actors locate velocity)
  (prefix-in unreal: orb-game-1/lang)
  (except-in orb-game-1/run-lang with-spawn)
  (prefix-in run-lang: orb-game-1/run-lang)
  racket/function
  racket/generator
  racket/format
  )

(require unreal/external-runtime/main)

(define spawn (make-parameter #f))
(define-syntax-rule (with-spawn m lines ...)
  (let ()
    (parameterize ([spawn m]) 
      (when (not (hash-has-key? manas (get-id-if-actor m)))
        (set-mana! m (starting-mana)))
      
      (run-lang:with-spawn m lines ...))))

(define (orb)
  (spawn))

(define (white-list)
  (append
    (list
      split
      eat
      my-mana
      assert-mana)
    (run-lang:white-list)))

(define (run-lang:white-list)
  ;These are free functions.  They should not cost mana and do not cost a tick
  (list 
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
   
   unreal:distance
   log!
   self
   locate
   velocity
   find-all-nearby 
   
   +vec
   vec
   *vec
   
   wait-for-ticks

   ))

;BEGIN MANA MODULE

(define starting-mana (make-parameter 1000))
(define (get-starting-mana) ;Don't provide parameters you don't want users to set! :)
  (starting-mana))

(define (mana-cost-list)
  (hash split (lambda (m [dummy1 #f] [dummy2 #f]) m)
        eat   (lambda (target mana) 0)
        waste-mana (lambda (m) m)
        force      (lambda (x y z)
                     (define m 
                       (sqrt (+ (* x x) (* y y) (* z z)))) 
                     (/ m 200))
        force-to   (lambda (n f) (/ f 20))
        anchor     (thunk* 10)
        de-anchor  (thunk* 10)
        color      (thunk* 1)))

(define (waste-mana m)
  ;Do nothing.  The mana-cost-list defines the cost of this noop
  (void))

(define manas (hash))

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

(define (has-mana? s)
  (hash-has-key? manas (get-id-if-actor s)))

(define (out-of-mana? s)
  (<= (current-mana s) 0))

(define (current-mana s)
  (hash-ref manas (get-id-if-actor s)))

(define (my-mana)
  (current-mana (spawn)))

(define (assert-mana f m)
  (when (not (f (my-mana) m))
    (raise-user-error (~a "Mana was not " (list f (my-mana) m)))))

(define (print-status-line mana-cost code)
  (display
   (~a " Mana: " (round (current-mana (spawn))) " (" (if (< mana-cost -1) "" "+") (round (+ mana-cost 1)) ")\033[1E"
       code "\033[2E")))


; ORB MITOSIS 
; Takes amount of mana and code for the new baby orb. Mana is removed
; from momma orb.
(define (split mana 
               [code '(let () "Do nothing")] 
               [args '()])
  (define child-name
    ;Need to fix this! We will likely end up with
    ; naming collisions :(
    (~a "child" (random 10000)))
  (define ran-distance 100)
  
  (define loc
    (+vec (vec (random (- ran-distance) ran-distance) 
               (random (- ran-distance) ran-distance) 
               (random (- ran-distance) ran-distance)) 
          (locate (self))))

  (define spawned
    (unreal-eval-js 
     @unreal-value{
 var Spawn = Root.ResolveClass('PickupMini');
 var spawn = new Spawn(GWorld, @(->unreal-value loc));
 spawn.SetText("");
 
 return spawn;
 }))

  (add-spawn! child-name spawned)
  (run-spell child-name code args)
  (set-mana! spawned mana) ; Is there a race condition here if run-spell's thread starts ticking the orb before the mana is set?

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
    (when (<= (current-mana target) 0)
      (unreal-eval-js (destroy-actor target))) 
    

    ;Add back lost mana from (eat mana ...)
    ;(update-mana! (self) mana)
    
    ;Add in mana from target
    (update-mana! (self) (min mana consumed-mana))
    
    ;Return code and args...
    (current-code-for target)))

;END MANA MODULE

(define (get-id-if-actor s)
  (if (hash? s)
      (let ()
        (when (not (hash-has-key? s 'id))
          (raise-user-error "Can't get-id-if-actor of a hash that isn't an actor" s))
        (hash-ref s 'id))
      s))

(define (is-dead? a)
  (unreal-eval-js
  @unreal-value{
 var spawn = @(->unreal-value a);
    return !spawn
  }))