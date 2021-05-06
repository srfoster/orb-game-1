#lang racket/base

(require unreal
         (prefix-in unreal: orb-game-1/lang)
         racket/generator
         racket/format
         racket/match
         racket/list)

(provide 
 generator
 
 with-spawn
 with-args
 args
 (rename-out [my-#%app #%app])
 #%top
 #%module-begin
 #%top-interaction
 #%datum
 
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
 displayln
 equal?
 string=?
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
 last
 rest
 empty?
 list-ref
 min
 max
 abs
 add1
 modulo
 string->number
 number->string
 void?
 
 ;Level functions
 (rename-out [unreal:blue-gate-location blue-gate-location]
             [unreal:red-gate-location red-gate-location])

 )

(define white-list
  (list 
   unreal:blue-gate-location 
   unreal:red-gate-location 
   not
   displayln
   equal?
   string=?
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
   last
   rest
   list-ref
   empty?
   min
   max
   abs
   add1
   modulo
   string->number
   number->string
   void?

   unreal:locate
   unreal:velocity
   unreal:distance))

(define spawn (make-parameter #f))
(define-syntax-rule (with-spawn m lines ...)
  (let ()
    (when (spawn)
      (error "You're not allowed to do that..."))
    (parameterize ([spawn m])
      lines ...)))

(define args (make-parameter #f))
(define-syntax-rule (with-args a lines ...)
  (parameterize ([args a])
    lines ...))

(define-syntax-rule (my-#%app f args ...)
  (let ()
    ;(displayln (~a "    Calling " 'f))
    (when (not (member f white-list))
      ;Special things can be free.
      ;But what if user redefines things like (random)?
      ;(displayln (~a "    Yielding " 'f))
      (yield 'f))
    (#%app f args ...)))

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

(define (velocity obj)
  (unreal-eval-js
   (unreal:velocity obj)))

(define (color col)
  (unreal-eval-js 
   (unreal:color (spawn) col)))