#lang racket

(require unreal
         unreal/libs/basic-types
         unreal/libs/actors
         (prefix-in unreal: orb-game-1/lang)
         racket/generator
         )

;Anything provided out of here is useable in spells.
;  Use caution!
(provide 
 generator
 
 displayln
 with-spawn
 self
 with-args
 
 args
 inputs
 #%top
 #%module-begin
 #%top-interaction
 #%datum

 ;Orb API
 force
 force-to
 anchor
 de-anchor
 locate
 velocity
 random
 color
 find-all-nearby
 wait-for-ticks ;what does this do?
 (rename-out [unreal:distance distance]
             [unreal:red red]
             [unreal:blue blue]
             [unreal:orange orange]
             [unreal:green green]
             [unreal:with-name with-name])
 log!
 
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
 set!

;Vector Helper Functions
 vec
 +vec
 *vec

;Void
 void?

;Logical Operators (would normally include OR and NOT)
 not

;Equality
 equal?
 string=?
 string-contains? ;should be in another section about matching?
 eq?

 ;Comparison Operators
 >=
 <=
 =
 <
 >

 ;Mathematical Operators
 +
 -
 *
 /
 
 ;Math Helper Functions
 positive?
 negative?
 round
 min
 max
 abs
 add1
 modulo
 
 ;Hash Helper Functions
 hash
 hash-ref
 hash-keys
 hash-values
 hash-has-key?
 
 ;List Helper Functions
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

 ;Higher Order Functions
 filter 
 map
 findf
 foldl
 apply

;Conversion Functions
 string->number
 number->string
 )



(define spawn (make-parameter #f))
(define-syntax-rule (with-spawn m lines ...)
  (let ()
    (when (spawn)
      (error "You're not allowed to do that..."))
    
    (parameterize ([spawn m])
      lines ...)))


(define (self)
  (spawn))

(define args (make-parameter #f))
(define-syntax-rule (with-args a lines ...)
  (parameterize ([args a])
    lines ...))
    
(define-syntax inputs (make-rename-transformer #'args))

(define (wait-for-ticks n)
  (for ([i (in-range 0 n)])
    (yield 'wait-for-ticks)))

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


(define (log! something)
  (unreal:log! (spawn) something))
