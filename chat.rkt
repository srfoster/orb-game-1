#lang at-exp racket

(require unreal
         (prefix-in unreal: orb-game-1/lang)
         racket/generator
         orb-game-1/runner/main)

(provide #%module-begin
         #%top-interaction
         #%app
         #%datum
         with-twitch-id
         𐑩𐑯

         ;CodeSpells only
         start-game
         add-radial-force

         errors
         help
         topic
         spawn
         (rename-out [spawn mini])
         (rename-out [run cast])
         color
         force
         force-to
         de-anchor
         anchor
         locate
         velocity
         show-spell
         run
         (rename-out [unreal:red red]
                     [unreal:blue blue]
                     [unreal:orange orange]
                     [unreal:green green]))

(define (𐑩𐑯)
  "Nice Shavian, duddeeeeee~!")
#|
-First spawn: !!spawn, but not required (spawn if not spawned)
-Respawn:     !!reset or !!respawn
-Exit:        !!exit or !!despawn or !!die
|#

(define (between? num1 num2)
  (lambda (x)
    (and (number? x) (<= num1 x) (>= num2 x))))

(define current-twitch-id (make-parameter #f))

(define-syntax-rule (with-twitch-id id lines ...)
  (begin
    (when (current-twitch-id)
       (raise-user-error "Twitch id already set."))
    (parameterize ([current-twitch-id id])
      lines ...)))

(define (topic) topic) ;No real meaning yet.  Just makes help work

(define (help . args)
  (cond
    [(empty? args)
     @~a{OhMyDog  Use "!!help [topic]" to get more info.  Available topics: spawn, help}]
    [(equal? (list help) args)
     @~a{OhMyDog Help is a command you use to learn the language I understand.  Try "!!help spawn"}]
    [(equal? (list spawn) args)
     @~a{OhMyDog spawn is a command you use to spawn your spawn into the world.  Try "!!spawn"}]
    [(equal? (list force) args)
     @~a{OhMyDog Force is a command you use to apply a.  Try "!!spawn"}]
 ))


(define (spawn)
  (if (get-spawn (current-twitch-id)) 
      (let ()
        (unreal-eval-js (unreal:respawn (get-spawn (current-twitch-id))))
        @~a{Respawning...!"})
      (let ()
        (define spawned
          (unreal-eval-js (unreal:spawn (current-twitch-id))))
        (add-spawn! (current-twitch-id) spawned)
        @~a{You spawned a spawn!"})))

(define/contract (force x y z)
  (-> (between? -10000 10000)
      (between? -10000 10000)
      (between? -10000 10000)
      string?)

  (if (not (get-spawn (current-twitch-id)))
      @~a{You don't have a spawn yet!}
      (let ()
        (unreal-eval-js
         (unreal:force (get-spawn (current-twitch-id))
                       x y z))
        @~a{May the force be with you...})))

(define/contract (force-to name mag)
  (-> string?
      (between? -10000 10000)
      string?)

  (cond [(not (get-spawn (current-twitch-id)))
         @~a{You don't have a spawn yet!"}]
        [(string=? name (current-twitch-id))
         @~a{You can only force towards other things.}]
        [else
         (let ()
           (unreal-eval-js
            (unreal:force-to (get-spawn (current-twitch-id))
                             name mag))
           @~a{May the force-to be with you...})]))

(define/contract (anchor name)
  (-> string?
      string?)

  (cond [(not (get-spawn (current-twitch-id)))
         @~a{You don't have a spawn yet!}]
        [(string=? name (current-twitch-id))
         @~a{You can only anchor to other things.}]
        [else
         (let ()
           (unreal-eval-js
            (unreal:anchor (get-spawn (current-twitch-id))
                           name))
           @~a{Never gonna let you go...})]))

(define (locate [name (current-twitch-id)])
  (let ()
    (unreal-eval-js
     (unreal:locate (unreal:with-name name)))))

(define (velocity [name (current-twitch-id)])
  (let ()
    (unreal-eval-js
     (unreal:velocity (unreal:with-name name)))))

(define/contract (de-anchor)
  (-> string?)

  (cond [(not (get-spawn (current-twitch-id)))
         @~a{You don't have a spawn yet!}]
        [else
         (let ()
           (unreal-eval-js
            (unreal:de-anchor (get-spawn (current-twitch-id))))
           @~a{De-anchoring...})]))

(define/contract (color col)
  (-> string?
      string?)

  (if (not (get-spawn (current-twitch-id)))
      @~a{You don't have a spawn yet!"}
      (let ()
        (displayln "Sending")
        (unreal-eval-js
         (unreal:color (get-spawn (current-twitch-id))
                       col))
        (displayln "Sent")
        @~a{Changing colors...})))

(define/contract (run spell-id . args)
  (->* (integer?) #:rest (listof any/c) string?)
  (seconds-between-ticks 0)

  (if (not (get-spawn (current-twitch-id)))
      @~a{You don't have a spawn yet!}
      (let ()
        (define code  (get-spell spell-id))
        (run-spell (current-twitch-id) code args) 
        @~a{Running your spell... @code})))

(define/contract (show-spell spell-id)
  (-> integer? string?)

  (if (not (get-spawn (current-twitch-id)))
      @~a{You don't have a spawn yet!}
      (let ()
        
        (define code
          (get-spell spell-id))

        @~a{This is your code: @code})
      ))

(define/contract (errors [twitch-id (current-twitch-id)])
  (-> list?)

  (map exn-message  (get-errors twitch-id)))

(define (start-game number-of-minis strength)
  (when (not (string=? "codespells" (current-twitch-id)))
    (error "Only codespells can start games"))
  
  (unreal:start-game number-of-minis strength)
  "Game started")

(define (add-radial-force radius force)
  (when (not (string=? "codespells" (current-twitch-id)))
    (error "Only codespells can start games"))
  
  (unreal-eval-js (unreal:radial-force radius force))

  "Made a radial force")