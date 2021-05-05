#lang at-exp racket

(require unreal
         (prefix-in unreal: orb-game-1/lang)
         racket/generator
         )

(provide #%module-begin
         #%top-interaction
         #%app
         #%datum
         with-twitch-id
         𐑩𐑯

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
         show-spell
         run
         (rename-out [unreal:red red]
                     [unreal:blue blue]
                     [unreal:orange orange]
                     [unreal:green green])
         )

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
  ;TODO: Fail if current twitch id already set.
  ;  Security bug
  (parameterize ([current-twitch-id id])
    lines ...))

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

(define current-spawns (hash))
(define current-programs (hash))
(define current-errors (hash))

(define runner #f)

(define (spawn)
  (if (hash-has-key? current-spawns (current-twitch-id))
      (let ()
        (unreal-eval-js (unreal:respawn (hash-ref current-spawns (current-twitch-id))))
        @~a{Respawning...!"})
      (let ()
        (define spawned
          (unreal-eval-js (unreal:spawn (current-twitch-id))))
        (set! current-spawns (hash-set current-spawns (current-twitch-id) spawned))
        @~a{You spawned a spawn!"})))

(define/contract (force x y z)
  (-> (between? -10000 10000)
      (between? -10000 10000)
      (between? -10000 10000)
      string?)

  (if (not (hash-has-key? current-spawns (current-twitch-id)))
      @~a{You don't have a spawn yet!"}
      (let ()
        (unreal-eval-js
         (unreal:force (hash-ref current-spawns (current-twitch-id))
                       x y z))
        @~a{May the force be with you...})))

(define/contract (force-to name mag)
  (-> string?
      (between? -10000 10000)
      string?)

  (cond [(not (hash-has-key? current-spawns (current-twitch-id)))
         @~a{You don't have a spawn yet!"}]
        [(string=? name (current-twitch-id))
         @~a{You can only force towards other things.}]
        [else
         (let ()
           (unreal-eval-js
            (unreal:force-to (hash-ref current-spawns (current-twitch-id))
                             name mag))
           @~a{May the force-to be with you...})]))

(define/contract (anchor name)
  (-> string?
      string?)

  (cond [(not (hash-has-key? current-spawns (current-twitch-id)))
         @~a{You don't have a spawn yet!}]
        [(string=? name (current-twitch-id))
         @~a{You can only anchor to other things.}]
        [else
         (let ()
           (unreal-eval-js
            (unreal:anchor (hash-ref current-spawns (current-twitch-id))
                           name))
           @~a{Never gonna let you go...})]))

(define (locate [name (current-twitch-id)])
  (let ()
    (unreal-eval-js
     (unreal:locate (unreal:with-name name)))))

(define/contract (de-anchor)
  (-> string?)

  (cond [(not (hash-has-key? current-spawns (current-twitch-id)))
         @~a{You don't have a spawn yet!}]
        [else
         (let ()
           (unreal-eval-js
            (unreal:de-anchor (hash-ref current-spawns (current-twitch-id))))
           @~a{De-anchoring...})]))

(define/contract (color col)
  (-> string?
      string?)

  (if (not (hash-has-key? current-spawns (current-twitch-id)))
      @~a{You don't have a spawn yet!"}
      (let ()
        (displayln "Sending")
        (unreal-eval-js
         (unreal:color (hash-ref current-spawns (current-twitch-id))
                       col))
        (displayln "Sent")
        @~a{Changing colors...})))

(define (program-stopped-working? e)
  (define m (exn-message e))
  (string-contains? m "cannot call a running generator"))

(define safe-ns #f)
(dynamic-require 'orb-game-1/run-lang #f)
(define (setup-ns)
  (displayln "Setting up run-lang namespace and ticker")
  (define main-ns (current-namespace))
  (when (not safe-ns)
    (set! safe-ns
          (parameterize ([current-namespace
                          (make-base-empty-namespace)])
            (namespace-attach-module main-ns 'unreal (current-namespace))
            (namespace-require
             'orb-game-1/run-lang)
            
            (current-namespace))))

  (when (not runner)
    (set! runner
          (thread
           (thunk
            (let loop ()
              (map (lambda (k)
                     ;(displayln (~a "  Ticking for: " k))
                     (define p (hash-ref current-programs k))

                     (with-handlers
                         ([exn:fail?
                           (lambda (e)
                             (if (program-stopped-working? e)
                                 (let ()
                                   (displayln "The program stopped working! Kill it!")
                                   (set! current-programs
                                         (hash-remove current-programs
                                                      k)))
                                 (let ()
                                   (displayln "Adding a new error now...")
                                   (displayln e) 
                                   (set! current-errors
                                         (hash-update current-errors
                                                      k
                                                      (lambda (es)
                                                        (cons e es)))))))])
                       (p)
                       ))
                   (hash-keys current-programs))

              ;(displayln "Ticked all programs.  Resting a bit.")
              (sleep 0.1)
              (loop))))))
  
  (displayln "run-lang and ticker setup complete"))

(define/contract (get-spell spell-id)
  (-> integer? list?)
 
  (local-require net/http-easy
                 json)

  (define id spell-id)
  (define twitch-id (current-twitch-id))
  
  (define res
    (get
     (~a "https://guarded-mesa-01320.herokuapp.com/secret/"
         id)))
  (define payload
    (response-json res))
  (define code-string
    (~a
     "(let () "
     (hash-ref payload 'text)
     ")"))
  (define code
    (read (open-input-string code-string)))

  code)

(define/contract (run spell-id . args)
  (->* (integer?) #:rest (listof any/c) string?)
  (setup-ns)
  (if (not (hash-has-key? current-spawns (current-twitch-id)))
      @~a{You don't have a spawn yet!}
      (let ()
        (define code
          (get-spell spell-id))

        (with-handlers
            ([exn:fail? (lambda (e) (~a e))])

          (define program
            (eval
             `(generator ()
                         (with-args ',args
                             (with-spawn ,(hash-ref current-spawns (current-twitch-id))
                           ,code)))
             safe-ns))

          (set! current-programs
                (hash-set current-programs
                          (current-twitch-id)
                          program))
          (set! current-errors
                (hash-set current-errors
                          (current-twitch-id)
                          '()))
          
          @~a{Running your spell... @code}))))

(define/contract (show-spell spell-id)
  (-> integer? string?)

  (if (not (hash-has-key? current-spawns (current-twitch-id)))
      @~a{You don't have a spawn yet!}
      (let ()
        
        (define code
          (get-spell spell-id))

        @~a{This is your code: @code})
      ))

(define/contract (errors [twitch-id (current-twitch-id)])
  (-> list?)

  (map exn-message (hash-ref current-errors twitch-id '())))