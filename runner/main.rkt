
#lang racket/base

;Trying to factor out the runtime into something we could
;  move into another package.
;Ideally, we would even drop the assumptions about spells and spell server, etc.

(provide add-spawn!
         get-spawn
         get-spell
         run-spell
         get-errors
         seconds-between-ticks
         use-unsafe-ns ;Because I haven't figured out how to get the safe one to work after raco distribute
         )


(require racket/function
         racket/string
         racket/contract
         racket/format)

;Can't run programs unless you have a spawn.  A visualization of the "computer" running the programs.
;  TODO: Pivot to a better name than spawn.
(define current-spawns (hash))      
(define current-programs (hash))
(define current-errors (hash))

(define runner #f)
(define seconds-between-ticks (make-parameter 0.1))

(define (add-spawn! name spawn)
  (set! current-spawns (hash-set current-spawns name spawn)))

(define (get-spawn name)
  (hash-ref current-spawns name #f))

(define (get-errors name)
  (hash-ref current-errors name '()))


(define (handle-spell-error username e) 
    (if (program-stopped-working? e)
        (let ()
          (displayln "The program stopped working! Kill it!")
          (set! current-programs
                (hash-remove current-programs
                             username)))
        (let ()
          (displayln "Adding a new error now...")
          (displayln e) 
          (set! current-errors
                (hash-update current-errors
                             username
                             (lambda (es)
                               (cons e es))))))) 

(define (tick-program username)
  (displayln (~a "  Ticking for: " username))
  (define program (hash-ref current-programs username))
  
  (with-handlers
      ([exn? (curry handle-spell-error username)])
    ;User's program is implemented as a generator, so we 
    ; call it as a function to tick it
    (program)))

(define safe-ns #f)
;(dynamic-require 'orb-game-1/run-lang #f)
(require racket/runtime-path)
;Should really pass path in as a parameter, not hard coded
(define-runtime-path run-lang.rkt "../run-lang.rkt")  
(define (setup-ns)
  (define main-ns (current-namespace))
  (when (not safe-ns)
    ;Ugh.  This namespace stuff doesn't work after raco exe/dist :(
    (set! safe-ns
          (if (use-unsafe-ns)
              (let () 
                (dynamic-require 'orb-game-1/run-lang-external #f)
                (module->namespace 'orb-game-1/run-lang-external)
                )
              (parameterize ([current-namespace
                              (make-base-empty-namespace)])
                (displayln "Attaching unreal")
                (namespace-attach-module main-ns 'unreal (current-namespace))
                (namespace-attach-module main-ns 'orb-game-1/lang (current-namespace))
                
                (displayln "Requiring run-lang")
                (namespace-require
                 ;  run-lang.rkt
                 'orb-game-1/run-lang
                 )
                
                (current-namespace)))))
  )

(define (setup-ticking-thread)
  (when (not runner)
    (set! runner
          (thread
           (thunk
            (let tick ()
              (define usernames (hash-keys current-programs))
              (for-each tick-program usernames)
              
              ;(displayln "Ticked all programs.  Resting a bit.")
              (sleep (seconds-between-ticks))
              (tick)))))))

(define (setup-ns-and-ticking-thread)
  (displayln "Setting up run-lang namespace and ticker")
  (setup-ns)  
  (setup-ticking-thread)
  (displayln "run-lang and ticker setup complete"))

(define (program-stopped-working? e)
  (define m (exn-message e))
  (string-contains? m "cannot call a running generator"))


(define/contract (get-spell spell-id)
  (-> integer? list?)
  
  (local-require net/http-easy
                 json)
  
  (define id spell-id)
  
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

(define use-unsafe-ns (make-parameter #f))
(define (run-spell spawn-name code args)
  (setup-ns-and-ticking-thread)
    
  (set! current-errors
        (hash-set current-errors
                  spawn-name
                  '()))
  
  (with-handlers
      ([exn? (curry handle-spell-error spawn-name)])
    
    (displayln (~a "Construction program generator for: " spawn-name))
    (define program
      (eval
       `(generator ()
                   (with-args ',args
                     (with-spawn ,(hash-ref current-spawns spawn-name)
                       ,code)))
       safe-ns))
    (displayln (~a "Done constructing generator for: " spawn-name))
    
    (set! current-programs
          (hash-set current-programs
                    spawn-name 
                    program))


    ))

