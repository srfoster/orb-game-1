
#lang racket/base

;Trying to factor out the runtime into something we could
;  move into another package.
;Ideally, we would even drop the assumptions about spells and spell server, etc.

(provide add-spawn!
         get-spawn
         get-spell
         run-spell
         get-errors
         seconds-between-ticks)


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
                     (displayln (~a "  Ticking for: " k))
                     (define p (hash-ref current-programs k))
                     
                     (with-handlers
                         ([exn? 
                           
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
                           (p)))
                   (hash-keys current-programs))
              
              ;(displayln "Ticked all programs.  Resting a bit.")
              (sleep (seconds-between-ticks))
              (loop))))))
  
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


(define (run-spell spawn-name code args)
  (setup-ns)
  
  (with-handlers
      ([exn? (lambda (e) (~a e))])
    
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

    (set! current-errors
          (hash-set current-errors
                    spawn-name
                    '()))

    ))

