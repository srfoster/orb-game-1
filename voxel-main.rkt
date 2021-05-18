#lang racket/base

(require twitch-bot
         unreal
         racket/runtime-path
         orb-game-1/lang)


(define-runtime-path Build "Build")

(define (prep-for-chat-output v)
  (if (unreal-actor? v)
      "[Unreal Actor]" ;Snip it.  Too long
      v))

(module+ main
  #;
  (bootstrap-and-start-unreal)


  (define e 
    (make-safe-evaluator 'orb-game-1/chat))
  
  (start-twitch-bot
   (handle-twitch-message
    (lambda (expr)
      (define evaled
        ((use-evaluator e) expr))
      
      (define ret
        (prep-for-chat-output
         evaled))
      
      ret))))