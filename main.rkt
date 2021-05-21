#lang racket/base
;taskkill" /T /IM "node" /F

(provide bootstrap-and-start-unreal
         bootstrap-and-start-unreal-voxels)

(require twitch-bot
         unreal
         racket/runtime-path)

(define-runtime-path Build "Build")
(define-runtime-path BuildVoxels "BuildVoxels")

(define (bootstrap-and-start-unreal)
  (bootstrap-unreal-js  
   (build-path Build "WindowsNoEditor\\OrbGames\\Content\\Scripts"))
  
  (start-unreal 
   (build-path Build "WindowsNoEditor\\OrbGames.exe")))

(define (bootstrap-and-start-unreal-voxels)
  (bootstrap-unreal-js  
   (build-path BuildVoxels "WindowsNoEditor\\Voxels\\Content\\Scripts"))
  
  (start-unreal 
   (build-path BuildVoxels "WindowsNoEditor\\Voxels.exe")))

(define (prep-for-chat-output v)
  (if (unreal-actor? v)
      "[Unreal Actor]" ;Snip it.  Too long
      v))

(module+ main
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