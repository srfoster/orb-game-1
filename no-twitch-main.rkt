#lang racket/base

(provide bootstrap-and-start-unreal)

(require unreal
         racket/runtime-path)

(define-runtime-path Build "Build")

(define (bootstrap-and-start-unreal)
  (bootstrap-unreal-js  
   (build-path Build "WindowsNoEditor\\OrbGames\\Content\\Scripts"))
  
  (start-unreal 
   (build-path Build "WindowsNoEditor\\OrbGames.exe")))

(module+ main
  (bootstrap-and-start-unreal)
  
  (require 
    orb-game-1/chat 
    orb-game-1/runner/main)
  
  (with-twitch-id "test1"
    (spawn))
  
  ;Testing regeneration...
  (run-spell "test1"
             '(let ()
                (let loop ()
                  (color "blue")
                  (color "red")
                  (loop)))
             '())
  
  (let loop () 
    (sleep 1)
    (loop)))
