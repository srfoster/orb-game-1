#lang at-exp racket/base

(require unreal
         unreal/libs/basic-shapes
         unreal/libs/basic-types
         orb-game-1/runner/main
         orb-game-1)

(bootstrap-and-start-unreal)

(define spawn1 
  (unreal-eval-js (cube #:location (*vec 100 (vec 0 0 1)))))

(add-spawn! "test1" spawn1)

;TODO: Test run-spell with various languages
