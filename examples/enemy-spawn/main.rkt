#lang at-exp racket/base

;TODO:
;  abstract out the character location -> enemy spawn location function,
;  rate of spawn,
;  despawn policy,
;  etc.

(provide start-spawning
  (struct-out behavior))

(require 
  unreal
  unreal/libs/actors
  orb-game-1
  orb-game-1/lang
  orb-game-1/runner/main
  racket/list)

(struct behavior (code args))

(define (start-spawning behavior-list)
  (bootstrap-and-start-unreal-voxels)
  (use-unsafe-ns #t)

  (define character 
    (unreal-eval-js (find-actor ".*OrbCharacter.*")))

  (define (r)
    (random -100 100))

  (define (spawn-other-orb loc)
    @unreal-value{
        var Spawn = Root.ResolveClass('PickupMini');
        var spawn = new Spawn(GWorld, @(->unreal-value loc));
        spawn.SetText("")
    
        return spawn;
    })

  (let loop ()
    (define character-location 
        (unreal-eval-js (locate character)))
    
    (define loc
        (hash 'X (+ (r) (hash-ref character-location 'X))
            'Y (+ (r) (hash-ref character-location 'Y))
            'Z (+ (r) (hash-ref character-location 'Z))))  
    
    (define other (unreal-eval-js (spawn-other-orb loc)))
    
    (displayln other)
    
    (add-spawn! (hash-ref other 'id) other)
    
    (define b 
        (first (shuffle behavior-list)))
    
    (run-spell (hash-ref other 'id)
                (behavior-code b)
                (behavior-args b))

    (sleep 1)
    (loop))) 