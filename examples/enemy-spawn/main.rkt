#lang at-exp racket/base

;TODO:
;  Stop/suspend de-spawned orbs (continuation??)

;  abstract out the character location -> enemy spawn location function,
;  rate of spawn,
;  despawn policy,
;  etc.

;  Orb consumption.
;  More orbanisms.  More wiki pages.

(provide start-spawning
         (struct-out behavior))

(require 
  unreal
  unreal/libs/actors
  unreal/libs/basic-types
  orb-game-1
  orb-game-1/lang
  orb-game-1/runner/main
  racket/list)

(struct behavior (code args))

(define (r)
  (random -1000 1000))

(define (random-vec)
  (vec (r) (r) (r)))

(define (spawn-other-orb loc)
  @unreal-value{
 var Spawn = Root.ResolveClass('PickupMini');
 var spawn = new Spawn(GWorld, @(->unreal-value loc));
 spawn.SetText("")
 
 return spawn;
 })

(define spawned '())

(define num-to-spawn 100)

(define (start-spawning behavior-list)
  (bootstrap-and-start-unreal-voxels)
  (use-unsafe-ns #t)
  
  (define character 
    (unreal-eval-js (find-actor ".*OrbCharacter.*")))
  
  (let loop ()
    (define character-location 
      (unreal-eval-js (locate character)))
    
    (define loc
      (+vec (random-vec)
            character-location))  
    
    (define dist-to-ground
      (unreal-eval-js
       @unreal-value{
 var l = @(->unreal-value loc)
 var hit = GWorld.LineTraceByChannel(l, {X: l.X, Y: l.Y, Z: l.Z - 10000}, ETraceTypeQuery.TraceTypeQuery1).OutHit
 return hit.Distance
 }))
    
    (when (not (void? dist-to-ground))
      (define other (unreal-eval-js (spawn-other-orb (+vec loc
                                                           (vec 0 0 (- 50 dist-to-ground))))))
      
      (set! spawned (cons other spawned))
      (when (> (length spawned) num-to-spawn)
        (define doomed (last spawned))
        (unreal-eval-js (destroy-actor doomed))
        (set! spawned (drop-right spawned 1)))
      
      (add-spawn! (hash-ref other 'id) other)
      
      (define b 
        (first (shuffle behavior-list)))
      
      (run-spell (hash-ref other 'id)
                 (behavior-code b)
                 (behavior-args b)))
    
    (sleep 1)
    (loop))) 