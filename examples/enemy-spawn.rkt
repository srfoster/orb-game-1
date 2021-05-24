#lang at-exp racket/base

(require
  unreal
  unreal/libs/actors
  orb-game-1
  orb-game-1/lang
  orb-game-1/runner/main
  racket/list
  )

(bootstrap-and-start-unreal)

(define behavior-list '(75 76))

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
  
  (run-spell (hash-ref other 'id)
             (get-spell (first (shuffle behavior-list)))
             '())

  (sleep 1)
  (loop))

#;
(with-twitch-id "test1"
  (spawn))

#;
(run-spell "test1"
           '(let ()
              (assert-mana = 10000)
              (waste-mana (my-mana)) ;Zero our mana.  Plus one to account for waste-mana taking 1 tick
              (assert-mana = 0)

              (let loop ()
                (waste-mana (my-mana))

                (color "blue")
                (assert-mana = 0)
                (color "red")
                (assert-mana = 0)
                (waste-mana 3)
                (loop)))
           '())
