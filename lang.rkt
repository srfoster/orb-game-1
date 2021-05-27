#lang at-exp racket

(provide spawn
         respawn
         log!
         logs
         distance
         force
         force-to
         anchor
         de-anchor
         locate
         velocity
         with-name
         color
         let
         green
         red
         blue
         orange

         orb-spawn-location
         
         radial-force

         find-all-nearby)

(require unreal
         unreal/libs/names
         unreal/libs/actors
         unreal/libs/physics
         unreal/libs/basic-types)


;Spawn module:
;  Orbs can spawn and respawn at a location settable by the orb-spawn-location parameter

(define orb-spawn-location
  (make-parameter (hash 'X 0 'Y 0 'Z 0)))

(define/contract (spawn twitch-id)
  (-> string? unreal-value?)
  
  (define js
    @unreal-value{
 var Spawn = Root.ResolveClass('PickupMini');
 var spawn = new Spawn(GWorld, @(->unreal-value (orb-spawn-location)));
 spawn.SetText(@(->unreal-value twitch-id))
 
 return spawn;
 })
  
  (with-name twitch-id js))

(define (respawn spawn)
  @unreal-value{
 var spawn = @(->unreal-value spawn);
 spawn.SetActorLocation(@(->unreal-value (orb-spawn-location)));
 return spawn;
 })

;End Spawn module


;Color module

(define green
  "green")
(define blue
  "blue")
(define red
  "red")
(define orange
  "orange")

; col = red, green, blue, orange OR hex-value
(define (color spawn col)
  (define (hex->vec c)
    (define rs (substring c 1 3))
    (define gs (substring c 3 5))
    (define bs (substring c 5 7))
    (define r (read (open-input-string (~a "#x" rs))))
    (define g (read (open-input-string (~a "#x" gs))))
    (define b (read (open-input-string (~a "#x" bs))))
    (define r-dec (/ r 256))
    (define g-dec (/ g 256))
    (define b-dec (/ b 256))
    (hash 'X r-dec 'Y g-dec 'Z b-dec))
  (define color-vec 
    (match col
      ["green" (hash 'X 0 'Y 1 'Z 0)]
      ["blue" (hash 'X 0 'Y 0 'Z 1)]
      ["red" (hash 'X 1 'Y 0 'Z 0)]
      ["orange" (hash 'X 1 'Y 0.5 'Z 0)]
      [else (hex->vec col)]))
  @unreal-value{
 var spawn = @(->unreal-value spawn);
 spawn.ChangeColor(@(->unreal-value color-vec));
 return spawn;
 })

;End color module


;Forces and anchor module

(define/contract (force-to spawn name-or-location mag)
  (-> any/c (or/c string? hash?) number? unreal-value?)
  
  (define unreal-location
    (if (string? name-or-location)
        @unreal-value{
          var obj = @(with-name name-or-location);
          if(obj == undefined) return null 
          return obj.GetActorLocation();
        }
        (->unreal-value name-or-location)))

  @unreal-value{
 var spawn = @(->unreal-value spawn);
 var spawnCoords = spawn.GetActorLocation();

 var objCoords = @(->unreal-value unreal-location) 
 var vect = {X: (objCoords.X - spawnCoords.X),
  Y: (objCoords.Y - spawnCoords.Y),
  Z: (objCoords.Z - spawnCoords.Z)};
 var magnitude = Math.sqrt(Math.pow(vect.X,2) +
 Math.pow(vect.Y,2) +
 Math.pow(vect.X,2))
 
 if(magnitude == 0) return null
 
 var vect2 = {X: (vect.X / magnitude) * @mag,
  Y: (vect.Y / magnitude) * @mag,
  Z: (vect.Z / magnitude) * @mag};
 spawn.AddForce(vect2);
 })


(define/contract (anchor spawn name-or-ref)
  (-> any/c any/c unreal-value?)
  
  @unreal-value{
 var spawn = @(->unreal-value spawn);
 var obj = @(if (string? name-or-ref) 
                (with-name name-or-ref)
                (->unreal-value name-or-ref));
 spawn.AttachTo(obj);
 })


(define/contract (de-anchor spawn)
  (-> any/c unreal-value?)
  
  @unreal-value{
 var spawn = @(->unreal-value spawn);
 spawn.DetachAll();
 })

;End forces and anchor module


;Perception module

(define (find-all-nearby spawn)
  @unreal-value{
 var spawn = @(->unreal-value spawn);
 var found = spawn.FindNearby().OutActors;
    return found; 
  })

;End perception module


;Logs module
;  Separates logs by spawn, via a hash

(define current-logs (hash))

(define (get-logs s)
  (hash-ref current-logs s '()))

(define (add-logs s l)
  (define safe-take (lambda (l n)
                       (if (>= (length l) n)
                         (take l n)
                         l)))

  (set-logs! s (safe-take 
                 (cons l (get-logs s))
                 10)))

(define (set-logs! s l)
  (set! current-logs (hash-set current-logs s l)))

;Takes a spawn (unreal actor)
(define (log! s something)
  (add-logs (hash-ref s 'id) something))

;Takes a spawn (unreal actor)
(define (logs s)
  (get-logs (hash-ref s 'id)))

;End Logs module