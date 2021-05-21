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
         red-gate-location
         blue-gate-location
         orb-spawn-location
         
         radial-force
         start-game

         find-all-nearby
         
         #%app
         #%top
         #%module-begin
         #%top-interaction
         #%datum)

(require unreal
         unreal/libs/names)

(define (red-gate-location)
  (hash 'X 0
        'Y 0
        'Z -500))

(define (blue-gate-location)
  (hash 'X 0
        'Y 0
        'Z 500))


(define orb-spawn-location
  (make-parameter (hash 'X 0 'Y 0 'Z 0)))

(orb-spawn-location (hash 'X -420.0 'Y 2210.0 'Z 4973.177246))

; (unreal-eval-js (spawn "Hello"))
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

(define game-loop #f)
(define number-of-minis #f)
(define (start-game [n 100]
                    [strength 1000])
  (when game-loop
    (kill-thread game-loop))

  (set! number-of-minis n)
  
  (for ([i (in-range number-of-minis)])
    (define s (unreal-eval-js (spawn (~a i))))
    (unreal-eval-js (color s "green"))
    s)
  
  (set! game-loop
        (thread 
         (thunk
          (let loop ()
            (displayln "Moving...")
            (define to-move (with-name (~a (random number-of-minis))))
            (unreal-eval-js (color to-move "orange"))
            (unreal-eval-js 
             (force to-move 
                    (random (- strength) strength)
                    (random (- strength) strength)
                    (random (- strength) strength)))
            (sleep 0.1)
            (unreal-eval-js (color to-move "green"))
            (loop))))))

#;
(define (end-game)
  (when game-loop
    (kill-thread game-loop))

  (for ([i (in-range number-of-minis)])
    (unreal-eval-js (destroy-actor (with-name (~a i))))
    s)


  )

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
 console.log("Color change")
 var spawn = @(->unreal-value spawn);
 if(!spawn){
  console.log(Object.keys(global.namedThings))
  console.log(GWorld.GetAllActorsOfClass(Actor).OutActors.length)
 }
 spawn.ChangeColor(@(->unreal-value color-vec));
 return spawn;
 })

;(unreal-eval-js (radial-force (hash 'X 0 'Y 0 'Z 0)))
(define (radial-force radius force-strength )
  @unreal-value{
 var r = new RadialForceActor(GWorld)

 r.ForceComponent.ForceStrength = @(->unreal-value force-strength)
 r.ForceComponent.Radius = @(->unreal-value radius)

 return r
 })

(define/contract (force spawn x y z)
  (-> any/c number? number? number? unreal-value?)
  
  @unreal-value{
 var spawn = @(->unreal-value spawn);
 var scm = spawn.StaticMeshComponent
 scm.AddImpulse({X:@x,Y:@y,Z:@z})
 
 return true
 })

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


(define/contract (distance a b)
  (-> hash? hash? number?)
  
  (define x1 (hash-ref a 'X))
  (define y1 (hash-ref a 'Y))
  (define z1 (hash-ref a 'Z))
  (define x2 (hash-ref b 'X))
  (define y2 (hash-ref b 'Y))
  (define z2 (hash-ref b 'Z))
  (sqrt (+ (sqr (- x1 x2))
           (sqr (- y1 y2))
           (sqr (- z1 z2)))))

(define (find-all-nearby spawn)
  @unreal-value{
 var spawn = @(->unreal-value spawn);
 var found = spawn.FindNearby().OutActors;
 console.log("FOUND!!!! ", found);
    return found; 
  })

(define/contract (de-anchor spawn)
  (-> any/c unreal-value?)
  
  @unreal-value{
 var spawn = @(->unreal-value spawn);
 spawn.DetachAll();
 })

(define green
  "green")
(define blue
  "blue")
(define red
  "red")
(define orange
  "orange")

(define (get-all-actors)
  @unreal-value{
    return GWorld.GetAllActorsOfClass(Actor).OutActors
 })
 
(define (camera)
  @unreal-value{
 return GWorld.GetAllActorsOfClass(CameraActor).OutActors[0]
 })



;;; TODO: move to unreal package

(define (velocity a)
  @unreal-value{
    return @(->unreal-value a).GetVelocity()
  })

(define (set-location a l)
  @unreal-value{
 var a = @(->unreal-value a)
 var l = @(->unreal-value l)
 
 a.SetActorLocation(l) 

 return a
 })

(define/contract (locate obj)
  (-> any/c unreal-value?)
  
  @unreal-value{
 var obj = @(->unreal-value obj);
 return obj.GetActorLocation();
 })

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