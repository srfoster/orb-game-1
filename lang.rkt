#lang at-exp racket

(provide spawn
         respawn
         distance
         force
         force-to
         anchor
         de-anchor
         locate
         with-name
         color
         let
         green
         red
         blue
         orange

         start-game
         
         #%app
         #%top
         #%module-begin
         #%top-interaction
         #%datum)

(require unreal
         unreal/libs/names)

(define (red-gate)
  (->unreal-value
   (hash 'id "RedGate"
         'type "actor")))

(define (blue-gate)
  (->unreal-value
   (hash 'id "BlueGate"
         'type "actor")))

(define (main-light)
  (->unreal-value
   (hash 'id "PointLight3_7"
         'type "actor")))

(define (red-light)
  (->unreal-value
   (hash 'id "PointLight_1"
         'type "actor")))

(define (blue-light)
  (->unreal-value
   (hash 'id "PointLight2_4"
         'type "actor")))

; (unreal-eval-js (spawn "Hello"))
(define/contract (spawn twitch-id)
  (-> string? unreal-value?)
  
  (define js
    @unreal-value{
 var Spawn = Root.ResolveClass('PickupMini');
 var spawn = new Spawn(GWorld,{X:0,Y:0,Z:0});
 spawn.ChangeName(@(->unreal-value twitch-id))
 
 return spawn;
 })
  
  (with-name twitch-id js))

(define (respawn spawn)
  @unreal-value{
 var spawn = @(->unreal-value spawn);
 spawn.SetActorLocation({X: 0, Y: 0, Z: 0});
 return spawn;
 })

(define game-loop #f)
(define (start-game [number-of-minis 100])
  (when game-loop
     (kill-thread game-loop))
  
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
                    (random -10000 10000)
                    (random -10000 10000)
                    (random -10000 10000)))
            (sleep 0.1)
            (unreal-eval-js (color to-move "green"))
            (loop))))))

(define (color spawn col)
  @unreal-value{
 console.log("Color change")
 var spawn = @(->unreal-value spawn);
 if(!spawn){
  console.log(Object.keys(global.namedThings))
  console.log(GWorld.GetAllActorsOfClass(Actor).OutActors.length)
 }
 spawn.ChangeColor(ParticleSystem.Load("/Game/Orbs/" + @(~s (string-titlecase col)) + "Orb"));
 return spawn;
 })

;(unreal-eval-js (radial-force (hash 'X 0 'Y 0 'Z 0)))
(define (radial-force force-strength radius)
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

(define/contract (force-to spawn name mag)
  (-> any/c string? number? unreal-value?)
  
  @unreal-value{
 var spawn = @(->unreal-value spawn);
 var obj = @(with-name name);
 if(obj == undefined) return null
 
 var spawnCoords = spawn.GetActorLocation();
 var objCoords = obj.GetActorLocation();
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

(define/contract (anchor spawn name)
  (-> any/c string? unreal-value?)
  
  @unreal-value{
 var spawn = @(->unreal-value spawn);
 var obj = @(with-name name);
 spawn.AddChild(obj);
 })

(define/contract (locate obj)
  (-> any/c unreal-value?)
  
  @unreal-value{
 var obj = @(->unreal-value obj);
 return obj.GetActorLocation();
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

(define/contract (de-anchor spawn)
  (-> any/c unreal-value?)
  
  @unreal-value{
 var spawn = @(->unreal-value spawn);
 spawn.DetachChild();
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

(define (set-location a l)
  @unreal-value{
 var a = @(->unreal-value a)
 var l = @(->unreal-value l)
 
 a.SetActorLocation(l) 

 return a
 })