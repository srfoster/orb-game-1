#lang at-exp racket

(require unreal
         unreal/libs/names
         unreal/libs/actors
         racket/runtime-path)

(define-runtime-path here ".")

(bootstrap-unreal-js  
 (build-path here "Build\\WindowsNoEditor\\LogCabinWorld\\Content\\Scripts")
 )

(start-unreal 
 (build-path here "Build\\WindowsNoEditor\\CodeSpellsDemoWorld.exe"))

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

(define (spawn player-name gate color)
  
  (define js
    @unreal-value{
 var Spawn = Root.ResolveClass('PickupMini');
 var gate = @(->unreal-value gate)
 var spawn = new Spawn(GWorld,gate.GetActorLocation());
 spawn.ChangeName(@(->unreal-value player-name));
 spawn.ChangeColor(ParticleSystem.Load("/Game/Orbs/" + @(~s (string-titlecase color)) + "Orb"))
 
 return spawn;
 })
  
  (with-name player-name js))

(define/contract (force spawn x y z)
  (-> any/c number? number? number? unreal-value?)
  
  @unreal-value{
 var spawn = @(->unreal-value spawn);
 spawn.AddForce({X:@x,Y:@y,Z:@z})
 
 return true
 })

(define (spawn-neutral-minis [total 100])
  (define ids (range total))
  (define (spawn-in i)
    (unreal-eval-js
     (spawn (~a i)
            (main-light)
            "green"
            )))
  
  (map spawn-in ids))

(define (test [total 100]
              [rate 0.25]
              [f 200])
  (define ids (range total))
  (define (spawn-in i)
    (unreal-eval-js
     (spawn (~a i)
            (main-light)
            
            "green"
            )))

  (map spawn-in ids)

  (define count 0)
  (let loop ()
    (define n
      (first
       (shuffle
        ids)))

    (unreal-eval-js
     (force
      (with-name (~a n))
      (random (- f) f)
      (random (- f) f)
      (random (- f) f)))

    (unreal-eval-js
     (color
      (with-name (~a n))
      (first (shuffle
              '("red" "blue")))))


    
    (displayln (~a "tick: " count))
    (set! count (add1 count))
    (sleep rate)
    (loop)))

(define (color spawn col)
   @unreal-value{
 var spawn = @(->unreal-value spawn);
 spawn.ChangeColor(ParticleSystem.Load("/Game/Orbs/" + @(~s (string-titlecase col)) + "Orb"));
 return spawn;
 })

(define (force-to spawn target mag)

  @unreal-value{
 var spawn = @(->unreal-value spawn);
 var obj = @(->unreal-value target);
 var spawnCoords = spawn.GetActorLocation();
 var objCoords = obj.GetActorLocation();
 var vect = {X: (objCoords.X - spawnCoords.X),
             Y: (objCoords.Y - spawnCoords.Y),
             Z: (objCoords.Z - spawnCoords.Z)};
 var magnitude = Math.sqrt(Math.pow(vect.X,2) +
                           Math.pow(vect.Y,2) +
                           Math.pow(vect.X,2))
 var vect2 = {X: (vect.X / magnitude) * @mag,
              Y: (vect.Y / magnitude) * @mag,
              Z: (vect.Z / magnitude) * @mag};
 spawn.AddForce(vect2);
 })
