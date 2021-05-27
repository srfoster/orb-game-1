#lang at-exp racket/base

(require
  unreal
  orb-game-1
  orb-game-1/chat)

(bootstrap-and-start-unreal)

(with-twitch-id "laurond"
  (spawn))

(with-twitch-id "stephenrfoster"
  (spawn))

(with-twitch-id "kenzo_25"
  (spawn))

(with-twitch-id "runiracc"
  (spawn))

(with-twitch-id "woogachaka"
  (spawn))

(with-twitch-id "jesscxc"
  (spawn))


(with-twitch-id "stephenrfoster"
  (run 75))

(with-twitch-id "kenzo_25"
  (run 75))

(with-twitch-id "runiracc"
  (run 75))

(with-twitch-id "woogachaka"
  (run 75))

(with-twitch-id "laurond"
  (run 75))

(with-twitch-id "jesscxc"
  (run 75))

(sleep 10000)
