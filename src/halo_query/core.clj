(ns halo-query.core
  (:require [clojure.string :as str])
  (:import [java.net DatagramSocket DatagramPacket InetSocketAddress]))

(def ^:private flags
  "Flag string lookup table for decoding game and player flags"
  {;; Player Flags
   :lives [##Inf 1 3 5]
   :maximum-health ["50%" "100%" "150%" "200%" "300%" "400%"]
   :shields [true false]
   :respawn-time [0 5 10 15]
   :respawn-growth [0 5 10 15]
   :odd-man-out [false true]
   :invisible-players [false true]
   :suicide-penalty [0 5 10 15]
   :infinite-grenades [false true]
   :weapon-set ["Normal"
                "Pistols"
                "Assault Rifles"
                "Plasma"
                "Sniper"
                "No Sniping"
                "Rocket Launchers"
                "Shotguns"
                "Short Range"
                "Human"
                "Covenant"
                "Classic"
                "Heavy Weapons"]
   :starting-equipment ["Custom" "Generic"]
   :indicator ["Motion Tracker" "Nav Points" "None"]
   :other-players-on-radar ["No" "All" "" "Friends"]
   :friend-indicators [false true]
   :friendly-fire ["Off" "On" "Shields Only" "Explosives Only"]
   :friendly-fire-penalty [0 5 10 15]
   :auto-team-balance [false true]
    ;; Vehicle Flags
   :vehicle-respawn [0 30 60 90 120 180 300]
   :red-vehicle-set ["Default"
                     "No vehicles"
                     "Warthogs"
                     "Ghosts"
                     "Scorpions"
                     "Rocket Warthogs"
                     "Banshees"
                     "Shades"
                     "Custom"]
   :blue-vehicle-set ["Default"
                      "No vehicles"
                      "Warthogs"
                      "Ghosts"
                      "Scorpions"
                      "Rocket Warthogs"
                      "Banshees"
                      "Shades"
                      "Custom"]
    ;; Game Falgs
   :game-type [""
               "Capture the Flag"
               "Slayer"
               "Oddball"
               "King of the Hill"
               "Race"]
    ;; CTF
   :assault [false true]
   :flag-must-reset [false true]
   :flag-at-home-to-score [false true]
   :single-flag [0 60 120 180 300 600]
    ;; Slayer
   :death-bonus [true false]
   :kill-penalty [false true]
   :kill-in-order [false true]
    ;; Oddball
   :random-start [false true]
   :speed-with-ball ["Slow" "Normal" "Fast"]
   :trait-with-ball ["None" "Invisible" "Extra Damage" "Damage Resistant"]
   :trait-without-ball ["None" "Invisible" "Extra Damage" "Damage Resistant"]
   :ball-type ["Normal" "Reverse Tag" "Juggernaut"]
   :ball-spawn-count [1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16]
    ;; KOTH
   :moving-hill [false true]
    ;; Race
   :race-type ["Normal" "Any Order" "Rally"]
   :team-scoring ["Minimum" "Maximum" "Sum"]})

(defn- to-int
  [s]
  (if (str/blank? s)
    (Integer. 0)
    (Integer. s)))

(defn- to-bool
  [s]
  (if (= s "1")
    true
    false))

(defn- get-flags
  "Takes keys and values, returns an array map where
  each key has been mapped to the associated value
  in `flags`

  ```clojure
  (get-flags :lives   (bit-and 63 3)
             :shields (-> (bit-shift-right 63 5)
                          (bit-and 1)))
  ;; => {:lives 5, :shields false}
  ```"
  [& kvs]
  (let [pairs (partition 2 kvs)]
    (into {}
          (map #(vector (first %)
                        (get-in flags [(first %) (second %)]))
               pairs))))

(defn- request
  "Takes a `host` string and `port` integer,
  returns a vector containing keywords
  and values.

  ```clojure
  (request \"216.128.147.196\" 2302)
  ;; => [:hostname \"Halo\" :gamever \"01.00.10.0621\" :hostport \"2302\" ...]
  ```"
  [host port]
  (with-open [socket (DatagramSocket.)]
    (let [address (InetSocketAddress. host port)
          packet-out (DatagramPacket. (.getBytes "\\query") 6 address)
          packet-in (DatagramPacket. (byte-array 2048) 2048)]
      (.send socket packet-out)
      (.receive socket packet-in)
      (let [result (String. (.getData packet-in) 0 (.getLength packet-in))
            split (str/split result #"\\")]
        (->> (rest (if (= (last split) "nextmode")
                     (conj split "")
                     split))
             (map-indexed #(if (even? %1)
                             (keyword %2)
                             %2))
             (vec))))))

(defn- offsets
  "Takes a [[request]] `req`, returns an array map
  of offsets for locating player names, scores, pings,
  teams, and the end of player data.

  ```clojure
  (let [req (request \"216.128.147.196\" 2302)]
                   (offsets req))
  ;; => {:player-offset 32,
         :score-offset 34,
         :ping-offset 36,
         :team-offset 38,
         :end-offset 40}
  ```"
  [req]
  (let [player-count (Integer. (nth req 19))
        player-offset (.indexOf req :player_0)
        score-offset (+ player-offset (* player-count 2))
        ping-offset (+ player-offset (* player-count 4))
        team-offset (+ player-offset (* player-count 6))
        end-offset (+ player-offset (* player-count 8))]
    (array-map :player-offset player-offset
               :score-offset score-offset
               :ping-offset ping-offset
               :team-offset team-offset
               :end-offset end-offset)))

(defn- server-empty?
  "Takes a [[request]] `req`, returns `true` if
  the server is empty and `false` if it has players"
  [req]
  (not (pos? (Integer. (get req 19)))))

(defn- cast-players-map
  "Takes a [[players]] `players` array map,
  returns a new map with values cast to
  appropriate types."
  [players-map]
  (-> players-map
      (update :score to-int)
      (update :ping to-int)
      (update :team to-int)))

(defn- get-players
  "Takes a [[request]] `req` and [[offsets]] `offsets`,
  returns a vector of array maps holding player data.

  ```clojure
  (let [req (request \"216.128.147.196\" 2302)
        offsets (offsets req)]
    (get-players req offsets))
  ;; => [{:name \"New001\", :score \"0\", :ping \"33\", :team \"0\"}]
  ```"
  [req offsets]
  (if (server-empty? req)
    []
    (let [player-count (Integer. (nth req 19))
          {player-offset :player-offset
           score-offset :score-offset
           ping-offset :ping-offset
           team-offset :team-offset
           end-offset :end-offset} offsets
          player-parts (vector (partition 2 (subvec req player-offset score-offset))
                               (partition 2 (subvec req score-offset ping-offset))
                               (partition 2 (subvec req ping-offset team-offset))
                               (partition 2 (subvec req team-offset end-offset)))]
      (vec (for [i (range player-count)]
             (->> player-parts
                  (map #(second (nth % i)))
                  (zipmap [:name :score :ping :team])
                  (cast-players-map)))))))

(defn- decode-player-flags
  "Takes `i` integer player flags,
  returns an array map containing the
  keys mapped to their decoded values.

  See: [[get-flags]]"
  [i]
  (get-flags
   :lives                  (bit-and i 3)
   :maximum-health         (-> (bit-shift-right i 2)
                               (bit-and 7))
   :shields                (-> (bit-shift-right i 5)
                               (bit-and 1))
   :respawn-time           (-> (bit-shift-right i 6)
                               (bit-and 3))
   :respawn-growth         (-> (bit-shift-right i 8)
                               (bit-and 3))
   :odd-man-out            (-> (bit-shift-right i 10)
                               (bit-and 1))
   :invisible-players      (-> (bit-shift-right i 11)
                               (bit-and 1))
   :suicide-penalty        (-> (bit-shift-right i 12)
                               (bit-and 3))
   :infinite-grenades      (-> (bit-shift-right i 14)
                               (bit-and 1))
   :weapon-set             (-> (bit-shift-right i 15)
                               (bit-and 15))
   :starting-equipment     (-> (bit-shift-right i 19)
                               (bit-and 1))
   :indicator              (-> (bit-shift-right i 20)
                               (bit-and 3))
   :other-players-on-radar (-> (bit-shift-right i 22)
                               (bit-and 3))
   :friend-indicators      (-> (bit-shift-right i 24)
                               (bit-and 1))
   :friendly-fire          (-> (bit-shift-right i 25)
                               (bit-and 3))
   :friendly-fire-penalty  (-> (bit-shift-right i 27)
                               (bit-and 3))
   :auto-team-balance      (-> (bit-shift-right i 29)
                               (bit-and 1))))

(defn- decode-vehicle-flags
  "Takes `i` integer vehicle flags,
  returns an array map containing the
  keys mapped to their decoded values.

  See: [[get-flags]]"
  [i]
  (get-flags
   :vehicle-respawn  (bit-and i 7)
   :red-vehicle-set  (-> (bit-shift-right i 3)
                         (bit-and 15))
   :blue-vehicle-set (-> (bit-shift-right i 7)
                         (bit-and 15))))

(defn- decode-game-flags
  "Takes `i` integer game flags,
  returns an array map containing the
  keys mapped to their decoded values.

  See: [[get-flags]]"
  [i]
  (let [game-flags (get-flags :game-type
                              (bit-and i 7))
        game-type (:game-type game-flags)]
    (merge
     game-flags
     (case game-type
       "Capture the Flag"
       (get-flags
        :assault               (-> (bit-shift-right i 3)
                                   (bit-and 1))
        :flag-must-reset       (-> (bit-shift-right i 5)
                                   (bit-and 1))
        :flag-at-home-to-score (-> (bit-shift-right i 6)
                                   (bit-and 1))
        :single-flag           (-> (bit-shift-right i 7)
                                   (bit-and 7)))
       "Slayer"
       (get-flags
        :death-bonus   (-> (bit-shift-right i 3)
                           (bit-and 1))
        :kill-penalty  (-> (bit-shift-right i 5)
                           (bit-and 1))
        :kill-in-order (-> (bit-shift-right i 6)
                           (bit-and 1)))
       "Oddball"
       (get-flags
        :random-start       (-> (bit-shift-right i 3)
                                (bit-and 1))
        :speed-with-ball    (-> (bit-shift-right i 5)
                                (bit-and 3))
        :trait-with-ball    (-> (bit-shift-right i 7)
                                (bit-and 3))
        :trait-without-ball (-> (bit-shift-right i 9)
                                (bit-and 3))
        :ball-type          (-> (bit-shift-right i 11)
                                (bit-and 3))
        :ball-spawn-count   (-> (bit-shift-right i 13)
                                (bit-and 31)))
       "King of the Hill"
       (get-flags
        :moving-hill (-> (bit-shift-right i 3)
                         (bit-and 1)))
       "Race"
       (get-flags
        :race-type    (-> (bit-shift-right i 3)
                          (bit-and 3))
        :team-scoring (-> (bit-shift-right i 5)
                          (bit-and 3)))))))

(defn- clean-request
  "Takes a [[request]] `req` and [[offsets]] `offsets`,
  returns the request vector stripped of player and flag
  data."
  [req offsets]
  (if (server-empty? req)
    ;; If the server is empty, just strip out flags
    (into (subvec req 0 28)
          (subvec req 32))
    ;; If players are present, strip both flags and players
    (into (subvec req 0 28)
          (subvec req (:end-offset offsets)))))

(defn- cast-cleaned-map
  "Takes `cleaned-map` (result of [[clean-request]] put into an array map),
  returns a new map with values cast to appropriate types."
  [cleaned-map]
  (-> cleaned-map
      (update :maxplayers to-int)
      (update :game_classic to-bool)
      (update :password to-bool)
      (update :fraglimit to-int)
      (update :numplayers to-int)
      (update :hostport to-int)
      (update :teamplay to-bool)
      (update :dedicated to-bool)))

(defn query
  "Query a Halo Custom Edition Server,
  returns an array map containing the
  server information.

  ```clojure
  (query \"216.128.147.196\" 2302)
  ;; => {:maxplayers 16,
         :numplayers 1,
         :hostname \"Halo\",
         :gametype \"Slayer\",
         :hostport 2302,
         :mapname \"bloodgulch\",
         :players [{:name \"New001\", :score 0, :ping 32, :team 0}],
         :dedicated true,
         ...}
  ```"
  [host port]
  (let [req (request host port)
        offsets (offsets req)
        players (get-players req offsets)
        pv-flags-str (get req 29)
        [p-flags v-flags] (map #(Integer. %)
                               (str/split pv-flags-str #","))
        player-flags (decode-player-flags p-flags)
        vehicle-flags (decode-vehicle-flags v-flags)
        game-flags (decode-game-flags (Integer. (get req 31)))
        cleaned (apply array-map
                       (clean-request req offsets))]
    (assoc (cast-cleaned-map cleaned)
           :players players
           :player-flags player-flags
           :vehicle-flags vehicle-flags
           :game-flags game-flags)))

query
