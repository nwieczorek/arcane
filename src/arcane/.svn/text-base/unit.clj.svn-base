(ns arcane.unit
  (:use arcane.common))


;=========  Unit Naming ===========================================
(def start-consonants
  [    "br" "bl" "dr" "dl" "dh"  "fr" "fl" "gr" "gl" "gh"
       "kr" "kh" "kl"  "pr" "pl" "str" "sl" "sp"  "tr" "j" ])
    
(def end-consonants
  ;[ "rb" "rt" "rg"  "lb" "lt" "lk" "nk" "nd" "nt"  "ft" ])
  [ "rb" "rt" "rg"  "lb" "lt" "lk" "nk" "nd"   "ft" ])

(def any-consonants
  [ "b" "d" "f" "g" "k" "l" "m" "n" "p" "r" "s" "t"
    "v" "x" "z"  "ch"  "sh" "st"  "th" ])

(def vowels
  [ "a" "i" "o" "u"])

(def rules
  '(  ( :s :v :e)
      ( :s :v :c :v)
      ( :s :v :c :v :e)
      ( :s :v :c :v :c :v ) ))


(defn expand
  [symbol-code]
  (cond
    (= symbol-code :c) (rand-nth any-consonants)
    (= symbol-code :v) (rand-nth vowels)
    (= symbol-code :s) (rand-nth (concat any-consonants start-consonants))
    (= symbol-code :e) (rand-nth (concat any-consonants end-consonants))
    ))

(defn get-name
  []
  (clojure.string/capitalize (reduce str (map expand (rand-nth rules)))))


;=========  Unit ===========================================


(def default-unit 
  {
   :cost 50
   :description "none"
   :move 0
   :health 0
   :max-health 0
   :frequency 1
   :min-level 1
   :max-level 9
   :threat 2
   :range 1 
   :effect :blood
   :missile :arrow
   :attack-type :physical
   ;:physical-resistance 4 
   ;:magic-resistance 4 
   ;:damage 3
   })

(def num-strikes 5)

(defn get-damage
  [attacker target range]
  (loop [strike-count 0
         damage 0]
    (if (= strike-count num-strikes)
      damage
      (let [roll (rand-int 10)
            base-damage (attacker :damage)
            defense (target (keyword-append (attacker :attack-type) :defense))
            new-damage (if (>= roll defense) base-damage 0)]
          (recur (inc strike-count) (+ damage new-damage))))))

(def no-name "~")
(defn make-unit
  ([base name]
    (assoc base :id (gensym) :name name))
  ([base]
    (make-unit base no-name))
  )

(defn apply-damage
  [unit damage]
  (assoc unit :health (- (unit :health) damage)))

(defn unit-equals
  [u1 u2]
  (= (u1 :id) (u2 :id)))

(defn describe
  [unit]
  (if (= (unit :name) no-name)
    (unit :description)
    (str (unit :name) " the " (unit :description)  )))


(defn status-describe
  [unit]
  (let [line1 (str (describe unit))
        line2 (str " Health: " (unit :health) 
                   " Move: " (unit :move) 
                   " Range: " (unit :range) 
                   " Damage: " (unit :damage))]
    [ line1 line2]))



(defn load-units
  [unit-chars]
  (reduce (fn [um u-info]
            (let [merged-info (merge default-unit u-info)
                  full-info (assoc merged-info :max-health (merged-info :health))]
             (assoc um (full-info :template-id) full-info))) 
            {} unit-chars))


(def hero-chars
  '(
    {:template-id :warrior :description "Warrior" :cost 50 :move 4 :health 120 :damage 5 :physical-defense 4 :magic-defense 4}
    {:template-id :sorcerer :description "Sorcerer" :cost 55 :move 3 :health 80 :range 6 :missile :orange :effect :orange-flash :damage 7 :physical-defense 4 :magic-defense 4}
    {:template-id :archer :description "Archer" :cost 45 :move 5 :health 90 :range 6 :missile :arrow :damage 3 :physical-defense 4 :magic-defense 4}
    {:template-id :priest :description "Priest" :cost 55 :move 3 :health 100 :damage 4 :physical-defense 4 :magic-defense 4}
    {:template-id :wizard :description "Wizard" :cost 55 :move 3 :health 70 :damage 6 :physical-defense 4 :magic-defense 4}
    {:template-id :enchanter :description "Enchanter" :cost 55 :move 3 :health 70 :damage 4 :physical-defense 4 :magic-defense 4}
    {:template-id :witch :description "Witch" :cost 55 :move 4 :health 80 :damage 5 :physical-defense 4 :magic-defense 4}
    ))

(def foe-chars
  '(
    {:template-id :kobold-spearman :description "Kobold Spearman" :move 4 :health 40 :range 3 :missile :spear :damage 3 :physical-defense 4 :magic-defense 4}
    {:template-id :kobold-warrior :description "Kobold Warrior" :move 4 :health 40 :damage 4 :physical-defense 4 :magic-defense 4}

    {:template-id :goblin-warrior :description "Goblin Warrior" :move 4 :health 40 :damage 4 :physical-defense 4 :magic-defense 4}
    {:template-id :goblin-champion :description "Goblin Champion" :move 4 :health 40 :threat 3 :damage 6 :physical-defense 4 :magic-defense 4}
    {:template-id :goblin-shaman :description "Goblin Shaman" :move 4 :health 40 :threat 3 :range 5 :missile :orange :effect :orange-flash :damage 4 :physical-defense 4 :magic-defense 4}

    {:template-id :ogre-warrior :description "Ogre" :move 4 :health 120 :threat 4 :damage 6 :physical-defense 4 :magic-defense 4} 
    {:template-id :ogre-two-headed :description "Two Headed Ogre" :move 4 :health 160 :threat 5 :damage 8 :physical-defense 4 :magic-defense 4}
    {:template-id :ogre-shaman :description "Ogre Shaman" :move 4 :health 120 :threat 4 :damage 5 :physical-defense 4 :magic-defense 4 :missile :stone :range 5}

    ))


(def heroes (load-units hero-chars))
(def foes (load-units foe-chars))

;=========================================================================================
;=========================================================================================
(defn get-foe-races
  [all-foe-list]
  (let [str-keys (map str (keys all-foe-list))
        race-foe-pairs (keep identity (map 
                                         #(when-let [m (re-find #":(.*?)-" %)]
                                            (let [[all-m gr-m] m]
                                              [gr-m (keyword (strip-colon %))]))
                                            str-keys))
        foe-map (reduce (fn [rf-map rf-pair]
                           (let [[race foe] rf-pair
                                 kw (keyword race)]
                             (if-let [curr-list (rf-map kw)]
                               (assoc rf-map kw (conj curr-list foe))
                               (assoc rf-map kw (list foe)))))
                          {} race-foe-pairs) ] 
    (reduce (fn [r-map kw]
              (let [foe-list (foe-map kw)]
                (assoc r-map kw 
                       {:foes foe-list
                        :min-level (apply min (map #((foes %) :min-level) foe-list)) 
                        :max-level (apply max (map #((foes %) :max-level) foe-list)) 
                        :total-freq  (apply + (map #((foes %) :frequency) foe-list))
                        } ))) 
              {} (keys foe-map)) 
    ))

(def foe-races (get-foe-races foes))

(defn get-foe-race
  [level]
  (println "Getting race for level " level)
  (rand-nth (filter #(let [race-map (foe-races %)]
                       (and (>= level (race-map :min-level))
                            (<= level (race-map :max-level))))
                       (keys foe-races))))


(defn get-random-foe 
  [race]
  (let [orig-n (rand-int (race :total-freq))]
    (loop [foe-list (race :foes)
           n orig-n]
      (let [ffk (first foe-list)
            ff (foes ffk)]
        (if (or (= (count foe-list) 1)
                (< n (ff :frequency)))
            ffk
            (recur (rest foe-list) (- n (ff :frequency))))))))

(defn get-threat-threshold 
  [level]
  (+ 6 (rand-int 5) (* 3 level) )
  )

(defn get-foes
  [race-key level]
  (println "Race key is " race-key " level is " level)
  (let [race (foe-races race-key)
        threat-threshold (get-threat-threshold level)]
    (loop [threat 0
           foe-keys '()]
      (if (> threat threat-threshold)
        foe-keys
        (let [new-foe (get-random-foe race)]
          (recur (+ threat ((foes new-foe) :threat)) (conj foe-keys new-foe)))))))


(defn get-random-foes
  [race level]
  (map #(make-unit (foes %)) (get-foes race level)))

(defn main 
  []
  (prn foe-races )
  (println)
  (prn (get-foe-race 1))
  (prn (get-foe-race 2))
  (println)
  (prn (get-foes :kobold 1))
  (println)
  (prn (get-foes :ogre 3))
  )


;=========================================================================================
(defn hero-leveling
  [hero-spaces]
  (for [h-sp hero-spaces]
    (let [hero (h-sp :unit)
          hero-reset-health (assoc hero :health (hero :max-health))]
      (assoc h-sp :unit hero-reset-health)
      ;TODO level up 
  )))
