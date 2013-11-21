(ns arcane.player
  (:require [arcane.unit :as unt])
  (:use arcane.common))


(defn make-player
  []
  { :gold initial-gold 
    :heroes {} })

(def ^:dynamic *player* (atom (make-player)))


(defn get-player
  [kw]
  (@*player* kw))


(defn player-take-gold
  [amt]
  (when (>= (@*player* :gold) amt)
    (swap! *player* (fn [op] (assoc op :gold (- (op :gold) amt))))))

(defn player-get-heroes
  []
  (vals (@*player* :heroes)))

(defn player-has-heroes?
  []
  (> (count (@*player* :heroes)) 0))

(defn player-add-hero
  [hero]
  (loop [new-name (unt/get-name)]
    (if (contains? (@*player* :heroes) new-name)
      (recur (unt/get-name))
      (let [named-hero (unt/make-unit hero new-name )]
        (swap! *player* (fn [op] 
                          (let [existing-heroes (op :heroes)
                                new-heroes (assoc existing-heroes (named-hero :id) named-hero)]
                            (assoc op :heroes new-heroes))))))))


(defn player-dismiss-hero
  [hero-id]
  (let [heroes (@*player* :heroes)]
    (swap! *player* (fn [op] (assoc op :heroes (dissoc (op :heroes) hero-id))))))  
  

(defn player-buy-hero
  "Return the hero if buy was sucessful, otherwise return false"
  [hero]
  (if (and (>= (get-player :gold) (hero :cost))
           (< (count (get-player :heroes)) max-party-size))
    (do 
      (player-take-gold (hero :cost))
      (player-add-hero hero)
      hero)
    false
    ))

(defn player-update-heroes!
  [hero-updates]
  (let [curr-map (@*player* :heroes)
        current-ids (keys curr-map)
        new-map   (reduce 
                    (fn [hero-map id]
                      (let [curr-hero (curr-map id)]
                        (if-let [hero-update (some #(when (unt/unit-equals % (curr-map id)) %) hero-updates)]
                          (assoc hero-map id (assoc curr-hero :xp (hero-update :xp)))
                          (assoc hero-map id curr-hero))))
                    {} current-ids) ]

    (swap! *player* (fn [op] (assoc op :heroes new-map)))))

(defn player-remove-dead!
  [dead-heroes]
  (let [new-map (reduce
                 (fn [hero-map id]
                   (dissoc hero-map id))
                 (@*player* :heroes)
                 (map #(% :id) dead-heroes))]
    (swap! *player* (fn [op] (assoc op :heroes new-map)))))
