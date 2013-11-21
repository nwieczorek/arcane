(ns arcane.dungen
  (:use arcane.common))


(def side-growth-chance 7)
(def middle-growth-chance 9)
(def empty-cell 0)
(def wall-cell 1)
(def start-bud-number 2)
;===================================================================
; Utilities
(defn get-key
  [x y]
  "Return a map key for the given x and y"
  (keyword (str x "-" y)))

(defn get-all-coords
  []
  (for [x (range world-width)
        y (range world-height)]
    [ x y ]))

(defn has-wall
  [c]
  (> c 0))

(defn get-index
  [x y]
  (+ x (* y world-width)))

(defn coord-to-key
  [c]
  (let [[x y] c]
    (get-key x y)))

(defn bud-to-key
  [b]
  (let [[x y bn bg] b]
    (get-key x y)))

(defn bud-number
  [b]
  (let [[x y bn bg] b]
    bn))


(defn coord-to-bud
  [c bn bg]
  (let [[x y] c]
   [x y bn bg])) 

;===================================================================
; Dungeon Creation

(defn make-cell
  [x y cell]
  (case cell
    :x wall-cell
     empty-cell))


(def base-dungeon
  [  :x :x :x :x :x :x :x :x :x :x :x :x :x :x
     :x :o :n1 :o :n1 :o :n1 :n2 :o :n2 :o :n2 :o :x
     :x :o :o :o :o :o :o :o :o :o :o :o :o :x
     :x :w :o :o :o :o :o :o :o :o :o :o :e :x
     :x :o :o :o :o :o :o :o :o :o :o :o :o :x
     :x :w :o :o :o :m :o :o :m :o :o :o :e :x
     :x :w :o :o :o :o :o :o :o :o :o :o :e :x
     :x :o :o :o :o :o :o :o :o :o :o :o :o :x
     :x :w :o :o :o :m :o :o :m :o :o :o :e :x
     :x :o :o :o :o :o :o :o :o :o :o :o :o :x
     :x :w :o :o :o :o :o :o :o :o :o :o :e :x
     :x :o :s1 :s1 :o :o :o :o :o :o :s2 :s2 :o :x
     :x :x :x :x :x :o :o :o :o :o :x :x :x :x
     :x :x :x  :x  :x :o :o :o :o :x :x  :x  :x :x ])

(defn get-base-coords
  [key]
  (filter (fn [c]
            (let [[x y] c]
              (= (base-dungeon (get-index x y)) key)))
          (get-all-coords)))
   


(defn get-buds-for-key
  [key num-to-take]
  (take num-to-take (shuffle (get-base-coords key))))

(defn get-buds
  [area]
  (let [num-to-take (case area
                      :north (+ 1 (rand-int 2))
                      :south 1
                      :east (+ 2 (rand-int 3))
                      :west (+ 2 (rand-int 3))
                      :middle 2)]
    (case area
      :north  (concat
                (get-buds-for-key :n1 num-to-take)
                (get-buds-for-key :n2 (- 3 num-to-take)))
      :south  (concat
                (get-buds-for-key :s1 num-to-take)
                (get-buds-for-key :s2 num-to-take))
      :east (get-buds-for-key :e num-to-take)
      :west (get-buds-for-key :w num-to-take)
      :middle (get-buds-for-key :m num-to-take)
      )))



(defn make-dungeon-grid
  []
  (let [coords (get-all-coords)]
    (reduce (fn [d-map coord]
              (let [[x y] coord
                    cell (base-dungeon (get-index x y))
                    key (get-key x y)]
                (assoc d-map key (make-cell x y cell))))
            {} coords)))
     


(defn add-bud-numbers
  [in-buds]
  (map (fn [inbud number] (let [[x y growth] inbud] [x y number growth]))
       in-buds
       (range start-bud-number (+ (count in-buds) start-bud-number))))

(defn add-bud-growth
  [in-buds growth]
  (map (fn [inbud] (let [[x y] inbud] [x y growth]))
       in-buds))


(defn get-start-buds
  "Bud is [ x-coordinate y-coordinate bud-number growth-change ]
  Growth chance is 0 - 10"
  []
  (let [raw-buds  (concat
                    (add-bud-growth
                      (reduce concat '() (map get-buds '(:east :west :north :south)))
                      side-growth-chance)
                    (add-bud-growth
                      (get-buds :middle)
                      middle-growth-chance))]
    (add-bud-numbers raw-buds)))



(defn make-dungeon
  []
  (let [initial-grid (make-dungeon-grid)
        buds (get-start-buds)]
  {:grid (reduce #(assoc %1 (bud-to-key %2) (bud-number %2))  initial-grid buds) :buds buds }
  ))


;=============================================================================
; Dungeon Transformation

(def orthogonal-adjacencies
  '([0 1] [0 -1] [1 0] [-1 0]))

(def diagonal-adjacencies
  '( [-1 -1] [-1 1] [1 -1] [1 1]))

(def all-adjacencies
  (concat orthogonal-adjacencies diagonal-adjacencies))

(defn get-adjacent-coords
  [which x y]
  (let [adjs (cond
               (= which :orthogonal) orthogonal-adjacencies
               (= which :all) all-adjacencies
               (= which :diagonal) diagonal-adjacencies)]
    (map (fn [c]
           (let [[x-inc y-inc] c]
             [(+ x x-inc) (+ y y-inc)]))
        adjs)))

(defn count-adjacent-walls
  [dungeon-grid  x y]
    (let [coords (get-adjacent-coords :all x y)
          keys (map coord-to-key coords)
          cells (keep identity (map #(dungeon-grid %) keys))
          walls (filter has-wall cells)]
      (count walls)))


(defn count-adjacent-alien-walls
  "Count walls having a different bud-number"
  [dungeon-grid  x y bud-number]
    (let [coords (get-adjacent-coords :all x y)
          keys (map coord-to-key coords)
          cells (keep identity (map #(dungeon-grid %) keys))
          walls (filter #(and (> % 0) (not= % bud-number)) cells)]
      (count walls)))


   
   
(defn get-random-adjacent
  [dungeon-grid x y bud-number]
  (let [coords (get-adjacent-coords :orthogonal x y)
        not-blocked-coords (filter (fn [c]
                                     (let [[x y] c]
                                       (=  (count-adjacent-alien-walls dungeon-grid x y bud-number) 0 )))
                                     coords)]
    (when (not-empty not-blocked-coords)
     (rand-nth not-blocked-coords))))


(defn add-new-growth
 "Iterate through buds, add new growth for some percentage of them.
  Need to do one at a time to get the correct adjacency"
  [dungeon]
  (loop [dg (dungeon :grid)
        old-buds (dungeon :buds)
        new-buds '()]
    (if (empty? old-buds)
      {:grid dg :buds new-buds}
      (do
        (let [[bud-x bud-y bud-number bud-growth] (first old-buds) ]
          (if (< (rand-int 10) bud-growth)
            (if-let [adj (get-random-adjacent dg bud-x bud-y bud-number)]
              (let [adj-key (coord-to-key adj)]
                (recur (assoc dg adj-key bud-number) (rest old-buds) (conj new-buds (coord-to-bud adj bud-number bud-growth))))
              (recur dg (rest old-buds) new-buds))
            (recur dg (rest old-buds) (conj new-buds (first old-buds)))))))))

(defn get-density
  [dungeon]
  (let [ks (map coord-to-key (get-all-coords))]
    (reduce (fn [t k]
             (+ t (if (has-wall ((dungeon :grid) k)) 1 0)))
              0 ks)))
 

(defn iterate-dungeon
  [dung-orig]
  (loop [i 0
         dungeon dung-orig]
    (let [density (get-density dungeon)]
      (if (and (< i dun-gen-max-iterations) (< density dun-gen-threshold-density))
        (recur (+ i 1) (add-new-growth dungeon))
        (do
          ;(prn 'i i 'dnsty density)
          dungeon)
      ))))


(defn get-dungeon
  []
  (iterate-dungeon (make-dungeon)))

;==================================================================
; Printing

(defn print-cell
  [cell]
  (cond (= cell 0) "-"
        :else "#"))

(defn print-row
  [dungeon y]
  (let [keys (map #(get-key % y) (range world-width)) ]
    (println (apply str (map #(print-cell ((dungeon :grid) %)) keys)))))


(defn print-dungeon
  [dungeon]
  (doseq [y (range world-height)]
    (print-row dungeon y)))


(defn main
  []
  (println "Building....")
  (println)
  (print-dungeon (iterate-dungeon (make-dungeon))))

