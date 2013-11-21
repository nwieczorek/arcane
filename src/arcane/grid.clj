(ns arcane.grid
  (:require clojure.set )
  (:use arcane.common))

(defn to-index
  ([x y width]
    (+ x (* y width)))
  ([x y]
   (to-index x y world-width)))

(defn space-to-index
  [sp]
  (to-index (sp :x) (sp :y)))

(defn to-x-y
  [index width]
  [  (mod index width) (quot index width)])

(defn within
  [box-x box-y box-width box-height x y]
  (and (>= x box-x) (<= x (+ box-x box-width -1))
       (>= y box-y) (<= y (+ box-y box-height -1))))

(defn between
  [start stop test-val]
  (and (>= test-val start) (<=  test-val stop)))


(defn make-space
  [x y other-map]
  (merge {:x x :y y } other-map))


(defn make-grid
   ([layout thing-func width height]
    (let [actual-count (count layout)
          required-count (* width height)]
      (assert (= actual-count required-count)
              (str "Layout not correct size, " required-count " required, " actual-count " given"))
    {:width width
     :height height
     :grid (into [] (for [idx (range (* width height))]
                 (let [[x y] (to-x-y idx width)
                       loc (layout idx)
                       things (thing-func x y loc) ]
                   (assert (contains?  things :thing) "Thing function did not return map containing :thing")
                   (make-space x y things ))))}))
  ([layout thing-func]
   (make-grid layout thing-func world-width world-height )))


(defn is-valid
  [grid x y]
  (and (between 0 (dec (grid :width)) x)
       (between 0 (dec (grid :height)) y)))


(defn get-spaces
  [grid pred]
  (filterv pred (:grid grid)))
 

(defn get-space
  ([grid x y]
    (let [idx (to-index x y (grid :width))]
      (when (is-valid grid x y)
          ((:grid grid) idx))))
  ([grid sp]
   (get-space grid (sp :x) (sp :y)))
  )

(defn get-thing
  [thinggrid x y]
  (let [space (get-space thinggrid x y)]
    (if space
        (:thing space)
        nil
      )))

(defn space-str
  [sp]
  (str "(" (sp :x) "," (sp :y) "," (sp :thing) ")"))

(defn equal-space
  [sp1 sp2]
  (and (= (sp1 :x) (sp2 :x))
       (= (sp1 :y) (sp2 :y))))


(defn find-space
  [sp-list sp]
  (let [ml (filter #(equal-space % sp) sp-list)]
   (when (not-empty ml)
    (first ml)))) 
;========================================================
; Directions
(def up {:x 0 :y -1 :kw :u })
(def down {:x 0 :y 1 :kw :d })
(def left {:x -1 :y 0 :kw :l })
(def right {:x 1 :y 0 :kw :r })
(def orthogonal-directions (list up down left right ))

(def up-index (space-to-index up))
(def down-index (space-to-index down))
(def left-index (space-to-index left))
(def right-index (space-to-index right))

(def orthogonal-indices (list up-index down-index left-index right-index))

(def up-right {:x 1 :y -1 :kw :ur})
(def up-left {:x -1 :y -1 :kw :ul})
(def down-right {:x 1 :y 1 :kw :dr})
(def down-left {:x -1 :y 1 :kw :dl})
(def diagonal-directions (list up-right up-left down-right down-left))

(def all-directions (concat orthogonal-directions diagonal-directions))


(defn apply-direction
  [sp d]
  {:x (+ (sp :x) (d :x)) :y (+ (sp :y) (d :y))})

(defn are-adjacent? 
  ([sp1 sp2 include-diagonal]
    (or (some #(equal-space sp2 %) (map #(apply-direction sp1 %) orthogonal-directions))
        (and include-diagonal
            (some #(equal-space sp2 %) (map #(apply-direction sp1 %) diagonal-directions)))))
  ([sp1 sp2]
   (are-adjacent? sp1 sp2 true)))


(defn get-adjacent-space
  [grid space direction]
  (let [new-sp (apply-direction space direction)]
    (get-space grid (new-sp :x) (new-sp :y))))

(defn get-adjacent
  ([grid space include-diagonal]
   (let [dirs (if include-diagonal (concat orthogonal-directions diagonal-directions) orthogonal-directions)]
   (keep identity (map #(get-adjacent-space grid space %) dirs))))
  ([grid space]
   (get-adjacent grid space false)))



;========================================================
; Update functions
;
(defn update-grid
  "Update the grid for specific spaces, using update-func. update-func takes the updated space as an argument"
  [in-grid update-spaces update-func]
  (let [grid-size (* (in-grid :width) (in-grid :height))
        in-spaces (in-grid :grid)
        update-indices (map space-to-index update-spaces)
        ]
    (assoc in-grid :grid
      (into []
        (for [idx (range grid-size)]
          (let [sp (in-spaces idx)]
            (if (some #(= % idx) update-indices)
              (update-func sp)
              sp)))))))


(defn- make-adjacency-map
  [grid sp]
  (reduce #(assoc %1 (space-to-index %2) (get-adjacent-space grid sp %2)) {} all-directions))

(defn update-grid-by-adjacent
  "Update the grid. update-func takes a space and a map of direction : adjacent space"
  [in-grid update-func]
  (assoc in-grid :grid
    (into []
      (for [sp (in-grid :grid)]
        (let [am (make-adjacency-map in-grid sp)]
          (update-func sp am))))))
        



;=======================================================================
(defn get-distance
  [space-a space-b]
  (+ (Math/abs (- (space-a :x) (space-b :x)))
     (Math/abs (- (space-a :y) (space-b :y)))))

(defn distance-from-center
  [grid sp]
  (let [center (make-space (quot (grid :width) 2) (quot (grid :height) 2) {})]
    (get-distance center sp)))


(defn- unwind-path
  [came-from target]
  (loop [path '()
        curr-node target]
    (let [new-path (conj path curr-node)
          curr-key (space-to-index curr-node)]
      (if (contains? came-from curr-key )
        (recur new-path (came-from curr-key))
        new-path))))


(defn get-path
  "path includes origin and target"
  [grid origin target passable-func max-distance]
  (let [not-nil-and-passable #(when-let [sp %] (passable-func sp))]
    (loop [distance {}
           came-from {}
           to-visit (list origin)
           curr-dist 0]
      (when (and (not-empty to-visit)
                 (< curr-dist max-distance))
        (let [new-dist (inc curr-dist)
              adj-spaces (keep identity
                               (for [sp to-visit
                                    d orthogonal-directions]
                                  (when-let [adj  (get-adjacent-space grid sp d)]
                                    (when (and (not (equal-space adj origin))
                                               (not (contains? distance (space-to-index adj)))
                                               (passable-func adj))
                                       ;[new-space from-space]
                                       [adj sp]
                                    ))))
              new-spaces (map #(% 0) adj-spaces)
              new-distance (reduce #(assoc %1 (space-to-index %2) new-dist) {} new-spaces)
              new-came-from (reduce #(let [[sa sf] %2] (assoc %1 (space-to-index sa) sf)) {} adj-spaces)
              ]
          (if (some #(equal-space target %) new-spaces)
            (unwind-path (merge came-from new-came-from) target)
            (recur (merge distance new-distance)
                   (merge came-from new-came-from)
                   new-spaces
                   new-dist)))))))


(defn get-sign
  [x]
  (cond (< x 0) -1
        (> x 0) 1
        :else 0))

(defn get-direction
  [origin target]
  (let [x-diff (get-sign (- (target :x) (origin :x)))
        y-diff (get-sign (- (target :y) (origin :y)))
        diff-sp (make-space x-diff y-diff {})]
    (first (filter #(equal-space % diff-sp) all-directions))))


(defn get-line
  "Does not include the origin, if the line ended before max-distance it includes the space blocking the line.
  To end the line at a target space, passable-func should return false for the space, and 
  included-block-func should return true"
  [grid origin direction passable-func included-blocker-func max-distance]
  (let [not-nil-and-passable #(when-let [sp %] (passable-func sp))
        not-nil-and-inc-blocker #(when-let [sp %] (included-blocker-func sp))]
    (reverse
      (loop [line '()
             curr-dist 0
             curr-sp origin]
        (if (< curr-dist max-distance)
          ;TODO curr-sp can be nil?
          (let [next-space (get-space grid (apply-direction curr-sp direction))]
            (cond (not-nil-and-passable next-space)
                    (recur (conj line next-space) (inc curr-dist) next-space)
                  (not-nil-and-inc-blocker next-space)
                    (conj line next-space)
                  :else  line))
          line)))))


(defn get-attackable
  [grid origin passable-func attackable-func max-distance]
  (if origin
    (let [attack-lines (map #(get-line grid origin % passable-func attackable-func max-distance) all-directions)]
      (keep identity
        (for [line attack-lines]
          (let [attackable (filter attackable-func line)]
            (when (not-empty attackable)
              (first attackable))))))
    '()
    
    ))
  

(defn get-reachable
  "reachable includes origin "
  [grid origin passable-func max-distance]
  (let [not-nil-and-passable #(when-let [sp %] (passable-func sp))]
    (loop [reachable #{}
           to-visit (list origin)
           curr-dist 0]
      (if (and (not-empty to-visit)
                 (< curr-dist max-distance))
        (let [new-dist (inc curr-dist)
              adj-spaces (keep identity
                               (for [sp to-visit
                                    d orthogonal-directions]
                                  (when-let [adj  (get-adjacent-space grid sp d)]
                                    (when (and (not (equal-space adj origin))
                                               (not (contains? reachable (space-to-index adj)))
                                               (passable-func adj))
                                       adj
                                    ))))
              new-reachable (set adj-spaces) 
              ]
            (recur (clojure.set/union reachable new-reachable)
                   adj-spaces
                   new-dist))
        reachable
        ))))

 

;=======================================================================
; Functions for Unit Testing
;

(def grid-test-layout
[  :c :c :c :c :c :c :c :c :c :t :c :c :c :c
   :c :c :c :c :w :c :c :c :c :c :c :c :c :c
   :c :c :c :c :w :t :t :c :w :c :c :c :c :c
   :c :c :c :c :w :c :c :c :w :c :c :c :c :c
   :c :c :c :c :w :w :w :c :w :c :c :c :c :c
   :c :w :w :c :c :c :c :c :w :c :w :w :c :c
   :c :c :c :c :c :c :w :w :w :c :c :c :c :c
   :c :c :c :c :c :c :w :c :c :c :c :c :c :c
   :c :c :c :c :c :c :c :c :c :c :c :c :c :c
   :c :w :w :c :w :w :w :c :c :w :w :w :w :c
   :c :c :c :c :c :w :c :c :c :c :c :c :c :c
   :c :c :c :c :c :w :c :c :c :w :c :c :c :c
   :c :c :c :c :c :w :c :c :c :w :c :c :c :c
   :c :c :c :c :c :c :c :c :c :w :c :c :c :c ])

(defn- grid-test-thing-func
  [x y loc]
  {:thing
   (case loc
    :w :wall
     :t :target
    :clear)})


(defn- grid-test-print
  [grid]
  (let [width (grid :width)
        height (grid :height)
        spaces (grid :grid)]
    (print "  ")
    (doseq [x (range width)]
      (print (str (mod x 10))))
    (println)
    (doseq [y (range height)]
      (print (str (mod y 10) " "))
      (doseq [x (range width)]
        (print (case ((spaces (to-index x y width)) :thing)
             :wall "#"
             :clear "-"
             :target "@")))
      (println))
             ))


(defn- print-spaces
  [path]
  (doseq [sp path]
    (print (space-str sp))))

(defn- grid-test-pass-f
  [sp]
  (or (= (sp :thing) :clear)
      (= (sp :thing) :target)))

(defn- grid-test-attack-f
  [sp]
  (= (sp :thing) :target))

(defn- grid-test-path
  [grid x1 y1 x2 y2 max-distance]
  (let [sp-f #(make-space %1 %2 {})
        path (get-path grid (sp-f x1 y1) (sp-f x2 y2) grid-test-pass-f max-distance)]
    (println)
    (print "path(" max-distance ","(count path) "):")
    (print-spaces path)
    (println)))

(defn- grid-test-reachable
  [grid x1 y1 max-distance]
  (let [sp-f #(make-space %1 %2 {})
        reachable (get-reachable grid (sp-f x1 y1) grid-test-pass-f max-distance)]
    (println)
    (print "reachable(" max-distance "): ")
    (print-spaces reachable)
    (println)))


(defn- grid-test-attackable
  [grid x1 y1 max-distance]
  (let [sp-f #(make-space %1 %2 {})
        attackable (get-attackable grid (sp-f x1 y1) grid-test-pass-f grid-test-attack-f max-distance)]
    (println)
    (print "attackable(" max-distance "): ")
    (print-spaces attackable)
    (println)))

(defn- grid-test-line
  [grid x y direction max-distance]
  (let [sp-f #(make-space %1 %2 {})
        line (get-line grid (sp-f x y) direction grid-test-pass-f grid-test-attack-f max-distance)]
    (println)
    (print "line(" max-distance ") :")
    (print-spaces line)
    (println)))

(defn main
  []
  (let [g (make-grid grid-test-layout grid-test-thing-func)]
    (println "Running grid tests")
    (grid-test-print g)
    (grid-test-path g 4 12 6 12 5)
    (grid-test-path g 1 1 4 0 5)
    (grid-test-path g 2 2 7 4 8)
    (grid-test-path g 2 2 10 4 15)
    (grid-test-path g 2 2 10 4 7)

    (grid-test-reachable g 2 2 5)
    (grid-test-line g 0 0  down-left 10)
    (grid-test-line g 0 0  down-right 10)
    (grid-test-attackable g 6 3 4)
    ))

