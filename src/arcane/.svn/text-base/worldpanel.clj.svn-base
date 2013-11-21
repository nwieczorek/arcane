(ns arcane.worldpanel
  (:import (javax.swing JPanel)
           javax.imageio.ImageIO
           (java.awt.event ActionListener MouseListener MouseMotionListener MouseEvent)
           (java.awt Color RenderingHints Toolkit Image Font))
  (:require [arcane.gui-common :as gui]
            [arcane.tileset :as til]
            [arcane.player :as ply]
            [arcane.grid :as grd])
  (:use arcane.common))


(def world-layout
    [
     :w :w  :w  :w  :w  :w  :w  :w  :w  :w  :w  :w  :w  :w
     :w :w  :w  :w  :w  :w  :w  :w  :w  :w  :w  :w  :w  :w
     :w :w  :w  :w  :w  :w  :w  :w  :w  :w  :w  :w  :w  :w
     :w :w  :w  :w  :w  :w  :w  :w  :w  :w  :w  :w  :w  :w
     :w :w  :w  :w  :w  :w  :w  :w  :w  :w  :w  :w  :w  :w
     :w :w  :w  :w  :s  :s  :s  :s  :s  :s  :w  :w  :w  :w
     :w :w  :w  :w  :s  :v  :v :v  :v  :s  :w  :w  :w  :w
     :w :w  :w  :w  :s  :v  :sv  :sv2  :v  :s  :w  :w  :w  :w
     :w :w  :w  :w  :s  :s  :r  :r  :s  :s  :w  :w  :w  :w
     :w :w  :w  :w  :s  :s  :s  :s  :s  :s  :w  :w  :w  :w
     :w :w  :w  :w  :w  :w  :w  :w  :w  :w  :w  :w  :w  :w
     :w :w  :w  :w  :w  :w  :w  :w  :w  :w  :w  :w  :w  :w
     :w :w  :w  :w  :w  :w  :w  :w  :w  :w  :w  :w  :w  :w
     :w :w  :w  :w  :w  :w  :w  :w  :w  :w  :w  :w  :w  :w ])

(def world-thing-map
  {:w :wild
   :s :safe
   :v :village
   :vb :village
   :sv :sign-village
   :sv2 :sign-village
   :r :road})

(def world-nil-trailers
  '(:sv2 ))

(def road-start-space
 { grd/left {:x 6 :y 8}
   grd/right {:x 7 :y 8}})
(def road-placement-times 3) ;each iteration moves 2 spaces to left or right



(defn- get-road-direction
  [primary-dir]
  (flatten 
    (let [ placements (repeat road-placement-times primary-dir)] 
      (map (fn [base-dir] 
             (let [road-die (rand-int 10)]
               (cond (<= road-die 2)
                      (list base-dir grd/up base-dir)
                     (<= road-die 5)
                      (list base-dir grd/down base-dir)
                     :else
                      (list base-dir base-dir))))
          placements))))

(defn- get-roads-for-direction
  [in-grid dir]
  (let [start-space (road-start-space dir)
        directions (get-road-direction dir)]
    (loop [curr-space start-space
           dirs directions
           path '()]
      (if (empty? dirs)
        (reverse path)
        (if-let [adj (grd/get-adjacent-space in-grid curr-space (first dirs))]
          (recur adj (rest dirs) (conj path adj))
          (recur nil '() path))))))



(defn add-roads
  [w-grid ]
  (let [road-spaces (concat (get-roads-for-direction w-grid grd/left)
                            (get-roads-for-direction w-grid grd/right))]
    (grd/update-grid
      w-grid
      road-spaces
      (fn [sp]
        (assoc sp :thing :road
               :images [ (til/get-tileset-image :terrain :clear rand-nth) 
                        (til/get-tileset-image :terrain :road-ew rand-nth)])))))
   
  
(defn adjust-for-roads
  [w-grid ]
  (let [is-road? (fn [sps idx] (when-let[sp (sps idx)] (= (sp :thing) :road)))
        road-images #( vector (til/get-tileset-image :terrain :clear rand-nth) 
                        (til/get-tileset-image :terrain % rand-nth))]
    (grd/update-grid-by-adjacent
      w-grid
      (fn [sp adj-map]
        (cond 
          (= (sp :thing) :road)
          (cond (and (is-road? adj-map grd/right-index)
                     (is-road? adj-map grd/down-index))
                (assoc sp :images (road-images :road-turn-es))
                (and (is-road? adj-map grd/right-index)
                     (is-road? adj-map grd/up-index))
                (assoc sp :images (road-images :road-turn-en))
                (and (is-road? adj-map grd/left-index)
                     (is-road? adj-map grd/down-index))
                (assoc sp :images (road-images :road-turn-ws))
                (and (is-road? adj-map grd/left-index)
                     (is-road? adj-map grd/up-index))
                (assoc sp :images (road-images :road-turn-wn))
                :else sp)
          (and  (= (sp :thing) :wild)
                (some #(= (% :thing) :road) (keep identity (vals adj-map))))
          (assoc sp :thing :safe
                    :images [ (til/get-tileset-image :terrain :clear)])
             
          :else  sp)))))

(defn adjust-wild-border
  [w-grid]
  (let [is-wild? (fn [sps idx] (when-let [sp (sps idx)] (or (= (sp :thing) :wild)
                                                            (= (sp :thing) :lair)  )))]
    (grd/update-grid-by-adjacent
      w-grid
      (fn [sp adj-map]
        (if (and (= (sp :thing) :safe) (some #(is-wild? adj-map %) grd/orthogonal-indices))
          (assoc sp :images
            (into []
              (keep identity
                (list (til/get-tileset-image :terrain :clear rand-nth)
                      (when (is-wild? adj-map grd/up-index) (til/get-tileset-image :terrain :clear-transition-n rand-nth))
                      (when (is-wild? adj-map grd/down-index) (til/get-tileset-image :terrain :clear-transition-s rand-nth))
                      (when (is-wild? adj-map grd/right-index) (til/get-tileset-image :terrain :clear-transition-e rand-nth))
                      (when (is-wild? adj-map grd/left-index) (til/get-tileset-image :terrain :clear-transition-w rand-nth))
                      ))))
          sp)))))

(defn get-world-images
  [loc things]
  (let [get-rand-image (fn [kw] (til/get-tileset-image :terrain kw rand-nth ))
        clear-image (get-rand-image :clear)
        thing (things :thing)]
    (cond 
      (some #(= loc %) world-nil-trailers) [nil]
          (= thing :sign-village)
          [(til/get-tileset-image :terrain :sign-village)]
          (= thing :village)
          [clear-image (get-rand-image :village)]
          (= thing :road)
            [clear-image (get-rand-image :road-ew)]
          (= thing :safe)
            [clear-image]
          :else
            [(get-rand-image :dark-clear)])
  ))

(defn- get-initial-lair-spaces
  [grid]
  (let [clear-spaces (shuffle (grd/get-spaces grid #(= (% :thing) :wild)))]
    (loop [added-count 0
           remaining-clear clear-spaces
           lair-spaces '()]
      (if (or (>= added-count initial-lair-count)
              (empty? remaining-clear))
        lair-spaces
        (let [candidate (first remaining-clear)]
          (if (some #(grd/are-adjacent? % candidate) lair-spaces)
            (recur added-count (rest remaining-clear) lair-spaces)
            (recur (inc added-count) (rest remaining-clear) (conj lair-spaces candidate))))))
    ))

(defn add-initial-lairs
 [w-grid ]
  (let [lair-spaces (get-initial-lair-spaces w-grid)]
  (grd/update-grid 
    w-grid 
    lair-spaces
   (fn [sp] 
     (assoc sp :thing :lair
              :images [ (til/get-tileset-image :terrain :dark-clear rand-nth)])))))


(defn add-thing-decorations
  [grid thing-to-decorate decoration-kw]
  (let [dec-spaces (shuffle (grd/get-spaces grid #(= (% :thing) thing-to-decorate)))
        decoration-count (int (* (count dec-spaces) (/ decoration-percent 100.0) ))
        dec-spaces (take decoration-count dec-spaces)]
    (grd/update-grid
      grid
      dec-spaces
      (fn [sp]
        (assoc sp :images (conj (sp :images)
                           (til/get-tileset-image :terrain decoration-kw rand-nth) )))
      )))

(defn add-decorations
  [grid]
  (-> grid
    (add-thing-decorations :wild :wild-decoration)
    (add-thing-decorations :safe :safe-decoration))) 


(defn make-lair
  [sp level]
  (assoc sp :level level))


(defn get-lair-levels
  "return a sequence of lair levels, with the distribution as defined in the lair-count map"
  []
  (flatten
    (for [level (keys lair-count)]
      (repeat (lair-count level) level)))) 



(defn get-lair-spaces
  "Randomly assign levels to the lair spaces"
  [grid]
  (let [lss (shuffle (grd/get-spaces grid #(= (% :thing) :lair)))
        levels (get-lair-levels) ]
    (loop [in-spaces lss
           levs  levels
           out-spaces '()]
      (if (empty? in-spaces)
        out-spaces
        (recur (rest in-spaces) (rest levs) (conj out-spaces (make-lair (first in-spaces) (first levs) )))))))

(defn get-lair-grob-list
  [lair-spaces ]
  (let [image (til/get-tileset-image :terrain :lair)]
    (flatten
      (for [ls lair-spaces]
        (list (gui/get-image-grob image (ls :x) (ls :y))
              (gui/get-image-grob (til/get-tileset-image :terrain (keyword-append :level (ls :level))) (ls :x) (ls :y)))
        ))))

(defn get-description
  [thing]
  (cond (= thing :lair) "A Lair"
        (= thing :village) "The Village - Hire Your Party Here"
        (= thing :wild) "Nothing of Interest"
        (= thing :safe) "Nothing of Interest"
        (= thing :road) "The Road"
        :else ""))


;================================================================================
;  Main Panel Functions 

(defn world-panel
  [scene-func]
  (let [initial-grid (grd/make-grid world-layout 
                             (fn [x y loc] 
                              (let [things (gui/simple-layout-thing world-thing-map loc)]
                                (merge things
                                       {:images (get-world-images loc things)}))))
        worldgrid (atom (-> initial-grid
                          add-roads
                          adjust-for-roads
                          add-initial-lairs
                          adjust-wild-border
                          add-decorations ))
        lairs (atom (get-lair-spaces @worldgrid))
        pnl-state (gui/make-panel-state)
        dialogue (gui/make-dialogue :ok) ]

  (gui/simple-panel 
        (fn []
          (concat
            (gui/get-grid-grob-list @worldgrid)
            (get-lair-grob-list @lairs)
            (gui/get-selection-grob-list)
            (gui/get-status-grob-list @worldgrid)
            (if (= (@pnl-state :state) :cannot-enter)
                (gui/get-dialogue-grob-list dialogue ["Your Party is Empty" 
                                                      "Enter the Village to Hire Them"])
              [])
            ))
        ;mouse click
        (fn [x y btn] 
         (let [[space-x space-y btn] (gui/translate-mouse-coords x y)]
             (cond (= (@pnl-state :state) :cannot-enter)
                   (when-let [space (gui/get-dialogue-space dialogue space-x space-y)]
                       (let [thing (space :thing)] 
                         (when (= thing :sign-ok)
                               (gui/to-ready-state pnl-state))))
                    
                   :else
                    (when-let [space (grd/get-space @worldgrid space-x space-y)]
                        (let [thing (space :thing)]
                          (println "(" space-x "," space-y "," btn)
                          (println thing "," btn)
                          (cond
                            (= thing :sign-village) 
                              (scene-func :world :village)
                            (= thing :lair)
                              (if (ply/player-has-heroes?)
                                (scene-func :world :lair 
                                            {:lair-index (grd/space-to-index space)
                                             :level ((some #(when (grd/equal-space space %) % )@lairs) :level)
                                             })
                                (gui/to-state pnl-state :cannot-enter))
                            )))
          )))
        ;mouse over
        (fn [x y]
         (let [[space-x space-y btn] (gui/translate-mouse-coords x y)]
                 (when-let [sp (grd/get-space @worldgrid space-x space-y)] 
                   (gui/set-status (get-description (sp :thing))))))
                )))

