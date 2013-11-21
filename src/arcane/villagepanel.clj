(ns arcane.villagepanel
  (:require [arcane.gui-common :as gui]
            [arcane.tileset :as til]
            [arcane.unit :as unt]
            [arcane.player :as ply]
            [arcane.grid :as grd])
  (:use arcane.common))


(def village-layout
  [ :cl :wb '(:wb :lb) :wb :wb '(:wb :lb) :wb :wb '(:wb :lb) :wb :wb '(:wb :lb) :wb :cr
    :wl :f  :f  :f  :f  :f  :f  :f  :f  :f  :f  :f  :f  :wr
    :wl :f  :f  :f  :f  :f  :f  :f  :f  :f  :f  :f  :f  :wr
    '(:wl :ll) :f  :f  :ua  :ua  :ua  :ua  :ua  :ua  :ua  :ua  :f  :f  '(:wr :lr)
    :wl :f  :f  :ua  :ua  :ua  :ua  :ua  :ua  :ua  :ua  :f  :f  :wr
    :wl :f  :f  :f  :f  :f  :f  :f  :f  :f  :f  :f  :f  :wr
    '(:wl :ll) :f  :f  :f  :f  :f  :f  :f  :f  :f  :f  :f  :f  '(:wr :lr)
    :wl :f  :f  :f  :f  :f  :sh  :sh2  :f  :f  :f  :f  :f  :wr
    :wl :f  :f  :f  :f  :f  :f  :f  :f  :f  :f  :f  :f  :wr
    :wl :f  :f  :f  :f  :f  :f  :f  :f  :f  :f  :f  :f  :wr
    '(:wl :ll) :f  :f  :f  :us  :us  :us  :us  :us  :us  :f  :f  :f  '(:wr :lr)
    :wl :f  :f  :f  :us  :us  :us  :us  :us  :us  :f  :f  :f  :wr
    :wl :f  :f  :f  :f  :f  :f  :f  :f  :f  :f  :f  :f  :wr
    :wl :sd :sd2  :f  :f  :f  :f  :f  :f  :f  :f  :se  :se2  :wr ])

(def hero-available-min-x 3)
(def hero-available-min-y 3)
(def hero-available-width 8)

(def village-thing-map
  { :wl :wall-left
    :wr :wall-right
    :wb :wall-back
    :f :floor
    :cl :corner-left
    :cr :corner-right
    :se :sign-exit
    :se2 :sign-exit
    :sd :sign-dismiss
    :sd2 :sign-dismiss
    :sh :sign-hire
    :sh2 :sign-hire
    :lb :lantern-back
    :ll :lantern-left
    :lr :lantern-right 
    :ua :hero-available
    :us :hero-slot
   })

(def village-sign-trailers
  '(:se2 :sd2 :sh2))


(defn decode-village-layout
  [kw]
  (let [th (village-thing-map kw)]
    (assert th (str "No mapping for " kw))
    th))



(defn get-available-hero
  [x y]
  (let [idx (+ (- x hero-available-min-x) (* (- y hero-available-min-y) hero-available-width))
        u-keys (into [] (keys unt/heroes))]
    (when (and (>= idx 0) (< idx (count u-keys)))
      (unt/heroes (u-keys idx))
      ))
  )

   
(defn get-available-spaces
  [grid]
  (filter #(and (= (% :thing) :hero-available) 
                (% :unit)) (grid :grid)))

(defn get-village-images
  [loc thing]
  (let [get-image (fn [kw] (til/get-tileset-image :terrain (decode-village-layout kw) rand-nth ))
        thing-kw (thing :thing)]
    (cond 
      (seq? loc) (into [] (map get-image loc))
      (= thing-kw :hero-slot)
        [(til/get-tileset-image :terrain :floor rand-nth) (get-image loc)]
      (= thing-kw :hero-available) [(get-image :f)]
      (some #(= loc %) village-sign-trailers) [nil]
      :else [(get-image loc)]
      )))



(defn hire-hero
  []
  (when (and @gui/*selection*
             (= :hero-available (@gui/*selection* :thing))
             (contains? @gui/*selection* :unit))
    (let [hero (@gui/*selection* :unit)]
      (if (ply/player-buy-hero hero)
          (println "hiring " hero)
        (println "cannot hire " hero)))))


(defn village-thing
  [x y loc]
  (let [k (if (seq? loc) (first loc) loc)
        tm {:thing (village-thing-map k)}]
    (cond 
      (= loc :ua) 
      {:thing :hero-available :unit (get-available-hero x y)} 
      :else tm)))


;================================================================================



;================================================================================

(defn village-panel
  [ scene-func]
  (let [v-grid (grd/make-grid 
                 village-layout 
                 (fn [x y loc]
                   (let [things (village-thing x y loc)]
                    (merge
                     things
                     {:images (get-village-images loc things)}
                      ))))
        hero-slots (atom (gui/get-hero-slots v-grid))
        available-spaces (get-available-spaces v-grid)
        pnl-state (gui/make-panel-state)
        dialogue (gui/make-dialogue :yes-no)]

  (gui/simple-panel
        (fn []
          (concat
            (gui/get-grid-grob-list v-grid )
            (gui/get-status-grob-list v-grid )
            (gui/get-selection-grob-list)
            (gui/get-unit-spaces-grob-list available-spaces)
            (gui/get-unit-spaces-grob-list @hero-slots)
            (if (= (@pnl-state :state) :confirm-dismiss)
              (when-let [hero (gui/get-selected-hero @hero-slots)]
                (gui/get-dialogue-grob-list dialogue [(str "Dismiss " (unt/describe hero) "?")]))
              [])
            ))
        ;mouse click
        (fn [x y btn] 
           (let [[space-x space-y btn] (gui/translate-mouse-coords x y)]
             (cond (= (@pnl-state :state) :confirm-dismiss)
                   (let [space (gui/get-dialogue-space dialogue space-x space-y)]
                     (when space 
                       (let [thing (space :thing)] 
                         (cond (= thing :sign-yes)
                               (do 
                                 (gui/dismiss-hero @hero-slots)
                                 (reset! hero-slots (gui/get-hero-slots v-grid))
                                 (gui/to-ready-state pnl-state))
                               (= thing :sign-no)
                               (gui/to-ready-state pnl-state)))))

                   :else
                   (when-let [space (grd/get-space v-grid space-x space-y)]
                      (let [thing (space :thing)]
                        (println thing "," btn)
                        (cond 
                          (= thing :sign-exit) (scene-func :village :exit)
                          (= thing :sign-hire) (do 
                                                 (hire-hero)
                                                 (reset! hero-slots (gui/get-hero-slots v-grid)))
                          (= thing :sign-dismiss) 
                            (when (gui/get-selected-hero @hero-slots)
                              (swap! pnl-state (fn [ps] (assoc ps :state :confirm-dismiss)))) 
                          (= thing :hero-available)
                            (gui/set-selection space)
                          (= thing :hero-slot)
                            (gui/set-selection space))))
                   )))
        ; mouse over
        (fn [x y]
         (let [[space-x space-y btn] (gui/translate-mouse-coords x y)]
            (when-let [sp (grd/get-space v-grid space-x space-y)]
              (let [thing (sp :thing)]
                (cond (= thing :hero-slot)
                      (let [hero (gui/get-space-hero sp @hero-slots)]
                        (when hero
                           (gui/set-status (unt/describe hero ))))
                      (= thing :hero-available)
                        (when-let [hero (sp :unit)]
                          (gui/set-status  (str (hero :description) " for Hire")))
                      (= thing :sign-exit) (gui/set-status "Leave the Village")
                      (= thing :sign-hire) (gui/set-status "Hire the Selected Character")
                      (= thing :sign-dismiss) (gui/set-status "Dismiss the Selected Character")
                      :else
                       (gui/set-status (str thing)))))))

                )))

