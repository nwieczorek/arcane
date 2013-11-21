(ns arcane.entrancepanel
  (:require [arcane.gui-common :as gui]
            [arcane.tileset :as til]
            [arcane.unit :as unt]
            [arcane.grid :as grd])
  (:use arcane.common))


(def entrance-layout
  [ :ew :ew :ew :ew :ew :e  :e2 :e2 :e2  :ew :ew :ew :ew :ew
    :ew :ew :ew :ew :ew :e2 :e2 :e2 :e2 :ew :ew :ew :ew :ew
    :d  :d  :d  :d  :d  :d  :d  :d  :d  :d  :d  :d  :d  :d 
    :d :d  :sa  :sa2  :d  :es  :es  :es  :es  :d :sn  :sn2  :d  :d
    :d :d  :d  :d  :d  :es  :es  :es  :es  :d :d  :d  :d  :d
    :d :d  :d  :d  :d  :d  :d  :d  :d  :d  :d  :d  :d  :d
    :d :d  :d  :d  :d  :d  :d  :d  :d  :d  :d  :d  :d  :d
    :d :d  :d  :d  :hs  :hs  :hs  :hs  :hs  :hs  :d  :d  :d  :d
    :d :d  :d  :d  :hs  :hs  :hs  :hs  :hs  :hs  :d  :d  :d  :d
    :d :d  :d  :d  :d  :d  :d  :d  :d  :d  :d  :d  :d  :d
    :d :d  :d  :d  :d  :d  :d  :d  :d  :d  :d  :d  :d  :d
    :d :d  :d  :d  :d  :d  :d  :d  :d  :d  :d  :d  :d  :d
    :d :d  :d  :d  :d  :d  :d  :d  :d  :d  :d  :d  :d  :d
    :d :d :d  :d  :d  :d  :d  :d  :d  :d  :d  :se  :se2  :d ])


(def entering-slot-min-y 3)

(def entrance-thing-map
  { :ew :entrance-wall
    :e :entrance
    :e2 :entrance
    :d :dirt
    :se :sign-exit
    :se2 :sign-exit
    :sa :sign-auto
    :sa2 :sign-auto
    :sn :sign-enter
    :sn2 :sign-enter
    :hs :hero-slot
    :es :entering-slot
   })

(def entrance-nil-trailers
  '(:se2 :e2 :sn2 :sa2))

(defn get-entrance-images
  [loc thing]
  (let [get-image (fn [kw] (til/get-tileset-image :terrain (gui/simple-layout-decode entrance-thing-map kw) rand-nth ))
        thing-kw (thing :thing)]
    (cond 
      (some #(= loc %) entrance-nil-trailers) [nil]
      (= thing-kw :entering-slot)
        [(til/get-tileset-image :terrain :dirt rand-nth) (til/get-tileset-image :terrain :entering-slot rand-nth)] 
      (= thing-kw :hero-slot)
        [(til/get-tileset-image :terrain :dirt rand-nth) (get-image loc)] 
      (= thing-kw :dirt)
        (let [thing-die (rand-int 10)]
          (if (grd/between 7 9 thing-die)
            [(get-image loc) (til/get-tileset-image :terrain :dirt-doodad rand-nth)]
            [(get-image loc)]))
      :else
        [(get-image loc)]
      )))


;================================================================================
(defn get-entering-slots
  [grid]
  (map #(assoc % :unit nil) (grd/get-spaces grid #(= (% :thing) :entering-slot))))



(defn move-hero
  "Move the hero in the from-space to the to-space, in the hero-slot-lists.
  From-slots and to-slots are atoms containing space lists"
  [from-slots from-space to-slots to-space]
  (when (not (gui/get-space-hero to-space @to-slots))
    (when-let [hero (gui/get-space-hero from-space @from-slots)]
      (swap! to-slots #(gui/set-slot-hero % (to-space :x) (to-space :y) hero)) 
      (swap! from-slots #(gui/set-slot-hero % (from-space :x) (from-space :y) nil))
      (gui/clear-selection))))

(defn can-move-from-selection
  [slots]
  (and (gui/selection-in slots)
       (gui/get-selected-hero slots)))


(defn deploy
  [unit-list space-list from-slots to-slots]
  (loop [ ulist unit-list
         splist space-list]
    (when (and (not-empty ulist) (not-empty splist))
      (move-hero from-slots (first ulist) to-slots (first splist))
      (recur (rest ulist) (rest splist)))))

(defn auto-deploy
  [hero-slots entering-slots]
  (let [row-map (group-by #(% :y) (filter #(nil? (% :unit)) @entering-slots))
        front-row (row-map entering-slot-min-y)
        back-row (row-map (inc entering-slot-min-y)) 
        hero-map (group-by #(case ((% :unit) :range) 1 :close :ranged) (filter #(% :unit) @hero-slots))
        front-heroes (if (hero-map :close)
                       (take (count front-row) (shuffle (hero-map :close)))
                       '())
        back-heroes (if (hero-map :ranged)
                      (take (count back-row) (shuffle (hero-map :ranged)))
                      '())
        dsc-f (fn [sp] (let [u (sp :unit)] (str (unt/describe u) ":" (u :range)   )))
        ]
    (println "front: " (map dsc-f front-heroes)) 
    (println "back: " (map dsc-f back-heroes)) 
    (deploy front-heroes front-row hero-slots entering-slots)
    (deploy back-heroes back-row hero-slots entering-slots)
    ))


(defn have-deployed?
  [entering-slots]
  (not-empty (keep identity (map #(% :unit) entering-slots))))
          

;================================================================================
(defn entrance-panel
  [scene-func]
  (let [e-grid (grd/make-grid entrance-layout 
                                (fn [x y loc] 
                                  (let [things (gui/simple-layout-thing entrance-thing-map loc)] 
                                    (merge things
                                           {:images (get-entrance-images loc things)}
                                           ))))
        hero-slots (atom (gui/get-hero-slots e-grid))
        entering-slots (atom (get-entering-slots e-grid))
        pnl-state (gui/make-panel-state)
        dialogue (gui/make-dialogue :ok )]

  (gui/simple-panel 
        ;get images
        (fn []
          (concat
            (gui/get-grid-grob-list e-grid )
            (gui/get-status-grob-list e-grid )
            (gui/get-selection-grob-list)
            (gui/get-unit-spaces-grob-list @hero-slots )
            (gui/get-unit-spaces-grob-list @entering-slots )
            (if (= (@pnl-state :state) :cannot-enter)
                (gui/get-dialogue-grob-list dialogue ["Deploy Your Heroes to Enter the Lair"])
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
                   (when-let [ space (grd/get-space e-grid space-x space-y)]
                      (let [thing (space :thing)]
                          (cond 
                            (= thing :sign-exit) (scene-func :entrance :exit nil)
                            (= thing :sign-enter) 
                              (if (have-deployed? @entering-slots) 
                                (scene-func :entrance :enter {:heroes @entering-slots })
                                (gui/to-state pnl-state :cannot-enter))
                            (= thing :sign-auto) (auto-deploy hero-slots entering-slots)
                            (= thing :entering-slot)
                              (cond 
                                (can-move-from-selection @hero-slots)
                                (move-hero hero-slots @gui/*selection* entering-slots space)
                                (can-move-from-selection @entering-slots)
                                (move-hero entering-slots @gui/*selection* entering-slots space)
                                :else
                                (gui/set-selection space))
                            (= thing :hero-slot)
                              (cond 
                                (can-move-from-selection @entering-slots)
                                (move-hero entering-slots @gui/*selection* hero-slots space)
                                :else
                                (gui/set-selection space)))))
               )))
        ;mouse over
        (fn [x y]
         (let [[space-x space-y btn] (gui/translate-mouse-coords x y)]
            (when-let [sp (grd/get-space e-grid space-x space-y)]
              (let [thing (sp :thing)]
                (cond (= thing :hero-slot)
                      (when-let [hero (gui/get-space-hero sp @hero-slots)]
                        (gui/set-status (unt/describe hero)))
                      (= thing :entering-slot)
                      (when-let [hero (gui/get-space-hero sp @entering-slots)]
                        (gui/set-status (unt/describe hero)))
                      (= thing :sign-exit) (gui/set-status "Leave the Lair Entrance")
                      (= thing :sign-enter) (gui/set-status "Enter the Lair")
                      (= thing :sign-auto) (gui/set-status "Automatically Deploy your Heroes")
                      :else
                       (gui/set-status (str thing)))))))

                )))

