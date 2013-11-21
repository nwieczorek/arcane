(ns arcane.dungeonpanel
  (:require [arcane.gui-common :as gui]
            [arcane.tileset :as til]
            [arcane.unit :as unt]
            [arcane.player :as ply]
            [arcane.dungen :as dun]
            [arcane.grid :as grd]) 
  (:use arcane.common))



(def dungeon-thing-map
  { :f :dungeon-floor
    :w :dungeon-wall
    :se :sign-exit
    :se2 :sign-exit
    :sp :sign-pass
    :sp2 :sign-pass
    :hs :hero-slot
   })

(def dungeon-sign-trailers
  '(:se2 :sp2 ))


(def wall-keys
  '( :dungeon-wall-a
     :dungeon-wall-b
     :dungeon-wall-c
     :dungeon-wall-d
     :dungeon-wall-e
     :dungeon-wall-f))

(defn randomize-wall
  []
  (rand-nth wall-keys))  

(defn make-get-dungeon-images
  []
  (let [wall-key (randomize-wall)]
  (fn
    [loc thing ]
    (let [ get-image (fn [kw] (til/get-tileset-image :terrain (gui/simple-layout-decode dungeon-thing-map kw) rand-nth ))
          thing-kw (thing :thing)]
      (cond 
        (= thing-kw :hero-slot)
          [(til/get-tileset-image :terrain :dungeon-floor rand-nth) 
           (til/get-tileset-image :terrain :exit-space rand-nth)] 
        (= thing-kw :dungeon-wall)
            [(til/get-tileset-image :terrain wall-key rand-nth) ] 
        (some #(= loc %) dungeon-sign-trailers) [nil]
        :else
          [(get-image loc)]
        )))))


;================================================================================
(def hero-space-min-x 5)
(def hero-space-max-x 8)
(def hero-space-min-y (- world-height 2))
(def exit-sign-y (- world-height 1))
(def exit-sign-min-x (- world-height 3))
(def exit-sign-max-x (+ exit-sign-min-x 1))

(def pass-sign-y (- world-height 1))
(def pass-sign-min-x 1)
(def pass-sign-max-x (+ pass-sign-min-x 1))

(defn make-dungeon-layout
  []
  (let [d-gen (dun/get-dungeon)]
      (into [] (for [y (range world-height)
                     x (range world-width)]
                  (let [cell ((d-gen :grid) (dun/get-key x y))]
                    (cond (and (>= y hero-space-min-y) 
                               (and (>= x hero-space-min-x) (<= x hero-space-max-x)))
                          :hs
                          (and (= y exit-sign-y)
                               (= x exit-sign-min-x))
                          :se
                          (and (= y exit-sign-y)
                               (= x exit-sign-max-x))
                          :se2
                          (and (= y pass-sign-y)
                               (= x pass-sign-min-x))
                          :sp
                          (and (= y pass-sign-y)
                               (= x pass-sign-max-x))
                          :sp2
                          (= cell 0)
                          :f
                          :else
                          :w))))
    ))
   

(def hero-slot-y-offset 9)

(defn make-hero-spaces 
  [grid entered-heroes]
  (filter #(identity (% :unit))
    (for [s entered-heroes]
      (let [x (s :x)
            y (+ hero-slot-y-offset (s :y))]
      (grd/make-space x y {:unit (s :unit)})))))

(defn make-foe-spaces
  [grid race level]
  (let [foes (unt/get-random-foes race level)
        clear-spaces (take (count foes) (shuffle (grd/get-spaces grid #(= (% :thing) :dungeon-floor))))]
    (map #(assoc %2 :unit %1 ) foes clear-spaces)
    ))



(defn has-unit?
  [space unit-spaces]
  (some #(grd/equal-space space %) unit-spaces))

(defn get-unit
  [space unit-spaces]
  (when-let [sp (first (filter #(grd/equal-space space %) unit-spaces))]
    (sp :unit)))

(defn can-select?
  [space hero-spaces]
  (has-unit? space hero-spaces))


(defn have-selected-hero?
  [hero-spaces]
  (when @gui/*selection*
    (has-unit? @gui/*selection* hero-spaces)))

;================================================================================
; Movement/Attack functions
;================================================================================



(defn update-unit!
  [unit damage unit-spaces]
  (let [new-unit (unt/apply-damage unit damage)]
    (reset! unit-spaces
      (keep identity
          (for [us @unit-spaces]
            (let [sp-u (us :unit)]
              (if (unt/unit-equals new-unit sp-u) 
                (if (<= (new-unit :health) 0)
                  nil
                  (assoc us :unit new-unit))
                us)))))))

(defn make-space-in?
  [space-list]
  (fn [space]
    (some #(grd/equal-space % space) @space-list)))


(defn- passable-terrain
  [sp]
  (let [th (sp :thing)]
    (or (= :dungeon-floor th)
        (= :hero-slot th))))

(defn- make-passable-func
  [hero-spaces foe-spaces]
  (fn 
    [sp]
    (and (passable-terrain sp )
         (not (has-unit? sp hero-spaces))
         (not (has-unit? sp foe-spaces)))))

(defn- make-attackable-func
  [unit-spaces]
  (fn
    [sp]
    (has-unit? sp unit-spaces)))

(defn- set-unit-status!
  [u-sp unit-spaces new-status]
    (let [unit-space (grd/find-space @unit-spaces u-sp)
          u (unit-space :unit)]
      (reset! unit-spaces (for [sp @unit-spaces]
                             (if (grd/equal-space sp unit-space)
                               (assoc sp :unit (assoc u :status new-status ))
                               sp)))
      ))

(defn- set-unit-unready!
  [u-sp unit-spaces]
   (set-unit-status! u-sp unit-spaces :unready))

(defn- get-actions-from-path
  [path action-type]
  (loop [prev (first path)
        destinations (rest path)
         actions '()]
    (if (empty? destinations)
      (reverse actions)
      (let [curr (first destinations)
            rest-dest (rest destinations)]
        (if (empty? rest-dest)
          (recur curr rest-dest (conj actions {:action-type action-type :to curr :from prev :final true}))
          (recur curr rest-dest (conj actions {:action-type action-type :to curr :from prev}))
          )
        ))))

(defn- start-hero-move
  [grid origin target hero-spaces foe-spaces action-queue]
  (when-let [u (get-unit origin @hero-spaces)]
    (let [path (grd/get-path grid origin target (make-passable-func @hero-spaces foe-spaces) (u :move))]
      (reset! action-queue (get-actions-from-path path :move-hero))
    )))


(def clear-effect-action  {:action-type :clear-effects})


(defn get-attack-actions
  ([attacker-key grid origin target attacker-spaces target-spaces attacker]
    (let  [direction (grd/get-direction origin target) 
          ;do not want the target space in this, so make-attackable-func always returns false!
          line (grd/get-line grid origin direction  
                             (make-passable-func @attacker-spaces @target-spaces)
                             (fn [sp] false)
                             (attacker :range))
          m-key (gui/get-missile-key (attacker :missile) direction) 
          action-line (map (fn [sp] {:action-type :effect :to sp :thing m-key}) line)]
      (concat action-line
              (list {:action-type (keyword-append :attack attacker-key) :to target :from origin}
                    clear-effect-action ))))
  ([attacker-key grid origin target attacker-spaces target-spaces]
  (let [attacker-sp (grd/find-space @attacker-spaces origin)
        attacker (attacker-sp :unit)]
    (get-attack-actions attacker-key grid origin target attacker-spaces target-spaces attacker))))


(defn- start-hero-attack
  [grid origin target hero-spaces foe-spaces action-queue] 
  (reset! action-queue (get-attack-actions :hero grid origin target hero-spaces foe-spaces)))

;================================================================================
; Foe Actions
;================================================================================

(defn ranged?
  [u]
  (> (u :range) 1))

(defn rank-foe-move
  [grid foe target-space hero-spaces nearest-hero foe-spaces]
  (let [distance-base (quot world-width 2)
        adj (grd/get-adjacent grid target-space true)
        adj-heroes (keep identity (map (fn [a-sp] (get-unit a-sp @hero-spaces)) adj))
        center-factor (- distance-base (grd/distance-from-center grid target-space))
        nearest-hero-factor (if nearest-hero 
                              (- distance-base (grd/get-distance target-space nearest-hero))
                              0)
        can-attack-factor (count (grd/get-attackable grid target-space  (make-passable-func @hero-spaces @foe-spaces) (make-attackable-func @hero-spaces) (foe :range)))
        adj-factor (count adj-heroes)
        adj-weight (if (ranged? foe) -10 0)
        nearest-hero-weight (if (ranged? foe) 5 10)
        can-attack-weight 100
        ]
    (+ center-factor 
       (* nearest-hero-weight nearest-hero-factor)
       (* adj-weight adj-factor)
       (* can-attack-weight can-attack-factor)
       )))

(defn get-nearest-hero
  [grid foe-space hero-spaces]
  (second (first (sort-by first (map (fn [hs] [(grd/get-distance foe-space hs) hs]) hero-spaces)))))

(defn get-foe-actions
  [grid foe-space hero-spaces foe-spaces]
  (let [foe (foe-space :unit)
        nearest-hero (get-nearest-hero grid foe-space @hero-spaces)
        p-func (make-passable-func @hero-spaces @foe-spaces)
        reach-spaces (grd/get-reachable grid foe-space  p-func(foe :move))
        ranked-dests (for [sp reach-spaces]
                          [(rank-foe-move grid foe sp hero-spaces nearest-hero foe-spaces) sp])
        dest (second (first (reverse (sort-by first ranked-dests)))) 
        path (grd/get-path grid foe-space dest p-func (foe :move))

        targets (grd/get-attackable grid dest p-func  (make-attackable-func @hero-spaces) (foe :range))
        target (when (not-empty targets) (rand-nth targets))
        attack-actions (if target (get-attack-actions :foe grid dest target foe-spaces hero-spaces foe) '())
        ]


    (concat (get-actions-from-path path :move-foe)
            attack-actions)

    )) 
        
        
(defn start-foe-actions
  [grid foe-space foe-spaces hero-spaces action-queue]
  (let [foe (foe-space :unit)
        foe-actions (get-foe-actions grid foe-space hero-spaces foe-spaces)]
      (reset! action-queue foe-actions)
      (set-unit-unready! foe-space foe-spaces)  
    ))



;================================================================================
;Action Resolution
;================================================================================


(defn- move-unit
  [origin target unit-spaces]
        (reset! unit-spaces (conj (remove #(grd/equal-space % origin) @unit-spaces)
                                  (assoc target :unit (get-unit origin @unit-spaces )))))

(defn- do-attack
  [ action attacker-spaces target-spaces effects]
  (let [attacker-sp (grd/find-space @attacker-spaces (action :from))
         attacker (attacker-sp :unit)
         target-sp (grd/find-space @target-spaces (action :to))
         target (target-sp :unit)
        rng 1 ;TODO
        damage (unt/get-damage attacker target rng)]

      (update-unit! target damage target-spaces)
      (set-unit-unready! attacker-sp attacker-spaces)
      (gui/clear-selection)  
      (reset! effects (list (assoc target-sp :thing (attacker :effect))))))

(defn- do-action
  [action grid hero-spaces foe-spaces attackable effects pnl-state]
  (case (action :action-type)
    :move-hero  (do 
                  (move-unit (action :from) (action :to) hero-spaces)
                  (gui/set-selection (action :to))
                  (set-unit-status! (action :to) hero-spaces :moved)
                  (reset! attackable '())
                  (when (action :final)
                    (let [new-space (grd/find-space @hero-spaces (action :to))
                          u (new-space :unit)
                          new-attackable (grd/get-attackable grid (action :to) 
                                                           (make-passable-func @hero-spaces @foe-spaces) 
                                                           (make-attackable-func @foe-spaces) 
                                                           (u :range))]
                      (if (not-empty new-attackable)
                        (reset! attackable new-attackable)
                        (do
                          (reset! attackable '())
                          (set-unit-unready! new-space hero-spaces)
                          (gui/clear-selection))))))

    :move-foe   (do
                  (move-unit (action :from) (action :to) foe-spaces)
                  (gui/set-selection (action :to)))


    :attack-hero (do-attack action  hero-spaces foe-spaces effects)

    :attack-foe (do-attack action  foe-spaces hero-spaces effects)

    :effect (reset! effects (list (assoc (action :to) :thing (action :thing))))
    :clear-effects (do 
                     (reset! effects '())
                      (cond
                        (empty? @hero-spaces)
                        (gui/to-state pnl-state :no-heroes)
                        (empty? @foe-spaces)
                        (gui/to-state pnl-state :no-foes)
                        ))
    :no-op nil)
  )

;================================================================================
; Turn Maintenance
;================================================================================

(defn start-unit-turn
  [unit-spaces]
  (for [sp unit-spaces]
    (let [u (sp :unit)
          new-u (assoc u :status :ready)]
      (assoc sp :unit new-u))))

(defn end-unit-turn
  [unit-spaces]
  (for [sp unit-spaces]
    (let [u (sp :unit)
          new-u (assoc u :status :unready)]
      (assoc sp :unit new-u))))
  


(defn next-ready
  [unit-spaces]
  (let [ready-spaces (filter (fn [sp] (= :ready ((sp :unit) :status))) unit-spaces)]
    (when (not-empty ready-spaces)
      (first ready-spaces))))


(defn all-unready?
  [unit-spaces]
  (let [unready-spaces (filter (fn [sp] (= :unready ((sp :unit) :status))) unit-spaces)]
    (= (count unit-spaces) (count unready-spaces))))


(defn switch-turn
  [new-state pnl-state hero-spaces foe-spaces]
  (cond (= new-state :heroes)
        (do
          (swap! pnl-state (fn [ps] (assoc ps :acting :heroes)))
          (swap! hero-spaces (fn [hs] (start-unit-turn hs)))
          (gui/clear-selection))
          
        (= new-state :foes)
        (do
          (swap! pnl-state (fn [ps] (assoc ps :acting :foes)))
          (swap! foe-spaces (fn [fs] (start-unit-turn fs))))
        :else (println "Invalid state: " new-state)))


(defn all-exiting?
  "Are all the heroes at the exit"
  [grid hero-spaces]
  (empty? (filter #(not= :hero-slot %) (map #( (grd/get-space  grid %) :thing) hero-spaces))))

(defn find-dead-heroes
  [original-hero-spaces hero-spaces]
  (keep identity
    (for [orig-hero-sp original-hero-spaces]
      (let [orig-hero (orig-hero-sp :unit)]
        (when (not (some #(unt/unit-equals orig-hero (% :unit)) hero-spaces))
          orig-hero)))))


;================================================================================
; Main Panel functions
;================================================================================
(defn have-selected-hero?
  [hero-spaces]
  (when @gui/*selection*
    (has-unit? @gui/*selection* hero-spaces)))


(defn- do-select
  [grid space hero-spaces foe-spaces reachable attackable]
  (when-let [u (get-unit space @hero-spaces)]
    (when (have-selected-hero? @hero-spaces)
      (when-let [sel-sp (gui/get-from-spaces @gui/*selection* @hero-spaces)]
        (when-let [sel-u (sel-sp :unit)]
          (when (= (sel-u :status) :moved)
            (set-unit-unready! sel-sp hero-spaces)))))
    (let [new-reach (if (= :ready (u :status))
                      (grd/get-reachable grid space (make-passable-func @hero-spaces foe-spaces) (u :move))
                      '())
          new-attackable (if (= :ready (u :status))
                           (grd/get-attackable grid space (make-passable-func @hero-spaces foe-spaces) (make-attackable-func foe-spaces) (u :range))
                           '())
                           ]

      (reset! reachable new-reach)
      (reset! attackable new-attackable)
      (gui/set-selection space))))


(defn make-dungeon
  [level]
  (let [dungeon-layout (make-dungeon-layout)
        image-func (make-get-dungeon-images)
        d-grid (grd/make-grid dungeon-layout 
                                (fn [x y loc] 
                                  (let [things (gui/simple-layout-thing dungeon-thing-map loc)]
                                    (merge 
                                      things 
                                      {:images (image-func loc things)}
                                      ))))
        foe-race (unt/get-foe-race level)]
    {:grid d-grid
     :foe-race foe-race
     :level level}
    ))


(defn dungeon-panel
  [scene-func entered-heroes d-grid foe-race level]
  (let [pnl-state (atom {:state :ready  :acting :foes})
        original-hero-spaces (make-hero-spaces d-grid entered-heroes) 
        hero-spaces (atom (end-unit-turn (make-hero-spaces d-grid entered-heroes)))
        foe-spaces (atom (start-unit-turn (make-foe-spaces d-grid foe-race level)))
        reachable-spaces (atom '())
        attackable-spaces (atom '())
        action-queue (atom '())
        effects (atom '())
        dialogue (gui/make-dialogue :ok )
        can-move? (make-space-in? reachable-spaces)
        can-attack? (make-space-in? attackable-spaces)
        clear-ables (fn []
                        (reset! reachable-spaces '())
                        (reset! attackable-spaces '()))
        clear-selection (fn [] 
                          (gui/clear-selection)
                          (clear-ables))
        save-and-exit (fn [] 
                        (let [dead-heroes (find-dead-heroes original-hero-spaces @hero-spaces)]
                          (ply/player-remove-dead! dead-heroes))
                        (ply/player-update-heroes! @hero-spaces)
                        (swap! hero-spaces unt/hero-leveling)
                        (scene-func :dungeon :exit))
                      ]

  (gui/simple-panel
        ;====== get images ======
        (fn []
          (concat
            (gui/get-grid-grob-list d-grid)
            (gui/get-status-grob-list d-grid)
            (gui/get-selection-grob-list)
            (gui/get-reachable-grob-list @reachable-spaces)
            (gui/get-attackable-grob-list @attackable-spaces)
            (gui/get-unit-spaces-grob-list @hero-spaces true)
            (gui/get-unit-spaces-grob-list @foe-spaces)
            (gui/get-effect-grob-list @effects)
            (cond (= (@pnl-state :state) :cannot-exit)
                  (gui/get-dialogue-grob-list dialogue ["All Heroes Must Move to the Exit"])
                  (= (@pnl-state :state) :no-heroes)
                  (gui/get-dialogue-grob-list dialogue ["All Heroes Have Been Slain!"])
                  (= (@pnl-state :state) :no-foes)
                  (gui/get-dialogue-grob-list dialogue ["All Enemies Have Been Slain!"])
              :else [])
            ))
        ;====== mouse click ======
        (fn [x y btn] 
           (let [[space-x space-y btn] (gui/translate-mouse-coords x y)]
             (cond (= (@pnl-state :state) :cannot-exit)
                   (when-let [space (gui/get-dialogue-space dialogue space-x space-y)]
                       (let [thing (space :thing)] 
                         (when (= thing :sign-ok)
                               (gui/to-ready-state pnl-state))))
                   (or (= (@pnl-state :state) :no-heroes) (= (@pnl-state :state) :no-foes))
                   (when-let [space (gui/get-dialogue-space dialogue space-x space-y)]
                       (let [thing (space :thing)] 
                         (when (= thing :sign-ok)
                               (save-and-exit))))

                   :else
                   (when-let [space (grd/get-space d-grid space-x space-y)]
                      (let [thing (space :thing)]
                         (cond
                           (= thing :sign-exit) (if (all-exiting? d-grid @hero-spaces) 
                                                  ;TODO if exiting, do one more foe turn
                                                  (save-and-exit)
                                                  (gui/to-state pnl-state :cannot-exit))
                           (= thing :sign-pass) (when (have-selected-hero? @hero-spaces)
                                                  (set-unit-unready! @gui/*selection* hero-spaces)
                                                  (clear-selection))
                           (not= thing :dungeon-wall)
                           (cond (and (have-selected-hero? @hero-spaces) (can-move? space ))
                                 (do
                                   (start-hero-move d-grid @gui/*selection* space hero-spaces @foe-spaces action-queue)
                                   (clear-ables))

                                 (and (have-selected-hero? @hero-spaces) (can-attack? space))
                                 (do 
                                   (start-hero-attack d-grid @gui/*selection* space hero-spaces foe-spaces action-queue)
                                   (clear-ables))

                                 (can-select? space @hero-spaces)
                                 (do-select d-grid space hero-spaces @foe-spaces reachable-spaces attackable-spaces)
                                 )
                           :else (println thing "," btn)
                       ))))))
        ;====== mouse over ======
        (fn [x y]
         (let [[space-x space-y btn] (gui/translate-mouse-coords x y)]
            (when-let [sp (grd/get-space d-grid space-x space-y)]
              (let [hero (get-unit sp @hero-spaces)
                    foe (get-unit sp @foe-spaces)]
                (cond hero 
                      (gui/set-status (unt/status-describe hero))
                      foe
                      (gui/set-status (unt/status-describe foe))
                      :else
                      (let [thing (sp :thing)]
                         (gui/set-status (str thing))))))))
        ;timer
        (fn []
          (cond (not-empty @action-queue)
                (let [action (first @action-queue)]
                  (reset! action-queue (rest @action-queue))
                  (do-action action d-grid hero-spaces foe-spaces attackable-spaces effects pnl-state))
                (= (@pnl-state :acting) :foes)
                (if-let [ready-sp (next-ready @foe-spaces)]  
                  (start-foe-actions d-grid ready-sp foe-spaces hero-spaces action-queue)
                  (switch-turn :heroes pnl-state hero-spaces foe-spaces))
                (= (@pnl-state :acting) :heroes)
                (when (all-unready? @hero-spaces)
                  (switch-turn :foes pnl-state hero-spaces foe-spaces))
                ))
      )))

