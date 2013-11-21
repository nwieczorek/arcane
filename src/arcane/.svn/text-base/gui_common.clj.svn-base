(ns arcane.gui-common
  (:import (javax.swing JPanel Timer )
           javax.imageio.ImageIO
           (java.awt.event ActionListener MouseListener MouseMotionListener MouseEvent KeyListener)
           (java.awt Color RenderingHints Toolkit Image Font))
  (:require [arcane.player :as ply]
            [arcane.tileset :as til]
            [arcane.grid :as grd])
  (:use arcane.common))

;========================================================================

(def colors
  { :white Color/WHITE
    :yellow Color/YELLOW
    :black Color/BLACK
    :red Color/RED
    :green Color/GREEN
    :blue Color/BLUE}) 



;========================================================================
; Status 

(def ^:dynamic *status* (atom {:text [""  ""]}))
(def status-lines 2)

(defn get-status
  [index]
  ((:text @*status*) index))

(defn set-status
  ([msg1 msg2]
    (swap! *status* (fn [orig] (assoc orig :text [msg1 msg2]))))
  ([msg]
   (if (vector? msg)
    (set-status (msg 0) (msg 1))
    (set-status msg  "")))

  )


;========================================================================
; Selection
(def ^:dynamic *selection* (atom nil))

(defn set-selection
  [sp]
  (reset! *selection* sp))

(defn clear-selection
  []
  (reset! *selection* nil))


;========================================================================
; General Functions
(defn get-text-height
  [^java.awt.Graphics2D g2d]
  (let [frc (.getFontRenderContext g2d)
        lm (.getLineMetrics (.getFont g2d), "A", frc )]
    (int (.getAscent lm))))


(defn mouse-event-data
  [e]
  (let [btn (.getButton e)
       x (.getX e)
       y (.getY e)
       btn-key (cond (= btn MouseEvent/BUTTON1) :left
                    (= btn MouseEvent/BUTTON3) :right
                    :else btn)]
  [x y btn-key])) 

(defn translate-mouse-coords
  [x y]
    (let [tile-x (quot x tile-width)
         tile-y (quot y tile-height)]
    [tile-x tile-y])) 


(defn translate-space-to-coord
  [space-x space-y]
    [(* space-x tile-width) (* space-y tile-height)])

(defn simple-layout-decode
  [thing-map kw]
  (let [th (thing-map kw)]
    (assert th (str "No mapping for " kw))
    th))

(defn simple-layout-thing
  [thing-map kw]
  (let [th (simple-layout-decode thing-map kw)]
    {:thing th}))

(defn get-text-grob
  [text log-x log-y color size log-y-offset]
  (let [text-line-height (quot tile-height 2)
        [x y1] (translate-space-to-coord log-x  log-y )
        y (+ y1 (* log-y-offset text-line-height))]
    {:type :text :x (+ x text-inset) :y (+ y text-inset) :text text :color color :size size}))

(defn get-image-grob
  ([image log-x log-y]
    (let [[x y] (translate-space-to-coord log-x log-y)]
      {:type :image :image image :x x :y y})) 
  ([image log-x log-y x-offset y-offset]
    (let [[x-o y-o] (translate-space-to-coord log-x log-y)
          x (+ x-offset x-o)
          y (+ y-offset y-o)]
      {:type :image :image image :x x :y y})) 
  )


(defn get-grid-grob-list
  ([grid x-offset y-offset]
  (let [grid-height (:height grid)
        grid-width (:width grid)]
    ;draw the grid
    (flatten
      (concat 
        ;first image
        (for [gs (grid :grid)]
          (let [images (gs :images)] 
            (when (not-empty  images)
              (get-image-grob (first images) (+ x-offset (:x gs)) (+ y-offset (:y gs))))))
        ;second to nth images
        (for [gs (grid :grid)]
          (let [images (gs :images)] 
            (for [img (rest images)]
              (get-image-grob img (+ x-offset (:x gs)) (+ y-offset (:y gs))))))
        ))))
  ([grid ]
   (get-grid-grob-list grid 0 0)))


(defn get-selection-grob-list
  []
    (list (let [ss @*selection*
                image (til/get-tileset-image :terrain :selection)
                ;x-offset (til/get-tileset-image :terrain :selection-x-offset)
                ;y-offset (til/get-tileset-image :terrain :selection-y-offset)
                ]
        (when (and ss image) 
            (get-image-grob image (:x ss) (:y ss))))))

(defn get-reachable-grob-list
  [reachable ]
  (let [image (til/get-tileset-image :terrain :reachable)]
    (for [rsp reachable]
      (get-image-grob image (rsp :x) (rsp :y)))))

(defn get-attackable-grob-list
  [attackable]
  (let [image (til/get-tileset-image :terrain :attackable)]
    (for [rsp attackable]
      (get-image-grob image (rsp :x) (rsp :y)))))

(defn get-effect-grob-list
  [effects]
  (for [ef effects]
   (get-image-grob (til/get-tileset-image :unit (ef :thing) rand-nth) (ef :x) (ef :y))))   

(defn get-status-grob-list
  [grid ]
  (let [grid-height (:height grid)
        grid-width (:width grid)]
    (concat 
      ;draw status background
      (flatten
        (for [ws-y (range grid-height (+ grid-height status-height))] 
          (for [ws-x (range grid-width)]
            (let [image (til/get-tileset-image :terrain :status) ]
              (get-image-grob image ws-x ws-y)))))
      ;draw status text
      (list 
        (get-text-grob (get-status 0) 0 grid-height :yellow status-font-size 0) 
        (get-text-grob (get-status 1) 0 grid-height :yellow status-font-size 1) 
        (get-text-grob (str "Gold: " (ply/get-player :gold)) 0 grid-height :yellow status-font-size 2) )
    )))

;========================================================================
;  Panel Status
;

(defn make-panel-state
  []
  (atom {:state :ready}))




(defn to-ready-state
  [state-var]
 (swap! state-var (fn [ps] (assoc ps :state :ready))))

(defn to-state
  [state-var new-state]
 (swap! state-var (fn [ps] (assoc ps :state new-state))))



;========================================================================
(defn get-hero-slots
  [grid]
  (let [slots (grd/get-spaces grid #(= (% :thing) :hero-slot))
        heros (ply/player-get-heroes )
        pad-heros (concat heros (repeat (- (count slots) (count heros)) nil))]
    (map (fn [sl u] 
             (assoc sl :unit u))
           slots pad-heros)))



(defn set-slot-hero
  [hero-slots space-x space-y hero]
  (for [slot hero-slots]
    (if (and (= space-x (slot :x)) (= space-y (slot :y)))
      (assoc slot :unit hero)
      slot)))

(defn get-from-spaces
  [space spaces]
  (some (fn [s] (when (grd/equal-space space s) s)) spaces))


(defn get-space-hero 
  [space hero-spaces]
  (when-let [u-space (get-from-spaces space hero-spaces)]
      (u-space :unit )))

(defn get-selected-hero 
  [hero-spaces]
  (get-space-hero @*selection* hero-spaces))
             
(defn get-unit-spaces-grob-list
  ([unit-spaces show-status] 
   (flatten
    (for [sp unit-spaces]
      (when-let [u (sp :unit)]
        (let [image (til/get-tileset-image :unit (u :template-id) first)
                   u-grob (get-image-grob image (sp :x) (sp :y))]
          (if (and show-status (not (= (u :status) :ready)))
            (let [ready-img (til/get-tileset-image :unit :unready first)
                  ready-grob (get-image-grob ready-img (sp :x) (sp :y))]
              (list u-grob ready-grob))
            u-grob
            ))))))
  ([unit-spaces]
   (get-unit-spaces-grob-list unit-spaces false)))


(defn selection-in
  [slots]
  (and @*selection*
       (some #(grd/equal-space @*selection* %) slots)))


(defn dismiss-hero
  [hero-spaces]
  (when-let [hero (get-selected-hero hero-spaces)]
    (ply/player-dismiss-hero (hero :id))))



(defn get-missile-key
  [base-key direction]
  (keyword-append base-key (direction :kw)))


;========================================================================
; Dialogue Functions
;
(def dialogue-layout
  { :yes-no
    [ :dtl :dt :dt :dt :dt :dt :dt :dtr
     :dl :d :d :d :d :d :d :dr
     :dl :d :d :d :d :d :d :dr
     :dl :d :d :d :d :d :d :dr
     :dl :sy :sy2 :d :d :sn :sn2 :dr 
     :dbl :db :db :db :db :db :db :dbr
    ]
    :ok 
    [ :dtl :dt :dt :dt :dt :dt :dt :dtr
     :dl :d :d :d :d :d :d :dr
     :dl :d :d :d :d :d :d :dr
     :dl :d :d :d :d :d :d :dr
     :dl :d :d :so :so2 :d :d :dr 
     :dbl :db :db :db :db :db :db :dbr
    ]
  } 
   )

(def dialogue-sign-trailers
  '(:sy2 :sn2 :so2))

(def dialogue-thing-map
  { :d :dialogue
   :dr :dialogue-border-right
   :dl :dialogue-border-left
   :db :dialogue-border-bottom
   :dt :dialogue-border-top
   :dtl :dialogue-border-top-left
   :dtr :dialogue-border-top-right
   :dbl :dialogue-border-bottom-left
   :dbr :dialogue-border-bottom-right
   :sy :sign-yes
   :sy2 :sign-yes
   :so :sign-ok
   :so2 :sign-ok
   :sn :sign-no
   :sn2 :sign-no})


(defn get-dialogue-images
  [loc thing ]
  (let [ get-image (fn [kw] (til/get-tileset-image :terrain (thing :thing) rand-nth ))]
    (cond 
      (some #(= loc %) dialogue-sign-trailers) [nil]
      :else [(get-image loc)]
      )))


(defn make-dialogue
  [button-key]
  (let [d-grid (grd/make-grid (dialogue-layout button-key)
                          (fn [x y loc] 
                            (let [things (simple-layout-thing dialogue-thing-map loc)]
                             (merge
                              things 
                              {:images (get-dialogue-images loc things) }))) 
                               dialogue-width dialogue-height  )]
  {:grid d-grid }
  ))

(defn get-dialogue-grob-list
  "Takes a message to display as a vector of strings"
  [dlog text-lines]
  (concat (get-grid-grob-list (dlog :grid) dialogue-x-offset dialogue-y-offset)
         (for [i (range (count text-lines))] 
          (get-text-grob (text-lines i) (+ 1 dialogue-x-offset) (+ 1 dialogue-y-offset) 
                         dialogue-font-color dialogue-font-size i)))) 

(defn get-dialogue-space
  [dlog raw-x raw-y]
  (grd/get-space (dlog :grid) (- raw-x dialogue-x-offset) (- raw-y dialogue-y-offset)))
;========================================================================

(defn noop-timer-func
  []
  nil)

(defn simple-panel

  ([grob-list-func space-click-func mouse-over-func timer-func] 

    (defn draw-text
      [^java.awt.Graphics2D g2d text x y1 color-key size]
        (.setColor g2d (colors color-key))
        (.setFont g2d (Font. font-family (. Font PLAIN) size))
          (let [text-height (get-text-height g2d) 
                y (+ text-height y1 )]
           (.drawString g2d text x y)))

      (let [
            pnl (doto (proxy [javax.swing.JPanel] []
                  (paintComponent [^java.awt.Graphics g]
                    (proxy-super paintComponent g)
                    (let [g2d (doto ^java.awt.Graphics2D 
                                (.create g))]

                      (doseq [grob (keep identity (grob-list-func))]
                        (cond (= (grob :type) :image)
                              (.drawImage g2d (grob :image) (grob :x) (grob :y) this)
                              (= (grob :type) :text)
                              (draw-text g2d (grob :text) (grob :x) (grob :y) (grob :color) (grob :size))))
                      )))


                  (.addMouseListener (proxy [MouseListener] []
                                       (mouseClicked [e] )
                                       (mouseEntered [e] )
                                       (mouseExited [e] )
                                       (mousePressed [e] )
                                       (mouseReleased [e] 
                                         (let [[x y btn] (mouse-event-data e)]
                                                    (space-click-func x y btn)))
                                       ))
                  (.addMouseMotionListener (proxy [MouseMotionListener] []
                                       (mouseDragged [e])
                                       (mouseMoved [e] 
                                         (let [[x y btn] (mouse-event-data e)]
                                           (mouse-over-func x y)))
                                       )))

            timer (Timer. timer-milliseconds 
                           (proxy [ActionListener] []
                              (actionPerformed [event] 
                               (timer-func) 
                               (.repaint pnl))))]
        [pnl  timer]
        ))
  ([grob-list-func space-click-func mouse-over-func]
   (simple-panel grob-list-func space-click-func mouse-over-func noop-timer-func))
  )





