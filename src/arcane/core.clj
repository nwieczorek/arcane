(ns arcane.core
  (:import (javax.swing JFrame Timer)
           (java.awt.event KeyListener ActionListener))
  (:require [arcane.worldpanel :as world]
        [arcane.villagepanel :as village ]
        [arcane.entrancepanel :as entrance]
        [arcane.dungeonpanel :as dungeon ]
        [arcane.gui-common :as gui]
        [arcane.tileset :as til ])
  (:use arcane.common ))




(def tileset-list
  '(  [:terrain "/terrain.tileset"]
      [:unit "/characters.tileset"]))


(def ^:dynamic *panels* (atom {}))
(def ^:dynamic *timers* (atom {}))


(defn set-panel
  [kw pnl-v]
  (let [[pnl timer] pnl-v]
    (swap! *panels* (fn [op] (assoc op kw pnl)))
    (swap! *timers* (fn [op] (assoc op kw timer)))
    ))


(def ^:dynamic *lair-index* (atom 0))
(def ^:dynamic *lair-level* (atom 0))
(def ^:dynamic *lairs* (atom {}))
(defn set-lair-index
  [idx level]
  (println "Setting lair index " idx " level " level)
  (reset! *lair-index* idx)
  (reset! *lair-level* level))

(defn get-lair  
  []
  (println "Getting lair for " @*lair-index* " lvl " @*lair-level*)
  (when (not (contains? @*lairs* @*lair-index*))
    (let [dung (dungeon/make-dungeon @*lair-level*)]
      (swap! *lairs* (fn [om] (assoc om @*lair-index* dung)))))
  (@*lairs* @*lair-index*))


(defn arcane-window
  []
    (til/load-tilesets tileset-list)


    (def frame (doto (JFrame. "Arcane")
                 (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE )
                 ))


    (defn set-scene
      [kw]
      (let [p (@*panels* kw)
            t (@*timers* kw)]
        (doseq [every-t (vals @*timers*)] (.stop every-t))
        (gui/clear-selection)
        (.setContentPane frame p)
        (.validate frame)
        (.repaint frame)
        (.start t)))


    (defn change-scene
      ([from signal entry-data]
        (println "changing from " from " to " signal)
          (cond 
            (= from :world) 
                (cond (= signal :village) 
                      (do
                        (set-panel :village (village/village-panel change-scene))
                        (set-scene :village))
                      (= signal :lair) 
                      (do
                        (set-lair-index (entry-data :lair-index) (entry-data :level))
                        (set-panel :entrance (entrance/entrance-panel change-scene))
                        (set-scene :entrance))
                      )
            (= from :village)
                (cond (= signal :exit) (set-scene :world))
            (= from :dungeon)
                (cond (= signal :exit) (set-scene :world))
            (= from :entrance)
                (cond (= signal :exit) (set-scene :world)
                      (= signal :enter) 
                        (let [lair (get-lair)
                              grid (lair :grid)
                              foe-race (lair :foe-race)
                              level (lair :level)]
                          (set-panel :dungeon (dungeon/dungeon-panel change-scene (entry-data :heroes) grid foe-race level))
                          (set-scene :dungeon))
            )))
      ([from signal]
       (change-scene from signal nil)))


    (set-panel :world (world/world-panel change-scene))
    (set-scene :world)
    (.setVisible frame true)
    (let [insets (.getInsets frame)]
      (.setSize frame (+ (.left insets) (.right insets) display-width ) 
                      (+ (.top insets) (.bottom insets) 
                          display-height (* tile-height status-height) )))

    )

(defn main
  []
  (arcane-window))
