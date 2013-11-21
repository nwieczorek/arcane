(ns arcane.tileset
  (:import (java.io BufferedReader FileReader InputStreamReader)
           javax.imageio.ImageIO)
  (:use arcane.common))



(defn get-subimage
  [kw full-image cell-width cell-height cell-x cell-y x-span y-span]
    (let [x (* cell-x cell-width)
          y (* cell-y cell-height)
          full-height (.getHeight full-image)
          full-width (.getWidth full-image)]
      (assert (and (< (+ x x-span) full-width) (< (+ y y-span) full-height))
              (str "Subimage " kw " at " cell-x "," cell-y "(" x "," y ")" 
                   " span (" x-span "," y-span ")"
                   " outside " full-width "," full-height))
    (.getSubimage full-image x y x-span y-span)))



(defn get-image-mapper
  "ts-map: the tileset definition file
   full-image: the full image file
  Returns a map with each keyword defined in the tileset definition mapped to a list of images.
  Each keyword will also have an entry under :keyword-x-offset for the x-offset and
  :keyword-y-offset for the y-offset.  Both of these are single integers and not lists, the
  assumption being that all images defined for a give keyword will have the same x-offset
  and y-offset"
  [ts-map full-image]
  (fn [image-map kw]
    (let [[image-file cell-width cell-height] (first (:file ts-map))
          subimgs (map (fn [attrs] 
                          (let [[x y & spans] attrs
                                [x-span y-span x-offset y-offset ] (if (seq spans)
                                                                      (if (= (count spans) 4)
                                                                            spans
                                                                          (concat spans '(0 0))) 
                                                                        (list cell-width cell-height 0 0))]
                                 {:image (get-subimage kw full-image cell-width cell-height x y  x-span y-span) :x-offset x-offset :y-offset y-offset}))
                           (kw ts-map))
          x-offset-kw (keyword-append kw :x-offset)
          y-offset-kw (keyword-append kw :y-offset)
          images (map #(% :image) subimgs)
          x-offset (first (map #(% :x-offset) subimgs))
          y-offset (first (map #(% :y-offset) subimgs))]

      (assoc image-map kw images x-offset-kw x-offset y-offset-kw y-offset))))




(defn get-attr-map
  [filename]
  (load-def filename resolve-multi-def-line))


(defn load-single-tileset
  [filename]
  (let [load-class (.getClass (Thread/currentThread))
        ts-map (get-attr-map filename)
        [ts-file ts-width ts-height] (first (:file ts-map))
        ts-filename (str "/" ts-file)
        no-file-map (dissoc ts-map :file)
        full-image (ImageIO/read (.getResource load-class ts-filename) )]
    (reduce (get-image-mapper ts-map full-image) {} (keys no-file-map))
    ))

(def ^:dynamic *tilesets* (atom nil))

(defn load-tilesets
  [ts-list]
  (reset! *tilesets*
    (reduce (fn [ts-map ts-item]
              (let [[ts-key ts-file] ts-item]
                (assoc ts-map ts-key (load-single-tileset ts-file))))
            {}
            ts-list)))

(defn get-tileset
  [kw]
  (@*tilesets* kw))


(defn get-tileset-image
  ([tileset-kw kw func]
    (let [tileset (get-tileset tileset-kw)]
      (assert tileset (str "No tileset for " tileset-kw "."))
      (let [imgs (tileset kw)]
        (assert imgs (str "No image for " kw "."))
        (func imgs))))
  ([tileset-kw kw]
   (get-tileset-image tileset-kw kw first)))




(defn load-test
  []
  
  (load-tilesets '([:world "/map.tileset"]))
  (get-tileset :world)
  )
