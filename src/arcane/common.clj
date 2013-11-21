(ns arcane.common
  (:import (java.io BufferedReader FileReader InputStreamReader)
           javax.imageio.ImageIO))

(def world-height 14)
(def world-width 14)

(def tile-width 32)
(def tile-height 32)

(def display-width (* tile-width world-width))
(def display-height (* tile-height world-height))
(def party-row (+ 1 world-height))
(def status-height 2)
(def status-font-size 12)
(def status-font-color :yellow)
(def dialogue-font-size 12)
(def dialogue-font-color :white)
(def text-inset 2)
(def font-family "Serif")
(def timer-milliseconds 200)
(def dialogue-width 8)
(def dialogue-height 6)
(def dialogue-x-offset (quot (- world-width dialogue-width) 2))
(def dialogue-y-offset (quot (- world-height dialogue-height) 2))


(def dun-gen-max-iterations 20)
(def dun-gen-threshold-density  (quot (* world-width world-height 10) 19)  )
(def initial-gold 500)
(def max-party-size 12)


(def lair-count 
  "Keys are the levels of lairs to create, values are the number of lairs for each level"
  { 1   6
    2   4
    3   3
    4   2
    5   2 })  

(def initial-lair-count  (reduce + (vals lair-count ))) 

(def decoration-percent 60)

(defn strip-colon
  [s]
  (clojure.string/replace s ":" ""))

(defn keyword-append
  [kw-a kw-b]
  (keyword (str (strip-colon (str kw-a)) "-" (strip-colon (str kw-b)))))


(defn load-def
  [filename reduce-func]
  (let [load-class (.getClass (Thread/currentThread))]
    (with-open [rdr (BufferedReader. (InputStreamReader. (.getResourceAsStream load-class filename)))]
                 (reduce reduce-func {} (line-seq rdr)))))



(defn resolve-line
  [def-map line assoc-func]
  (if (clojure.string/blank? line)
    def-map
    (let [tokens (clojure.string/split line #"\s+")]
      (if (> (count tokens) 1)
        (let [kw (read-string (str ":" (first tokens)))
              tokes (map read-string (rest tokens)) ]
          (assoc-func def-map kw tokes))
        def-map))))



(defn resolve-single-def-line
  [def-map line]
  (resolve-line def-map line
                (fn [dmap kw tokens]
                  (assoc dmap kw tokens))))

(defn resolve-multi-def-line
  [def-map line]
  (resolve-line def-map line
          (fn [dmap kw tokens]
            (if (contains? dmap kw)
              (assoc dmap kw (conj (dmap kw) tokens))
              (assoc dmap kw (list tokens))))))


