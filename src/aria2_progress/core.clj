(ns aria2-progress.core
  (:require 
    [aria2-progress.parser :refer [parse]]
    [clojure.java.io :as io]
    [clojure.math.numeric-tower :as math]
    [seesaw.core :refer :all] 
    [seesaw.graphics :refer :all]
    [seesaw.color :refer :all])
  (:gen-class))

(def progress (atom ""))
(def block-per-row (atom 20))

(def block-style
  {:complete (style :background (color 66 214 146) :foreground (color 242 242 242) :stroke 1)
   :partial (style :background (color 49 242 24) :foreground (color 242 242 242) :stroke 1)
   :empty (style :background (color 178 178 178) :foreground (color 242 242 242) :stroke 1)})

(def block-size 10)
(def padding-horizontal 11)
(def padding-vertical 41)


(defn update-progress [old-x x]
  (if (= (count (filter #{\0} old-x)) (count (filter #{\0} x)))
    old-x
    (if (empty? old-x)
      x
      (map 
        (fn [a b]
          (cond 
            (= a \x ) \1
            (not= a b) \x 
            :else b))
        old-x x))))

(defn print-bf [bf] 
  (let [n (int (math/ceil (math/sqrt (count bf))))
        field (partition n bf)]
    (println (apply str (repeat n "-")))
    (dorun (map #(println (apply str %)) field))))

(defn make-rect [index bit]
  (let [x (* block-size (mod index @block-per-row))
        y (* block-size (int (/ index @block-per-row)))]
    [(rect x y block-size block-size) 
     (condp = bit
       \0 (block-style :empty)
       \1 (block-style :complete)
       \x (block-style :partial))]))


(defn render [context graphics]
  (doseq [[r s] (map-indexed make-rect @progress)]
    (draw graphics r s)))


(defn make-window []
  (let [len (count @progress)
        new-block-per-row (int (math/ceil (math/sqrt len)))
        row-number (int (/ len new-block-per-row))
        w (+ padding-horizontal (* new-block-per-row block-size)) 
        h (+ padding-vertical (* block-size row-number))]
    (reset! block-per-row new-block-per-row)
    (native!)
    (-> (frame
          :id :root
          :title "aria2 progress"
          :minimum-size [w :by h]
          :on-close :dispose
          :content 
          (border-panel
            :hgap 5 :vgap 5 :border 5
            :center (canvas 
                      :id :canvas 
                      :paint render)))
        show!)))

(defn parse-loop [x w]
  (if-not (.exists (io/file x))
    (println (str x "doesn't exist anymore"))
    (do 
      (->> x 
           parse
           :bit-field
           (swap! progress update-progress)
           (print-bf))
      (if (nil? w)
        (recur x (make-window)) 
        (do
          (repaint! w) 
          (Thread/sleep 2000)
          (recur x w))))))

(defn -main [x]
  (if-not (.exists (io/file x))
    (println (str x " not found"))
    (parse-loop x nil)))

