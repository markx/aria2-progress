(ns aria2-progress.core
  (:require [clojure.java.io :as io])
  (:require [clojure.math.numeric-tower :as math])
  (:gen-class))

(def progress (atom ""))

(defn file-to-bytes [file]
  (with-open [in (io/input-stream file)]
    (let [buf (byte-array (.length (io/file file)))]
      (.read in buf)
      buf)))

(defn bytes-to-int [xs]
  (reduce (fn [acc x]
           (+ 
             (Byte/toUnsignedInt x) 
             (* 256 acc)))
          xs))

(defn byte-to-bin [x]
  (apply str (take-last 8 (str "0000000" (Integer/toBinaryString (Byte/toUnsignedInt x))))))

(defn parse [x]
  (let [
        [head more] (split-at 30 x)
        [bf-len more] (split-at 4 more)
        [bf more] (split-at (bytes-to-int bf-len) more)]
   {:head head
    :bf-len bf-len
    :bit-field (apply str (map byte-to-bin bf))}))


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

         
(defn -main [x]
  (if-not (.exists (io/file x))
    (println (str x " not found"))
    (do 
      (->> x 
          file-to-bytes
          parse
           :bit-field
          (swap! progress update-progress)
          (print-bf))
      (Thread/sleep 2000)
      (recur x))))



