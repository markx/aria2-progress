(ns aria2-progress.parser
  (:require [clojure.java.io :as io]))

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

(defn parse [f]
  (let [x (file-to-bytes f)
        [head more] (split-at 30 x)
        [bf-len more] (split-at 4 more)
        [bf more] (split-at (bytes-to-int bf-len) more)
        [in-flight-num more] (split-at 4 more)]
   {:head head
    :bf-len bf-len
    :bit-field (apply str (map byte-to-bin bf))}))

