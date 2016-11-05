(ns aria2-progress.core
  (:require [clojure.java.io :as io]))

(defn -main [x]
  (when (.exists (io/file x))
    (println (str x "exists!"))))



