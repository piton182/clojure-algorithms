(ns recipe1.core
  (:require [clojure.set :as cset]))

(defn expand
  [the-vector
   distance
   length]
  (let [end (count the-vector)
        start (- end
                 distance)
        pattern (subvec the-vector
                        start
                        end)]
    (into [] (take length
                   (cycle pattern)))))

(defn un-LZ77
  [bytes]
  (loop [result []
         remaining bytes]
    (if (seq remaining)
      (let [current (first remaining)
            the-rest (rest remaining)]
        (if-not (vector? current)
          (recur (conj result
                       current)
                 the-rest)
          (recur (into result (expand result
                                      (current 0)
                                      (current 1)))
                 the-rest)))
      result)))
