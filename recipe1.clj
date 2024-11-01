(ns recipe1.core
  (:require [clojure.set :as cset]))

(defn expand
  [the-vector
   distance
   length]
  (let [end (count the-vector)
        start (- end
                 distance)
;;=> Here we go backwards 'distance' elements.
        pattern (subvec the-vector
                        start
                        end)]      ;=> We have our pattern.
    (into [] (take length    ;=> We exactly take "length" from
                   (cycle pattern)))))
;; an infinite repetition of our pattern.

(defn un-LZ77
  [bytes]
  (loop [result []
         remaining bytes]
;;=> We recur over the contents of the array.
    (if (seq remaining)
      (let [current (first remaining)
            the-rest (rest remaining)]
;=> Current element under scrutiny;
        (if-not (vector? current)
;=> If it is not a vector, add to result
          (recur (conj result
;;      the very element, and move on.
                       current)
                 the-rest)
          (recur (into result (expand result
;;=> This is a vector, then we'll expand here and move on
                                      (current 0)
                                      (current 1)))
                 the-rest)))
      result)))
;;=> end of recursion, return result.

(defn all-subvecs-from-beginning
;;=> this function will generate a set of all sub-vectors starting ;; from begin
  [v]
  (set (map #(subvec v 0 %)
;;=> we apply subvec from 0 to all indices from 1 up to the size ;; of the array + 1.
            (range 1 (inc (count v))))))

(defn all-subvecs
;;=> this function will generate all
  [v]          ;       sub-vectors, applying
  (loop [result #{}
;;       all-subvecs from beginning to
;;       all possible beginnings.
         remaining v]
    (if (seq remaining)
      (recur (into result
                   (all-subvecs-from-beginning remaining))
             (into []  (rest remaining)))
;;=> We recur fetching all sub-vectors for next beginning.

      result)))
;;=> end of recursion, I return result.

(defn longest-match-w-beginning
  [left-array right-array]
  (let [all-left-chunks (all-subvecs left-array)
        all-right-chunks-from-beginning
;;=> I take all sub-vectors from left-array
        (all-subvecs-from-beginning right-array)
;;=> I take all sub-vectors from right-array
        all-matches (cset/intersection all-right-chunks-from-beginning
                                       all-left-chunks)]
;;=> I get all the matchings using intersection on sets
    (->> all-matches
         (sort-by count >)
         first)))
;=> Then I return the longest match, sorting them
;; by decreasing order and taking the first element.

(defn pos-of-subvec
  [sv v]
  {:pre [(<= (count sv)
             (count v))]}
;;=> I verify that sv elements are less than v's.
  (loop
   [cursor 0]
    (if (or (empty? v)
;;=> If on of the vectors is empty
            (empty? sv)
            (= cursor   (count v)))
;; or the cursor ended-up exiting v,
      nil              ;; we return nil
      (if (= (subvec v cursor
;; => If we found that the v sub-vector
                     (+ (count sv)
;;   beginning with cursor up to sv count
                        cursor)) sv)
;; is equal to sv cursor
;; we return cursor, this is where the match is.
        (recur (inc cursor))))))
;=> We recur incrementing the cursor

(defn LZ77-STEP
  [window look-ahead]
  (let [longest (longest-match-w-beginning window
  look-ahead)]         ;;=> We find the Longest match,
    (if-let [pos-subv-w (pos-of-subvec longest window)]
;;=> If there is a match  we find its position in window.
      (let [distance (-  (count window) pos-subv-w)
;;=> the distance,
            pos-subv-l (pos-of-subvec longest
                                      look-ahead)
;;=> the position of the match in look-ahead
            the-char (first (subvec look-ahead
                                    (+ pos-subv-l
                                       (count longest))))]
;;=> the first element occuring after the match
        {:distance distance
         :length (count longest)
         :char the-char})
;;=> and we return information about match
      {:distance 0
       :length 0
       :char (first look-ahead)})))
;;=> We did not find a match, we emit zeros  for "distance"
;; and "length", and first element of lookahead as first char
;; occurring after the (non-)match.

(defn LZ77
[bytes-array
 window-size]
(->> (loop [result []
            cursor 0
            window []
            look-ahead bytes-array]
;;=> we begin with position 0; and everything as look-ahead.
       (if (empty? look-ahead)
         result
;;=> end of recursion, I emit result.
         (let [this-step-output (LZ77-STEP window look-ahead)
               distance (:distance this-step-output)
               length (:length this-step-output)
               literal (:char this-step-output)
;;=> We grab informations about this step output
               raw-new-cursor (+ cursor
                                 length
                                 1)
               new-cursor (min raw-new-cursor
                               (count bytes-array))
;;=> We compute the new-cursor, that is, where to go in the next ;; step
;;which is capped by count of bytes-array
               new-window (subvec bytes-array
                                  (max 0 (inc (- new-cursor
                                                 window-size)))
                                  new-cursor)
;;=> new window is window-size elements back from new cursor.
               new-look-ahead (subvec bytes-array
                                      new-cursor )]
;;=> new look-ahead is everything from new cursor on.
           (recur (conj result
                        [distance length]
                        literal)
                  new-cursor
                  new-window
                  new-look-ahead))))
;; and we recur with the new elements.
     (filter   (partial
                not=
                [0 0]))
;;=> We eliminate the entries related to non-matches
     (filter (comp
              not
              nil?))   ;;=> and any nils
     (into [])))       ;;=> and make a vector out of the output.
