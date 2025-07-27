(ns main (:refer-clojure :exclude [nextRand shuffleChars bestShuffle main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare nextRand shuffleChars bestShuffle main)

(defn nextRand [seed]
  (try (throw (ex-info "return" {:v (mod (+ (* seed 1664525) 1013904223) 2147483647)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn shuffleChars [s seed]
  (try (do (def chars []) (def i 0) (while (< i (count s)) (do (def chars (conj chars (subs s i (+ i 1)))) (def i (+ i 1)))) (def sd seed) (def idx (- (count chars) 1)) (while (> idx 0) (do (def sd (nextRand sd)) (def j (mod sd (+ idx 1))) (def tmp (nth chars idx)) (def chars (assoc chars idx (nth chars j))) (def chars (assoc chars j tmp)) (def idx (- idx 1)))) (def res "") (def i 0) (while (< i (count chars)) (do (def res (str res (nth chars i))) (def i (+ i 1)))) (throw (ex-info "return" {:v [res sd]}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn bestShuffle [s seed]
  (try (do (def r (shuffleChars s seed)) (def t (nth r 0)) (def sd (nth r 1)) (def arr []) (def i 0) (while (< i (count t)) (do (def arr (conj arr (subs t i (+ i 1)))) (def i (+ i 1)))) (def i 0) (while (< i (count arr)) (do (def j 0) (loop [while_flag_1 true] (when (and while_flag_1 (< j (count arr))) (cond (and (and (not= i j) (not= (nth arr i) (subs s j (+ j 1)))) (not= (nth arr j) (subs s i (+ i 1)))) (do (def tmp (nth arr i)) (def arr (assoc arr i (nth arr j))) (def arr (assoc arr j tmp)) (recur false)) :else (do (def j (+ j 1)) (recur while_flag_1))))) (def i (+ i 1)))) (def count_v 0) (def i 0) (while (< i (count arr)) (do (when (= (nth arr i) (subs s i (+ i 1))) (def count_v (+ count_v 1))) (def i (+ i 1)))) (def out "") (def i 0) (while (< i (count arr)) (do (def out (str out (nth arr i))) (def i (+ i 1)))) (throw (ex-info "return" {:v [out sd count_v]}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (do (def ts ["abracadabra" "seesaw" "elk" "grrrrrr" "up" "a"]) (def seed 1) (def i 0) (while (< i (count ts)) (do (def r (bestShuffle (nth ts i) seed)) (def shuf (nth r 0)) (def seed (nth r 1)) (def cnt (nth r 2)) (println (str (str (str (str (str (nth ts i) " -> ") shuf) " (") (str cnt)) ")")) (def i (+ i 1))))))

(defn -main []
  (main))

(-main)
