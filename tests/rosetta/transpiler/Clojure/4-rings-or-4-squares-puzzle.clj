(ns main (:refer-clojure :exclude [validComb isUnique getCombs]))

(require 'clojure.set)

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare validComb isUnique getCombs)

(defn validComb [a b c d e f g]
  (try (do (def square1 (+ a b)) (def square2 (+ (+ b c) d)) (def square3 (+ (+ d e) f)) (def square4 (+ f g)) (throw (ex-info "return" {:v (and (and (= square1 square2) (= square2 square3)) (= square3 square4))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn isUnique [a b c d e f g]
  (try (do (def nums [a b c d e f g]) (def i 0) (while (< i (count nums)) (do (def j (+ i 1)) (while (< j (count nums)) (do (when (= (nth nums i) (nth nums j)) (throw (ex-info "return" {:v false}))) (def j (+ j 1)))) (def i (+ i 1)))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn getCombs [low high unique]
  (try (do (def valid []) (def count 0) (doseq [b (range low (+ high 1))] (doseq [c (range low (+ high 1))] (doseq [d (range low (+ high 1))] (do (def s (+ (+ b c) d)) (doseq [e (range low (+ high 1))] (loop [f_seq (range low (+ high 1))] (when (seq f_seq) (let [f (first f_seq)] (cond (or (< a low) (> a high)) (recur (rest f_seq)) (or (< g low) (> g high)) (recur (rest f_seq)) (not= (+ (+ d e) f) s) (recur (rest f_seq)) (not= (+ f g) s) (recur (rest f_seq)) :else (do (def a (- s b)) (def g (- s f)) (when (or (not unique) (isUnique a b c d e f g)) (do (def valid (conj valid [a b c d e f g])) (def count (+ count 1)))) (recur (rest f_seq)))))))))))) (throw (ex-info "return" {:v {"count" count "list" valid}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(def r1 (getCombs 1 7 true))

(def r2 (getCombs 3 9 true))

(def r3 (getCombs 0 9 false))

(defn -main []
  (println (str (str (get r1 "count")) " unique solutions in 1 to 7"))
  (println (get r1 "list"))
  (println (str (str (get r2 "count")) " unique solutions in 3 to 9"))
  (println (get r2 "list"))
  (println (str (str (get r3 "count")) " non-unique solutions in 0 to 9")))

(-main)
