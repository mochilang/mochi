(ns main (:refer-clojure :exclude [indexOf contains maxOf intSqrt sumProperDivisors classifySequence padLeft padRight joinWithCommas main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare indexOf contains maxOf intSqrt sumProperDivisors classifySequence padLeft padRight joinWithCommas main)

(def THRESHOLD 140737488355328)

(defn indexOf [xs value]
  (try (do (def i 0) (while (< i (count xs)) (do (when (= (nth xs i) value) (throw (ex-info "return" {:v i}))) (def i (+ i 1)))) (throw (ex-info "return" {:v (- 0 1)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn contains [xs value]
  (try (throw (ex-info "return" {:v (not= (indexOf xs value) (- 0 1))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn maxOf [a b]
  (try (if (> a b) (throw (ex-info "return" {:v a})) (throw (ex-info "return" {:v b}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn intSqrt [n]
  (try (do (when (= n 0) (throw (ex-info "return" {:v 0}))) (def x n) (def y (/ (+ x 1) 2)) (while (< y x) (do (def x y) (def y (/ (+ x (/ n x)) 2)))) (throw (ex-info "return" {:v x}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn sumProperDivisors [n]
  (try (do (when (< n 2) (throw (ex-info "return" {:v 0}))) (def sqrt (intSqrt n)) (def sum 1) (def i 2) (while (<= i sqrt) (do (when (= (mod n i) 0) (def sum (+ (+ sum i) (/ n i)))) (def i (+ i 1)))) (when (= (* sqrt sqrt) n) (def sum (- sum sqrt))) (throw (ex-info "return" {:v sum}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn classifySequence [k]
  (try (do (def last k) (def seq [k]) (while true (do (def last (sumProperDivisors last)) (def seq (conj seq last)) (def n (count seq)) (def aliquot "") (if (= last 0) (def aliquot "Terminating") (if (and (= n 2) (= last k)) (def aliquot "Perfect") (if (and (= n 3) (= last k)) (def aliquot "Amicable") (if (and (>= n 4) (= last k)) (def aliquot (str (str "Sociable[" (str (- n 1))) "]")) (if (= last (nth seq (- n 2))) (def aliquot "Aspiring") (if (contains (subvec seq 1 (maxOf 1 (- n 2))) last) (do (def idx (indexOf seq last)) (def aliquot (str (str "Cyclic[" (str (- (- n 1) idx))) "]"))) (when (or (= n 16) (> last THRESHOLD)) (def aliquot "Non-Terminating")))))))) (when (not= aliquot "") (throw (ex-info "return" {:v {"seq" seq "aliquot" aliquot}}))))) (throw (ex-info "return" {:v {"seq" seq "aliquot" ""}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn padLeft [n w]
  (try (do (def s (str n)) (while (< (count s) w) (def s (str " " s))) (throw (ex-info "return" {:v s}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn padRight [s w]
  (try (do (def r s) (while (< (count r) w) (def r (str r " "))) (throw (ex-info "return" {:v r}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn joinWithCommas [seq]
  (try (do (def s "[") (def i 0) (while (< i (count seq)) (do (def s (str s (str (nth seq i)))) (when (< i (- (count seq) 1)) (def s (str s ", "))) (def i (+ i 1)))) (def s (str s "]")) (throw (ex-info "return" {:v s}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (do (println "Aliquot classifications - periods for Sociable/Cyclic in square brackets:\n") (def k 1) (while (<= k 10) (do (def res (classifySequence k)) (println (str (str (str (str (padLeft k 2) ": ") (padRight (str (get res "aliquot")) 15)) " ") (joinWithCommas (get res "seq")))) (def k (+ k 1)))) (println "") (def s [11 12 28 496 220 1184 12496 1264460 790 909 562 1064 1488]) (def i 0) (while (< i (count s)) (do (def val (nth s i)) (def res (classifySequence val)) (println (str (str (str (str (padLeft val 7) ": ") (padRight (str (get res "aliquot")) 15)) " ") (joinWithCommas (get res "seq")))) (def i (+ i 1)))) (println "") (def big 15355717786080) (def r (classifySequence big)) (println (str (str (str (str (str big) ": ") (padRight (str (get r "aliquot")) 15)) " ") (joinWithCommas (get r "seq"))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (main)
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
