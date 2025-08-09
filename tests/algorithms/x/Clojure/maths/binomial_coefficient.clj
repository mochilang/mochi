(ns main (:refer-clojure :exclude [binomial_coefficient]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(defn indexOf [s sub]
  (let [idx (clojure.string/index-of s sub)] (if (nil? idx) -1 idx)))

(defn split [s sep]
  (clojure.string/split s (re-pattern sep)))

(defn toi [s]
  (Integer/parseInt (str s)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare binomial_coefficient)

(def ^:dynamic binomial_coefficient_c nil)

(def ^:dynamic binomial_coefficient_i nil)

(def ^:dynamic binomial_coefficient_j nil)

(defn binomial_coefficient [binomial_coefficient_n binomial_coefficient_r]
  (binding [binomial_coefficient_c nil binomial_coefficient_i nil binomial_coefficient_j nil] (try (do (when (or (< binomial_coefficient_n 0) (< binomial_coefficient_r 0)) (throw (Exception. "n and r must be non-negative integers"))) (when (or (= binomial_coefficient_n 0) (= binomial_coefficient_r 0)) (throw (ex-info "return" {:v 1}))) (set! binomial_coefficient_c []) (dotimes [_ (+ binomial_coefficient_r 1)] (set! binomial_coefficient_c (conj binomial_coefficient_c 0))) (set! binomial_coefficient_c (assoc binomial_coefficient_c 0 1)) (set! binomial_coefficient_i 1) (while (<= binomial_coefficient_i binomial_coefficient_n) (do (set! binomial_coefficient_j (if (< binomial_coefficient_i binomial_coefficient_r) binomial_coefficient_i binomial_coefficient_r)) (while (> binomial_coefficient_j 0) (do (set! binomial_coefficient_c (assoc binomial_coefficient_c binomial_coefficient_j (+ (nth binomial_coefficient_c binomial_coefficient_j) (nth binomial_coefficient_c (- binomial_coefficient_j 1))))) (set! binomial_coefficient_j (- binomial_coefficient_j 1)))) (set! binomial_coefficient_i (+ binomial_coefficient_i 1)))) (throw (ex-info "return" {:v (nth binomial_coefficient_c binomial_coefficient_r)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (binomial_coefficient 10 5)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
