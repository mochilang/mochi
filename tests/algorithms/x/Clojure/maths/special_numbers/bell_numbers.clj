(ns main (:refer-clojure :exclude [binomial_coefficient bell_numbers main]))

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

(declare binomial_coefficient bell_numbers main)

(def ^:dynamic bell_numbers_bell nil)

(def ^:dynamic bell_numbers_i nil)

(def ^:dynamic bell_numbers_j nil)

(def ^:dynamic binomial_coefficient_coefficient nil)

(def ^:dynamic binomial_coefficient_i nil)

(def ^:dynamic binomial_coefficient_k nil)

(defn binomial_coefficient [binomial_coefficient_total_elements binomial_coefficient_elements_to_choose]
  (binding [binomial_coefficient_coefficient nil binomial_coefficient_i nil binomial_coefficient_k nil] (try (do (when (or (= binomial_coefficient_elements_to_choose 0) (= binomial_coefficient_elements_to_choose binomial_coefficient_total_elements)) (throw (ex-info "return" {:v 1}))) (set! binomial_coefficient_k binomial_coefficient_elements_to_choose) (when (> binomial_coefficient_k (- binomial_coefficient_total_elements binomial_coefficient_k)) (set! binomial_coefficient_k (- binomial_coefficient_total_elements binomial_coefficient_k))) (set! binomial_coefficient_coefficient 1) (set! binomial_coefficient_i 0) (while (< binomial_coefficient_i binomial_coefficient_k) (do (set! binomial_coefficient_coefficient (* binomial_coefficient_coefficient (- binomial_coefficient_total_elements binomial_coefficient_i))) (set! binomial_coefficient_coefficient (quot binomial_coefficient_coefficient (+ binomial_coefficient_i 1))) (set! binomial_coefficient_i (+ binomial_coefficient_i 1)))) (throw (ex-info "return" {:v binomial_coefficient_coefficient}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn bell_numbers [bell_numbers_max_set_length]
  (binding [bell_numbers_bell nil bell_numbers_i nil bell_numbers_j nil] (try (do (when (< bell_numbers_max_set_length 0) (throw (Exception. "max_set_length must be non-negative"))) (set! bell_numbers_bell []) (set! bell_numbers_i 0) (while (<= bell_numbers_i bell_numbers_max_set_length) (do (set! bell_numbers_bell (conj bell_numbers_bell 0)) (set! bell_numbers_i (+ bell_numbers_i 1)))) (set! bell_numbers_bell (assoc bell_numbers_bell 0 1)) (set! bell_numbers_i 1) (while (<= bell_numbers_i bell_numbers_max_set_length) (do (set! bell_numbers_j 0) (while (< bell_numbers_j bell_numbers_i) (do (set! bell_numbers_bell (assoc bell_numbers_bell bell_numbers_i (+ (nth bell_numbers_bell bell_numbers_i) (* (binomial_coefficient (- bell_numbers_i 1) bell_numbers_j) (nth bell_numbers_bell bell_numbers_j))))) (set! bell_numbers_j (+ bell_numbers_j 1)))) (set! bell_numbers_i (+ bell_numbers_i 1)))) (throw (ex-info "return" {:v bell_numbers_bell}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (println (str (bell_numbers 5))))

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
