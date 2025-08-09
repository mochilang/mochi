(ns main (:refer-clojure :exclude [is_arithmetic_series arithmetic_mean]))

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

(declare is_arithmetic_series arithmetic_mean)

(def ^:dynamic arithmetic_mean_i nil)

(def ^:dynamic arithmetic_mean_total nil)

(def ^:dynamic is_arithmetic_series_diff nil)

(def ^:dynamic is_arithmetic_series_i nil)

(defn is_arithmetic_series [is_arithmetic_series_xs]
  (binding [is_arithmetic_series_diff nil is_arithmetic_series_i nil] (try (do (when (= (count is_arithmetic_series_xs) 0) (throw (Exception. "Input list must be a non empty list"))) (when (= (count is_arithmetic_series_xs) 1) (throw (ex-info "return" {:v true}))) (set! is_arithmetic_series_diff (- (nth is_arithmetic_series_xs 1) (nth is_arithmetic_series_xs 0))) (set! is_arithmetic_series_i 0) (while (< is_arithmetic_series_i (- (count is_arithmetic_series_xs) 1)) (do (when (not= (- (nth is_arithmetic_series_xs (+ is_arithmetic_series_i 1)) (nth is_arithmetic_series_xs is_arithmetic_series_i)) is_arithmetic_series_diff) (throw (ex-info "return" {:v false}))) (set! is_arithmetic_series_i (+ is_arithmetic_series_i 1)))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn arithmetic_mean [arithmetic_mean_xs]
  (binding [arithmetic_mean_i nil arithmetic_mean_total nil] (try (do (when (= (count arithmetic_mean_xs) 0) (throw (Exception. "Input list must be a non empty list"))) (set! arithmetic_mean_total 0.0) (set! arithmetic_mean_i 0) (while (< arithmetic_mean_i (count arithmetic_mean_xs)) (do (set! arithmetic_mean_total (+ arithmetic_mean_total (nth arithmetic_mean_xs arithmetic_mean_i))) (set! arithmetic_mean_i (+ arithmetic_mean_i 1)))) (throw (ex-info "return" {:v (quot arithmetic_mean_total (double (count arithmetic_mean_xs)))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (is_arithmetic_series [2.0 4.0 6.0])))
      (println (str (is_arithmetic_series [3.0 6.0 12.0 24.0])))
      (println (str (arithmetic_mean [2.0 4.0 6.0])))
      (println (str (arithmetic_mean [3.0 6.0 9.0 12.0])))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
