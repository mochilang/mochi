(ns main (:refer-clojure :exclude [is_harmonic_series harmonic_mean]))

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

(declare is_harmonic_series harmonic_mean)

(def ^:dynamic harmonic_mean_i nil)

(def ^:dynamic harmonic_mean_total nil)

(def ^:dynamic is_harmonic_series_common_diff nil)

(def ^:dynamic is_harmonic_series_i nil)

(def ^:dynamic is_harmonic_series_idx nil)

(def ^:dynamic is_harmonic_series_rec_series nil)

(def ^:dynamic is_harmonic_series_val nil)

(defn is_harmonic_series [is_harmonic_series_series]
  (binding [is_harmonic_series_common_diff nil is_harmonic_series_i nil is_harmonic_series_idx nil is_harmonic_series_rec_series nil is_harmonic_series_val nil] (try (do (when (= (count is_harmonic_series_series) 0) (throw (Exception. "Input list must be a non empty list"))) (when (= (count is_harmonic_series_series) 1) (do (when (= (nth is_harmonic_series_series 0) 0.0) (throw (Exception. "Input series cannot have 0 as an element"))) (throw (ex-info "return" {:v true})))) (set! is_harmonic_series_rec_series []) (set! is_harmonic_series_i 0) (while (< is_harmonic_series_i (count is_harmonic_series_series)) (do (set! is_harmonic_series_val (nth is_harmonic_series_series is_harmonic_series_i)) (when (= is_harmonic_series_val 0.0) (throw (Exception. "Input series cannot have 0 as an element"))) (set! is_harmonic_series_rec_series (conj is_harmonic_series_rec_series (/ 1.0 is_harmonic_series_val))) (set! is_harmonic_series_i (+ is_harmonic_series_i 1)))) (set! is_harmonic_series_common_diff (- (nth is_harmonic_series_rec_series 1) (nth is_harmonic_series_rec_series 0))) (set! is_harmonic_series_idx 2) (while (< is_harmonic_series_idx (count is_harmonic_series_rec_series)) (do (when (not= (- (nth is_harmonic_series_rec_series is_harmonic_series_idx) (nth is_harmonic_series_rec_series (- is_harmonic_series_idx 1))) is_harmonic_series_common_diff) (throw (ex-info "return" {:v false}))) (set! is_harmonic_series_idx (+ is_harmonic_series_idx 1)))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn harmonic_mean [harmonic_mean_series]
  (binding [harmonic_mean_i nil harmonic_mean_total nil] (try (do (when (= (count harmonic_mean_series) 0) (throw (Exception. "Input list must be a non empty list"))) (set! harmonic_mean_total 0.0) (set! harmonic_mean_i 0) (while (< harmonic_mean_i (count harmonic_mean_series)) (do (set! harmonic_mean_total (+ harmonic_mean_total (/ 1.0 (nth harmonic_mean_series harmonic_mean_i)))) (set! harmonic_mean_i (+ harmonic_mean_i 1)))) (throw (ex-info "return" {:v (quot (double (count harmonic_mean_series)) harmonic_mean_total)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (is_harmonic_series [1.0 (/ 2.0 3.0) (/ 1.0 2.0) (/ 2.0 5.0) (/ 1.0 3.0)]))
      (println (is_harmonic_series [1.0 (/ 2.0 3.0) (/ 2.0 5.0) (/ 1.0 3.0)]))
      (println (harmonic_mean [1.0 4.0 4.0]))
      (println (harmonic_mean [3.0 6.0 9.0 12.0]))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
