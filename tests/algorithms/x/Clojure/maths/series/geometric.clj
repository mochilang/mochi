(ns main (:refer-clojure :exclude [is_geometric_series geometric_mean pow_float nth_root test_geometric main]))

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

(declare is_geometric_series geometric_mean pow_float nth_root test_geometric main)

(def ^:dynamic geometric_mean_i nil)

(def ^:dynamic geometric_mean_n nil)

(def ^:dynamic geometric_mean_product nil)

(def ^:dynamic is_geometric_series_i nil)

(def ^:dynamic is_geometric_series_ratio nil)

(def ^:dynamic nth_root_high nil)

(def ^:dynamic nth_root_i nil)

(def ^:dynamic nth_root_low nil)

(def ^:dynamic nth_root_mid nil)

(def ^:dynamic nth_root_mp nil)

(def ^:dynamic pow_float_i nil)

(def ^:dynamic pow_float_result nil)

(def ^:dynamic test_geometric_a nil)

(def ^:dynamic test_geometric_b nil)

(defn is_geometric_series [is_geometric_series_series]
  (binding [is_geometric_series_i nil is_geometric_series_ratio nil] (try (do (when (= (count is_geometric_series_series) 0) (throw (Exception. "Input list must be a non empty list"))) (when (= (count is_geometric_series_series) 1) (throw (ex-info "return" {:v true}))) (when (= (nth is_geometric_series_series 0) 0.0) (throw (ex-info "return" {:v false}))) (set! is_geometric_series_ratio (quot (nth is_geometric_series_series 1) (nth is_geometric_series_series 0))) (set! is_geometric_series_i 0) (while (< is_geometric_series_i (- (count is_geometric_series_series) 1)) (do (when (= (nth is_geometric_series_series is_geometric_series_i) 0.0) (throw (ex-info "return" {:v false}))) (when (not= (quot (nth is_geometric_series_series (+ is_geometric_series_i 1)) (nth is_geometric_series_series is_geometric_series_i)) is_geometric_series_ratio) (throw (ex-info "return" {:v false}))) (set! is_geometric_series_i (+ is_geometric_series_i 1)))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn geometric_mean [geometric_mean_series]
  (binding [geometric_mean_i nil geometric_mean_n nil geometric_mean_product nil] (try (do (when (= (count geometric_mean_series) 0) (throw (Exception. "Input list must be a non empty list"))) (set! geometric_mean_product 1.0) (set! geometric_mean_i 0) (while (< geometric_mean_i (count geometric_mean_series)) (do (set! geometric_mean_product (* geometric_mean_product (nth geometric_mean_series geometric_mean_i))) (set! geometric_mean_i (+ geometric_mean_i 1)))) (set! geometric_mean_n (count geometric_mean_series)) (throw (ex-info "return" {:v (nth_root geometric_mean_product geometric_mean_n)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn pow_float [pow_float_base pow_float_exp]
  (binding [pow_float_i nil pow_float_result nil] (try (do (set! pow_float_result 1.0) (set! pow_float_i 0) (while (< pow_float_i pow_float_exp) (do (set! pow_float_result (* pow_float_result pow_float_base)) (set! pow_float_i (+ pow_float_i 1)))) (throw (ex-info "return" {:v pow_float_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn nth_root [nth_root_value nth_root_n]
  (binding [nth_root_high nil nth_root_i nil nth_root_low nil nth_root_mid nil nth_root_mp nil] (try (do (when (= nth_root_value 0.0) (throw (ex-info "return" {:v 0.0}))) (set! nth_root_low 0.0) (set! nth_root_high nth_root_value) (when (< nth_root_value 1.0) (set! nth_root_high 1.0)) (set! nth_root_mid (/ (+ nth_root_low nth_root_high) 2.0)) (set! nth_root_i 0) (while (< nth_root_i 40) (do (set! nth_root_mp (pow_float nth_root_mid nth_root_n)) (if (> nth_root_mp nth_root_value) (set! nth_root_high nth_root_mid) (set! nth_root_low nth_root_mid)) (set! nth_root_mid (/ (+ nth_root_low nth_root_high) 2.0)) (set! nth_root_i (+ nth_root_i 1)))) (throw (ex-info "return" {:v nth_root_mid}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn test_geometric []
  (binding [test_geometric_a nil test_geometric_b nil] (do (set! test_geometric_a [2.0 4.0 8.0]) (when (not (is_geometric_series test_geometric_a)) (throw (Exception. "expected geometric series"))) (set! test_geometric_b [1.0 2.0 3.0]) (when (is_geometric_series test_geometric_b) (throw (Exception. "expected non geometric series"))))))

(defn main []
  (do (test_geometric) (println (geometric_mean [2.0 4.0 8.0]))))

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
