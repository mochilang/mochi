(ns main (:refer-clojure :exclude [floor pow10 round sqrtApprox mean stdev normalization standardization]))

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

(declare floor pow10 round sqrtApprox mean stdev normalization standardization)

(def ^:dynamic floor_i nil)

(def ^:dynamic mean_i nil)

(def ^:dynamic mean_n nil)

(def ^:dynamic mean_total nil)

(def ^:dynamic normalization_denom nil)

(def ^:dynamic normalization_i nil)

(def ^:dynamic normalization_n nil)

(def ^:dynamic normalization_norm nil)

(def ^:dynamic normalization_result nil)

(def ^:dynamic normalization_x_max nil)

(def ^:dynamic normalization_x_min nil)

(def ^:dynamic pow10_i nil)

(def ^:dynamic pow10_result nil)

(def ^:dynamic round_m nil)

(def ^:dynamic round_y nil)

(def ^:dynamic sqrtApprox_guess nil)

(def ^:dynamic sqrtApprox_i nil)

(def ^:dynamic standardization_i nil)

(def ^:dynamic standardization_mu nil)

(def ^:dynamic standardization_n nil)

(def ^:dynamic standardization_result nil)

(def ^:dynamic standardization_sigma nil)

(def ^:dynamic standardization_z nil)

(def ^:dynamic stdev_diff nil)

(def ^:dynamic stdev_i nil)

(def ^:dynamic stdev_m nil)

(def ^:dynamic stdev_n nil)

(def ^:dynamic stdev_sum_sq nil)

(defn floor [floor_x]
  (binding [floor_i nil] (try (do (set! floor_i (long floor_x)) (when (> (double floor_i) floor_x) (set! floor_i (- floor_i 1))) (throw (ex-info "return" {:v (double floor_i)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn pow10 [pow10_n]
  (binding [pow10_i nil pow10_result nil] (try (do (set! pow10_result 1.0) (set! pow10_i 0) (while (< pow10_i pow10_n) (do (set! pow10_result (* pow10_result 10.0)) (set! pow10_i (+ pow10_i 1)))) (throw (ex-info "return" {:v pow10_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn round [round_x round_n]
  (binding [round_m nil round_y nil] (try (do (set! round_m (pow10 round_n)) (set! round_y (double (floor (+ (* round_x round_m) 0.5)))) (throw (ex-info "return" {:v (quot round_y round_m)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn sqrtApprox [sqrtApprox_x]
  (binding [sqrtApprox_guess nil sqrtApprox_i nil] (try (do (set! sqrtApprox_guess sqrtApprox_x) (set! sqrtApprox_i 0) (while (< sqrtApprox_i 20) (do (set! sqrtApprox_guess (/ (+ sqrtApprox_guess (quot sqrtApprox_x sqrtApprox_guess)) 2.0)) (set! sqrtApprox_i (+ sqrtApprox_i 1)))) (throw (ex-info "return" {:v sqrtApprox_guess}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn mean [mean_data]
  (binding [mean_i nil mean_n nil mean_total nil] (try (do (set! mean_total 0.0) (set! mean_i 0) (set! mean_n (count mean_data)) (while (< mean_i mean_n) (do (set! mean_total (+ mean_total (nth mean_data mean_i))) (set! mean_i (+ mean_i 1)))) (throw (ex-info "return" {:v (quot mean_total (double mean_n))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn stdev [stdev_data]
  (binding [stdev_diff nil stdev_i nil stdev_m nil stdev_n nil stdev_sum_sq nil] (try (do (set! stdev_n (count stdev_data)) (when (<= stdev_n 1) (throw (Exception. "data length must be > 1"))) (set! stdev_m (mean stdev_data)) (set! stdev_sum_sq 0.0) (set! stdev_i 0) (while (< stdev_i stdev_n) (do (set! stdev_diff (- (nth stdev_data stdev_i) stdev_m)) (set! stdev_sum_sq (+ stdev_sum_sq (* stdev_diff stdev_diff))) (set! stdev_i (+ stdev_i 1)))) (throw (ex-info "return" {:v (sqrtApprox (quot stdev_sum_sq (double (- stdev_n 1))))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn normalization [normalization_data normalization_ndigits]
  (binding [normalization_denom nil normalization_i nil normalization_n nil normalization_norm nil normalization_result nil normalization_x_max nil normalization_x_min nil] (try (do (set! normalization_x_min (double (apply min normalization_data))) (set! normalization_x_max (double (apply max normalization_data))) (set! normalization_denom (- normalization_x_max normalization_x_min)) (set! normalization_result []) (set! normalization_i 0) (set! normalization_n (count normalization_data)) (while (< normalization_i normalization_n) (do (set! normalization_norm (quot (- (nth normalization_data normalization_i) normalization_x_min) normalization_denom)) (set! normalization_result (conj normalization_result (round normalization_norm normalization_ndigits))) (set! normalization_i (+ normalization_i 1)))) (throw (ex-info "return" {:v normalization_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn standardization [standardization_data standardization_ndigits]
  (binding [standardization_i nil standardization_mu nil standardization_n nil standardization_result nil standardization_sigma nil standardization_z nil] (try (do (set! standardization_mu (mean standardization_data)) (set! standardization_sigma (stdev standardization_data)) (set! standardization_result []) (set! standardization_i 0) (set! standardization_n (count standardization_data)) (while (< standardization_i standardization_n) (do (set! standardization_z (quot (- (nth standardization_data standardization_i) standardization_mu) standardization_sigma)) (set! standardization_result (conj standardization_result (round standardization_z standardization_ndigits))) (set! standardization_i (+ standardization_i 1)))) (throw (ex-info "return" {:v standardization_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (normalization [2.0 7.0 10.0 20.0 30.0 50.0] 3)))
      (println (str (normalization [5.0 10.0 15.0 20.0 25.0] 3)))
      (println (str (standardization [2.0 7.0 10.0 20.0 30.0 50.0] 3)))
      (println (str (standardization [5.0 10.0 15.0 20.0 25.0] 3)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
