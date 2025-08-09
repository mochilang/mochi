(ns main (:refer-clojure :exclude [abs pow_int nth_root round_nearest compute_geometric_mean test_compute_geometric_mean main]))

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

(declare abs pow_int nth_root round_nearest compute_geometric_mean test_compute_geometric_mean main)

(def ^:dynamic compute_geometric_mean_i nil)

(def ^:dynamic compute_geometric_mean_mean nil)

(def ^:dynamic compute_geometric_mean_possible nil)

(def ^:dynamic compute_geometric_mean_product nil)

(def ^:dynamic nth_root_denom nil)

(def ^:dynamic nth_root_guess nil)

(def ^:dynamic nth_root_i nil)

(def ^:dynamic pow_int_i nil)

(def ^:dynamic pow_int_result nil)

(def ^:dynamic round_nearest_n nil)

(def ^:dynamic test_compute_geometric_mean_eps nil)

(def ^:dynamic test_compute_geometric_mean_m1 nil)

(def ^:dynamic test_compute_geometric_mean_m2 nil)

(def ^:dynamic test_compute_geometric_mean_m3 nil)

(def ^:dynamic test_compute_geometric_mean_m4 nil)

(def ^:dynamic test_compute_geometric_mean_m5 nil)

(defn abs [abs_x]
  (try (if (< abs_x 0.0) (- abs_x) abs_x) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn pow_int [pow_int_base pow_int_exp]
  (binding [pow_int_i nil pow_int_result nil] (try (do (set! pow_int_result 1.0) (set! pow_int_i 0) (while (< pow_int_i pow_int_exp) (do (set! pow_int_result (* pow_int_result pow_int_base)) (set! pow_int_i (+ pow_int_i 1)))) (throw (ex-info "return" {:v pow_int_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn nth_root [nth_root_x nth_root_n]
  (binding [nth_root_denom nil nth_root_guess nil nth_root_i nil] (try (do (when (= nth_root_x 0.0) (throw (ex-info "return" {:v 0.0}))) (set! nth_root_guess nth_root_x) (set! nth_root_i 0) (while (< nth_root_i 10) (do (set! nth_root_denom (pow_int nth_root_guess (- nth_root_n 1))) (set! nth_root_guess (quot (+ (* (double (- nth_root_n 1)) nth_root_guess) (quot nth_root_x nth_root_denom)) (double nth_root_n))) (set! nth_root_i (+ nth_root_i 1)))) (throw (ex-info "return" {:v nth_root_guess}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn round_nearest [round_nearest_x]
  (binding [round_nearest_n nil] (try (do (when (>= round_nearest_x 0.0) (do (set! round_nearest_n (long (+ round_nearest_x 0.5))) (throw (ex-info "return" {:v (double round_nearest_n)})))) (set! round_nearest_n (long (- round_nearest_x 0.5))) (throw (ex-info "return" {:v (double round_nearest_n)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn compute_geometric_mean [compute_geometric_mean_nums]
  (binding [compute_geometric_mean_i nil compute_geometric_mean_mean nil compute_geometric_mean_possible nil compute_geometric_mean_product nil] (try (do (when (= (count compute_geometric_mean_nums) 0) (throw (Exception. "no numbers"))) (set! compute_geometric_mean_product 1.0) (set! compute_geometric_mean_i 0) (while (< compute_geometric_mean_i (count compute_geometric_mean_nums)) (do (set! compute_geometric_mean_product (* compute_geometric_mean_product (nth compute_geometric_mean_nums compute_geometric_mean_i))) (set! compute_geometric_mean_i (+ compute_geometric_mean_i 1)))) (when (and (< compute_geometric_mean_product 0.0) (= (mod (count compute_geometric_mean_nums) 2) 0)) (throw (Exception. "Cannot Compute Geometric Mean for these numbers."))) (set! compute_geometric_mean_mean (nth_root (abs compute_geometric_mean_product) (count compute_geometric_mean_nums))) (when (< compute_geometric_mean_product 0.0) (set! compute_geometric_mean_mean (- compute_geometric_mean_mean))) (set! compute_geometric_mean_possible (round_nearest compute_geometric_mean_mean)) (when (= (pow_int compute_geometric_mean_possible (count compute_geometric_mean_nums)) compute_geometric_mean_product) (set! compute_geometric_mean_mean compute_geometric_mean_possible)) (throw (ex-info "return" {:v compute_geometric_mean_mean}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn test_compute_geometric_mean []
  (binding [test_compute_geometric_mean_eps nil test_compute_geometric_mean_m1 nil test_compute_geometric_mean_m2 nil test_compute_geometric_mean_m3 nil test_compute_geometric_mean_m4 nil test_compute_geometric_mean_m5 nil] (do (set! test_compute_geometric_mean_eps 0.0001) (set! test_compute_geometric_mean_m1 (compute_geometric_mean [2.0 8.0])) (when (> (abs (- test_compute_geometric_mean_m1 4.0)) test_compute_geometric_mean_eps) (throw (Exception. "test1 failed"))) (set! test_compute_geometric_mean_m2 (compute_geometric_mean [5.0 125.0])) (when (> (abs (- test_compute_geometric_mean_m2 25.0)) test_compute_geometric_mean_eps) (throw (Exception. "test2 failed"))) (set! test_compute_geometric_mean_m3 (compute_geometric_mean [1.0 0.0])) (when (> (abs (- test_compute_geometric_mean_m3 0.0)) test_compute_geometric_mean_eps) (throw (Exception. "test3 failed"))) (set! test_compute_geometric_mean_m4 (compute_geometric_mean [1.0 5.0 25.0 5.0])) (when (> (abs (- test_compute_geometric_mean_m4 5.0)) test_compute_geometric_mean_eps) (throw (Exception. "test4 failed"))) (set! test_compute_geometric_mean_m5 (compute_geometric_mean [(- 5.0) 25.0 1.0])) (when (> (abs (+ test_compute_geometric_mean_m5 5.0)) test_compute_geometric_mean_eps) (throw (Exception. "test5 failed"))))))

(defn main []
  (do (test_compute_geometric_mean) (println (compute_geometric_mean [(- 3.0) (- 27.0)]))))

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
