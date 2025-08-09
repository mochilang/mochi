(ns main (:refer-clojure :exclude [abs_val pow_float nth_root minkowski_distance test_minkowski main]))

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

(declare abs_val pow_float nth_root minkowski_distance test_minkowski main)

(def ^:dynamic minkowski_distance_diff nil)

(def ^:dynamic minkowski_distance_idx nil)

(def ^:dynamic minkowski_distance_total nil)

(def ^:dynamic nth_root_i nil)

(def ^:dynamic nth_root_num nil)

(def ^:dynamic nth_root_x nil)

(def ^:dynamic pow_float_i nil)

(def ^:dynamic pow_float_result nil)

(defn abs_val [abs_val_x]
  (try (if (< abs_val_x 0.0) (- abs_val_x) abs_val_x) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn pow_float [pow_float_base pow_float_exp]
  (binding [pow_float_i nil pow_float_result nil] (try (do (set! pow_float_result 1.0) (set! pow_float_i 0) (while (< pow_float_i pow_float_exp) (do (set! pow_float_result (* pow_float_result pow_float_base)) (set! pow_float_i (+ pow_float_i 1)))) (throw (ex-info "return" {:v pow_float_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn nth_root [nth_root_value nth_root_n]
  (binding [nth_root_i nil nth_root_num nil nth_root_x nil] (try (do (when (= nth_root_value 0.0) (throw (ex-info "return" {:v 0.0}))) (set! nth_root_x (quot nth_root_value (double nth_root_n))) (set! nth_root_i 0) (while (< nth_root_i 20) (do (set! nth_root_num (+ (* (double (- nth_root_n 1)) nth_root_x) (/ nth_root_value (pow_float nth_root_x (- nth_root_n 1))))) (set! nth_root_x (quot nth_root_num (double nth_root_n))) (set! nth_root_i (+ nth_root_i 1)))) (throw (ex-info "return" {:v nth_root_x}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn minkowski_distance [minkowski_distance_point_a minkowski_distance_point_b minkowski_distance_order]
  (binding [minkowski_distance_diff nil minkowski_distance_idx nil minkowski_distance_total nil] (try (do (when (< minkowski_distance_order 1) (throw (Exception. "The order must be greater than or equal to 1."))) (when (not= (count minkowski_distance_point_a) (count minkowski_distance_point_b)) (throw (Exception. "Both points must have the same dimension."))) (set! minkowski_distance_total 0.0) (set! minkowski_distance_idx 0) (while (< minkowski_distance_idx (count minkowski_distance_point_a)) (do (set! minkowski_distance_diff (abs_val (- (nth minkowski_distance_point_a minkowski_distance_idx) (nth minkowski_distance_point_b minkowski_distance_idx)))) (set! minkowski_distance_total (+ minkowski_distance_total (pow_float minkowski_distance_diff minkowski_distance_order))) (set! minkowski_distance_idx (+ minkowski_distance_idx 1)))) (throw (ex-info "return" {:v (nth_root minkowski_distance_total minkowski_distance_order)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn test_minkowski []
  (do (when (> (abs_val (- (minkowski_distance [1.0 1.0] [2.0 2.0] 1) 2.0)) 0.0001) (throw (Exception. "minkowski_distance test1 failed"))) (when (> (abs_val (- (minkowski_distance [1.0 2.0 3.0 4.0] [5.0 6.0 7.0 8.0] 2) 8.0)) 0.0001) (throw (Exception. "minkowski_distance test2 failed")))))

(defn main []
  (do (test_minkowski) (println (minkowski_distance [5.0] [0.0] 3))))

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
