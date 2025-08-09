(ns main (:refer-clojure :exclude [fx fx_derivative get_initial_point abs_float square_root_iterative]))

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

(declare fx fx_derivative get_initial_point abs_float square_root_iterative)

(def ^:dynamic get_initial_point_start nil)

(def ^:dynamic square_root_iterative_i nil)

(def ^:dynamic square_root_iterative_prev_value nil)

(def ^:dynamic square_root_iterative_value nil)

(defn fx [fx_x fx_a]
  (try (throw (ex-info "return" {:v (- (* fx_x fx_x) fx_a)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn fx_derivative [fx_derivative_x]
  (try (throw (ex-info "return" {:v (* 2.0 fx_derivative_x)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn get_initial_point [get_initial_point_a]
  (binding [get_initial_point_start nil] (try (do (set! get_initial_point_start 2.0) (while (<= get_initial_point_start get_initial_point_a) (set! get_initial_point_start (* get_initial_point_start get_initial_point_start))) (throw (ex-info "return" {:v get_initial_point_start}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn abs_float [abs_float_x]
  (try (if (< abs_float_x 0.0) (- abs_float_x) abs_float_x) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn square_root_iterative [square_root_iterative_a square_root_iterative_max_iter square_root_iterative_tolerance]
  (binding [square_root_iterative_i nil square_root_iterative_prev_value nil square_root_iterative_value nil] (try (do (when (< square_root_iterative_a 0.0) (throw (Exception. "math domain error"))) (set! square_root_iterative_value (get_initial_point square_root_iterative_a)) (set! square_root_iterative_i 0) (while (< square_root_iterative_i square_root_iterative_max_iter) (do (set! square_root_iterative_prev_value square_root_iterative_value) (set! square_root_iterative_value (- square_root_iterative_value (/ (fx square_root_iterative_value square_root_iterative_a) (fx_derivative square_root_iterative_value)))) (when (< (abs_float (- square_root_iterative_prev_value square_root_iterative_value)) square_root_iterative_tolerance) (throw (ex-info "return" {:v square_root_iterative_value}))) (set! square_root_iterative_i (+ square_root_iterative_i 1)))) (throw (ex-info "return" {:v square_root_iterative_value}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_r1 (square_root_iterative 4.0 9999 0.00000000000001))

(def ^:dynamic main_r2 (square_root_iterative 3.2 9999 0.00000000000001))

(def ^:dynamic main_r3 (square_root_iterative 140.0 9999 0.00000000000001))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str main_r1))
      (println (str main_r2))
      (println (str main_r3))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
