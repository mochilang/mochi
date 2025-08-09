(ns main (:refer-clojure :exclude [abs floor pow factorial radians taylor_sin test_sin main]))

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

(declare abs floor pow factorial radians taylor_sin test_sin main)

(def ^:dynamic factorial_i nil)

(def ^:dynamic factorial_result nil)

(def ^:dynamic floor_i nil)

(def ^:dynamic main_res nil)

(def ^:dynamic pow_i nil)

(def ^:dynamic pow_result nil)

(def ^:dynamic taylor_sin_a nil)

(def ^:dynamic taylor_sin_angle nil)

(def ^:dynamic taylor_sin_angle_in_radians nil)

(def ^:dynamic taylor_sin_i nil)

(def ^:dynamic taylor_sin_k nil)

(def ^:dynamic taylor_sin_result nil)

(def ^:dynamic taylor_sin_sign nil)

(def ^:dynamic test_sin_eps nil)

(def ^:dynamic main_PI 3.141592653589793)

(defn abs [abs_x]
  (try (if (< abs_x 0.0) (- abs_x) abs_x) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn floor [floor_x]
  (binding [floor_i nil] (try (do (set! floor_i (long floor_x)) (when (> (double floor_i) floor_x) (set! floor_i (- floor_i 1))) (throw (ex-info "return" {:v (double floor_i)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn pow [pow_x pow_n]
  (binding [pow_i nil pow_result nil] (try (do (set! pow_result 1.0) (set! pow_i 0) (while (< pow_i pow_n) (do (set! pow_result (* pow_result pow_x)) (set! pow_i (+ pow_i 1)))) (throw (ex-info "return" {:v pow_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn factorial [factorial_n]
  (binding [factorial_i nil factorial_result nil] (try (do (set! factorial_result 1.0) (set! factorial_i 2) (while (<= factorial_i factorial_n) (do (set! factorial_result (* factorial_result (double factorial_i))) (set! factorial_i (+ factorial_i 1)))) (throw (ex-info "return" {:v factorial_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn radians [radians_deg]
  (try (throw (ex-info "return" {:v (/ (* radians_deg main_PI) 180.0)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn taylor_sin [taylor_sin_angle_in_degrees taylor_sin_accuracy taylor_sin_rounded_values_count]
  (binding [taylor_sin_a nil taylor_sin_angle nil taylor_sin_angle_in_radians nil taylor_sin_i nil taylor_sin_k nil taylor_sin_result nil taylor_sin_sign nil] (try (do (set! taylor_sin_k (floor (/ taylor_sin_angle_in_degrees 360.0))) (set! taylor_sin_angle (- taylor_sin_angle_in_degrees (* taylor_sin_k 360.0))) (set! taylor_sin_angle_in_radians (radians taylor_sin_angle)) (set! taylor_sin_result taylor_sin_angle_in_radians) (set! taylor_sin_a 3) (set! taylor_sin_sign (- 1.0)) (set! taylor_sin_i 0) (while (< taylor_sin_i taylor_sin_accuracy) (do (set! taylor_sin_result (+ taylor_sin_result (/ (* taylor_sin_sign (pow taylor_sin_angle_in_radians taylor_sin_a)) (factorial taylor_sin_a)))) (set! taylor_sin_sign (- taylor_sin_sign)) (set! taylor_sin_a (+ taylor_sin_a 2)) (set! taylor_sin_i (+ taylor_sin_i 1)))) (throw (ex-info "return" {:v taylor_sin_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn test_sin []
  (binding [test_sin_eps nil] (do (set! test_sin_eps 0.0000001) (when (> (abs (- (taylor_sin 0.0 18 10) 0.0)) test_sin_eps) (throw (Exception. "sin(0) failed"))) (when (> (abs (- (taylor_sin 90.0 18 10) 1.0)) test_sin_eps) (throw (Exception. "sin(90) failed"))) (when (> (abs (- (taylor_sin 180.0 18 10) 0.0)) test_sin_eps) (throw (Exception. "sin(180) failed"))) (when (> (abs (- (taylor_sin 270.0 18 10) (- 1.0))) test_sin_eps) (throw (Exception. "sin(270) failed"))))))

(defn main []
  (binding [main_res nil] (do (test_sin) (set! main_res (taylor_sin 64.0 18 10)) (println main_res))))

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
