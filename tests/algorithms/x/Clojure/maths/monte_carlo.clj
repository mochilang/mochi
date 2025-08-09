(ns main (:refer-clojure :exclude [rand_float rand_range abs_float sqrtApprox pi_estimator area_under_curve_estimator area_under_line_estimator_check pi_estimator_using_area_under_curve main]))

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

(declare rand_float rand_range abs_float sqrtApprox pi_estimator area_under_curve_estimator area_under_line_estimator_check pi_estimator_using_area_under_curve main)

(def ^:dynamic area_under_curve_estimator_expected nil)

(def ^:dynamic area_under_curve_estimator_i nil)

(def ^:dynamic area_under_curve_estimator_sum nil)

(def ^:dynamic area_under_curve_estimator_x nil)

(def ^:dynamic area_under_line_estimator_check_estimated_value nil)

(def ^:dynamic area_under_line_estimator_check_expected_value nil)

(def ^:dynamic pi_estimator_i nil)

(def ^:dynamic pi_estimator_inside nil)

(def ^:dynamic pi_estimator_pi_estimate nil)

(def ^:dynamic pi_estimator_proportion nil)

(def ^:dynamic pi_estimator_using_area_under_curve_estimated_value nil)

(def ^:dynamic pi_estimator_x nil)

(def ^:dynamic pi_estimator_y nil)

(def ^:dynamic semi_circle_s nil)

(def ^:dynamic semi_circle_y nil)

(def ^:dynamic sqrtApprox_guess nil)

(def ^:dynamic sqrtApprox_i nil)

(def ^:dynamic main_PI 3.141592653589793)

(def ^:dynamic main_rand_seed 123456789)

(defn rand_float []
  (try (do (alter-var-root (var main_rand_seed) (fn [_] (mod (+ (* 1103515245 main_rand_seed) 12345) 2147483648))) (throw (ex-info "return" {:v (/ (double main_rand_seed) 2147483648.0)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn rand_range [rand_range_min_val rand_range_max_val]
  (try (throw (ex-info "return" {:v (+ (* (rand_float) (- rand_range_max_val rand_range_min_val)) rand_range_min_val)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn abs_float [abs_float_x]
  (try (if (< abs_float_x 0.0) (- abs_float_x) abs_float_x) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn sqrtApprox [sqrtApprox_x]
  (binding [sqrtApprox_guess nil sqrtApprox_i nil] (try (do (when (= sqrtApprox_x 0.0) (throw (ex-info "return" {:v 0.0}))) (set! sqrtApprox_guess (/ sqrtApprox_x 2.0)) (set! sqrtApprox_i 0) (while (< sqrtApprox_i 20) (do (set! sqrtApprox_guess (/ (+ sqrtApprox_guess (quot sqrtApprox_x sqrtApprox_guess)) 2.0)) (set! sqrtApprox_i (+ sqrtApprox_i 1)))) (throw (ex-info "return" {:v sqrtApprox_guess}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn pi_estimator [pi_estimator_iterations]
  (binding [pi_estimator_i nil pi_estimator_inside nil pi_estimator_pi_estimate nil pi_estimator_proportion nil pi_estimator_x nil pi_estimator_y nil] (do (set! pi_estimator_inside 0.0) (set! pi_estimator_i 0) (while (< pi_estimator_i pi_estimator_iterations) (do (set! pi_estimator_x (rand_range (- 1.0) 1.0)) (set! pi_estimator_y (rand_range (- 1.0) 1.0)) (when (<= (+ (* pi_estimator_x pi_estimator_x) (* pi_estimator_y pi_estimator_y)) 1.0) (set! pi_estimator_inside (+ pi_estimator_inside 1.0))) (set! pi_estimator_i (+ pi_estimator_i 1)))) (set! pi_estimator_proportion (quot pi_estimator_inside (double pi_estimator_iterations))) (set! pi_estimator_pi_estimate (* pi_estimator_proportion 4.0)) (println "The estimated value of pi is" pi_estimator_pi_estimate) (println "The numpy value of pi is" main_PI) (println "The total error is" (abs_float (- main_PI pi_estimator_pi_estimate))) pi_estimator_iterations)))

(defn area_under_curve_estimator [area_under_curve_estimator_iterations area_under_curve_estimator_f area_under_curve_estimator_min_value area_under_curve_estimator_max_value]
  (binding [area_under_curve_estimator_expected nil area_under_curve_estimator_i nil area_under_curve_estimator_sum nil area_under_curve_estimator_x nil] (try (do (set! area_under_curve_estimator_sum 0.0) (set! area_under_curve_estimator_i 0) (while (< area_under_curve_estimator_i area_under_curve_estimator_iterations) (do (set! area_under_curve_estimator_x (rand_range area_under_curve_estimator_min_value area_under_curve_estimator_max_value)) (set! area_under_curve_estimator_sum (+ area_under_curve_estimator_sum (area_under_curve_estimator_f area_under_curve_estimator_x))) (set! area_under_curve_estimator_i (+ area_under_curve_estimator_i 1)))) (set! area_under_curve_estimator_expected (quot area_under_curve_estimator_sum (double area_under_curve_estimator_iterations))) (throw (ex-info "return" {:v (* area_under_curve_estimator_expected (- area_under_curve_estimator_max_value area_under_curve_estimator_min_value))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn identity_function [area_under_line_estimator_check_iterations area_under_line_estimator_check_min_value area_under_line_estimator_check_max_value identity_function_x]
  (try (throw (ex-info "return" {:v identity_function_x})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn area_under_line_estimator_check [area_under_line_estimator_check_iterations area_under_line_estimator_check_min_value area_under_line_estimator_check_max_value]
  (binding [area_under_line_estimator_check_estimated_value nil area_under_line_estimator_check_expected_value nil] (try (do (set! area_under_line_estimator_check_estimated_value (area_under_curve_estimator area_under_line_estimator_check_iterations identity_function area_under_line_estimator_check_min_value area_under_line_estimator_check_max_value)) (set! area_under_line_estimator_check_expected_value (/ (- (* area_under_line_estimator_check_max_value area_under_line_estimator_check_max_value) (* area_under_line_estimator_check_min_value area_under_line_estimator_check_min_value)) 2.0)) (println "******************") (println "Estimating area under y=x where x varies from" area_under_line_estimator_check_min_value) (println "Estimated value is" area_under_line_estimator_check_estimated_value) (println "Expected value is" area_under_line_estimator_check_expected_value) (println "Total error is" (abs_float (- area_under_line_estimator_check_estimated_value area_under_line_estimator_check_expected_value))) (println "******************")) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn semi_circle [pi_estimator_using_area_under_curve_iterations semi_circle_x]
  (binding [semi_circle_s nil semi_circle_y nil] (try (do (set! semi_circle_y (- 4.0 (* semi_circle_x semi_circle_x))) (set! semi_circle_s (sqrtApprox semi_circle_y)) (throw (ex-info "return" {:v semi_circle_s}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn pi_estimator_using_area_under_curve [pi_estimator_using_area_under_curve_iterations]
  (binding [pi_estimator_using_area_under_curve_estimated_value nil] (try (do (set! pi_estimator_using_area_under_curve_estimated_value (area_under_curve_estimator pi_estimator_using_area_under_curve_iterations semi_circle 0.0 2.0)) (println "******************") (println "Estimating pi using area_under_curve_estimator") (println "Estimated value is" pi_estimator_using_area_under_curve_estimated_value) (println "Expected value is" main_PI) (println "Total error is" (abs_float (- pi_estimator_using_area_under_curve_estimated_value main_PI))) (println "******************")) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (do (pi_estimator 1000) (area_under_line_estimator_check 1000 0.0 1.0) (pi_estimator_using_area_under_curve 1000)))

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
