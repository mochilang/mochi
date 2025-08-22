(ns main (:refer-clojure :exclude [pow10 sqrt abs capture_radii capture_area run_tests main]))

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
  (int (Double/valueOf (str s))))

(defn mochi_str [v]
  (cond (float? v) (let [s (str v)] (if (clojure.string/ends-with? s ".0") (subs s 0 (- (count s) 2)) s)) :else (str v)))

(defn _fetch [url]
  {:data [{:from "" :intensity {:actual 0 :forecast 0 :index ""} :to ""}]})

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare pow10 sqrt abs capture_radii capture_area run_tests main)

(def ^:dynamic capture_area_sigma nil)

(def ^:dynamic capture_radii_capture_radius nil)

(def ^:dynamic capture_radii_denom nil)

(def ^:dynamic capture_radii_escape_velocity_squared nil)

(def ^:dynamic main_r nil)

(def ^:dynamic pow10_i nil)

(def ^:dynamic pow10_result nil)

(def ^:dynamic run_tests_a nil)

(def ^:dynamic run_tests_r nil)

(def ^:dynamic sqrt_guess nil)

(def ^:dynamic sqrt_i nil)

(def ^:dynamic main_G nil)

(def ^:dynamic main_C nil)

(def ^:dynamic main_PI nil)

(defn pow10 [pow10_n]
  (binding [pow10_i nil pow10_result nil] (try (do (set! pow10_result 1.0) (set! pow10_i 0) (while (< pow10_i pow10_n) (do (set! pow10_result (* pow10_result 10.0)) (set! pow10_i (+ pow10_i 1)))) (throw (ex-info "return" {:v pow10_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn sqrt [sqrt_x]
  (binding [sqrt_guess nil sqrt_i nil] (try (do (when (<= sqrt_x 0.0) (throw (ex-info "return" {:v 0.0}))) (set! sqrt_guess sqrt_x) (set! sqrt_i 0) (while (< sqrt_i 20) (do (set! sqrt_guess (/ (+ sqrt_guess (/ sqrt_x sqrt_guess)) 2.0)) (set! sqrt_i (+ sqrt_i 1)))) (throw (ex-info "return" {:v sqrt_guess}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn abs [abs_x]
  (try (if (< abs_x 0.0) (- abs_x) abs_x) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn capture_radii [capture_radii_target_body_radius capture_radii_target_body_mass capture_radii_projectile_velocity]
  (binding [capture_radii_capture_radius nil capture_radii_denom nil capture_radii_escape_velocity_squared nil] (try (do (when (< capture_radii_target_body_mass 0.0) (throw (Exception. "Mass cannot be less than 0"))) (when (< capture_radii_target_body_radius 0.0) (throw (Exception. "Radius cannot be less than 0"))) (when (> capture_radii_projectile_velocity main_C) (throw (Exception. "Cannot go beyond speed of light"))) (set! capture_radii_escape_velocity_squared (/ (* (* 2.0 main_G) capture_radii_target_body_mass) capture_radii_target_body_radius)) (set! capture_radii_denom (* capture_radii_projectile_velocity capture_radii_projectile_velocity)) (set! capture_radii_capture_radius (* capture_radii_target_body_radius (sqrt (+ 1.0 (/ capture_radii_escape_velocity_squared capture_radii_denom))))) (throw (ex-info "return" {:v capture_radii_capture_radius}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn capture_area [capture_area_capture_radius]
  (binding [capture_area_sigma nil] (try (do (when (< capture_area_capture_radius 0.0) (throw (Exception. "Cannot have a capture radius less than 0"))) (set! capture_area_sigma (* (* main_PI capture_area_capture_radius) capture_area_capture_radius)) (throw (ex-info "return" {:v capture_area_sigma}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn run_tests []
  (binding [run_tests_a nil run_tests_r nil] (do (set! run_tests_r (capture_radii (* 6.957 (pow10 8)) (* 1.99 (pow10 30)) 25000.0)) (when (> (abs (- run_tests_r (* 1.720959069143714 (pow10 10)))) 1.0) (throw (Exception. "capture_radii failed"))) (set! run_tests_a (capture_area run_tests_r)) (when (> (abs (- run_tests_a (* 9.304455331801812 (pow10 20)))) 1.0) (throw (Exception. "capture_area failed"))))))

(defn main []
  (binding [main_r nil] (do (run_tests) (set! main_r (capture_radii (* 6.957 (pow10 8)) (* 1.99 (pow10 30)) 25000.0)) (println (mochi_str main_r)) (println (mochi_str (capture_area main_r))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (alter-var-root (var main_G) (constantly 0.000000000066743))
      (alter-var-root (var main_C) (constantly 299792458.0))
      (alter-var-root (var main_PI) (constantly 3.141592653589793))
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
