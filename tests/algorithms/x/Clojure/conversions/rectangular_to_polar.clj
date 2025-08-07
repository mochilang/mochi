(ns main (:refer-clojure :exclude [sqrtApprox atanApprox atan2Approx deg floor pow10 round rectangular_to_polar show]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(defn indexOf [s sub]
  (let [idx (clojure.string/index-of s sub)] (if (nil? idx) -1 idx)))

(defn split [s sep]
  (clojure.string/split s (re-pattern sep)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare sqrtApprox atanApprox atan2Approx deg floor pow10 round rectangular_to_polar show)

(def ^:dynamic atan2Approx_r nil)

(def ^:dynamic floor_i nil)

(def ^:dynamic pow10_i nil)

(def ^:dynamic pow10_p nil)

(def ^:dynamic rectangular_to_polar_ang nil)

(def ^:dynamic rectangular_to_polar_mod nil)

(def ^:dynamic round_m nil)

(def ^:dynamic show_r nil)

(def ^:dynamic sqrtApprox_guess nil)

(def ^:dynamic sqrtApprox_i nil)

(def ^:dynamic main_PI 3.141592653589793)

(defn sqrtApprox [sqrtApprox_x]
  (binding [sqrtApprox_guess nil sqrtApprox_i nil] (try (do (set! sqrtApprox_guess (/ sqrtApprox_x 2.0)) (set! sqrtApprox_i 0) (while (< sqrtApprox_i 20) (do (set! sqrtApprox_guess (/ (+ sqrtApprox_guess (/ sqrtApprox_x sqrtApprox_guess)) 2.0)) (set! sqrtApprox_i (+ sqrtApprox_i 1)))) (throw (ex-info "return" {:v sqrtApprox_guess}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn atanApprox [atanApprox_x]
  (try (do (when (> atanApprox_x 1.0) (throw (ex-info "return" {:v (- (/ main_PI 2.0) (/ atanApprox_x (+ (* atanApprox_x atanApprox_x) 0.28)))}))) (if (< atanApprox_x (- 1.0)) (- (/ (- main_PI) 2.0) (/ atanApprox_x (+ (* atanApprox_x atanApprox_x) 0.28))) (/ atanApprox_x (+ 1.0 (* (* 0.28 atanApprox_x) atanApprox_x))))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn atan2Approx [atan2Approx_y atan2Approx_x]
  (binding [atan2Approx_r nil] (try (do (when (> atan2Approx_x 0.0) (do (set! atan2Approx_r (atanApprox (/ atan2Approx_y atan2Approx_x))) (throw (ex-info "return" {:v atan2Approx_r})))) (when (< atan2Approx_x 0.0) (do (when (>= atan2Approx_y 0.0) (throw (ex-info "return" {:v (+ (atanApprox (/ atan2Approx_y atan2Approx_x)) main_PI)}))) (throw (ex-info "return" {:v (- (atanApprox (/ atan2Approx_y atan2Approx_x)) main_PI)})))) (when (> atan2Approx_y 0.0) (throw (ex-info "return" {:v (/ main_PI 2.0)}))) (if (< atan2Approx_y 0.0) (/ (- main_PI) 2.0) 0.0)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn deg [deg_rad]
  (try (throw (ex-info "return" {:v (/ (* deg_rad 180.0) main_PI)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn floor [floor_x]
  (binding [floor_i nil] (try (do (set! floor_i (long floor_x)) (when (> (double floor_i) floor_x) (set! floor_i (- floor_i 1))) (throw (ex-info "return" {:v (double floor_i)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn pow10 [pow10_n]
  (binding [pow10_i nil pow10_p nil] (try (do (set! pow10_p 1.0) (set! pow10_i 0) (while (< pow10_i pow10_n) (do (set! pow10_p (* pow10_p 10.0)) (set! pow10_i (+ pow10_i 1)))) (throw (ex-info "return" {:v pow10_p}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn round [round_x round_n]
  (binding [round_m nil] (try (do (set! round_m (pow10 round_n)) (throw (ex-info "return" {:v (/ (floor (+ (* round_x round_m) 0.5)) round_m)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn rectangular_to_polar [rectangular_to_polar_real rectangular_to_polar_img]
  (binding [rectangular_to_polar_ang nil rectangular_to_polar_mod nil] (try (do (set! rectangular_to_polar_mod (round (sqrtApprox (+ (* rectangular_to_polar_real rectangular_to_polar_real) (* rectangular_to_polar_img rectangular_to_polar_img))) 2)) (set! rectangular_to_polar_ang (round (deg (atan2Approx rectangular_to_polar_img rectangular_to_polar_real)) 2)) (throw (ex-info "return" {:v [rectangular_to_polar_mod rectangular_to_polar_ang]}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn show [show_real show_img]
  (binding [show_r nil] (do (set! show_r (rectangular_to_polar show_real show_img)) (println (str show_r)))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (show 5.0 (- 5.0))
      (show (- 1.0) 1.0)
      (show (- 1.0) (- 1.0))
      (show 0.0000000001 0.0000000001)
      (show (- 0.0000000001) 0.0000000001)
      (show 9.75 5.93)
      (show 10000.0 99999.0)
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
