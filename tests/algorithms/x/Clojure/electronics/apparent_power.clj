(ns main (:refer-clojure :exclude [abs to_radians sin_taylor cos_taylor rect multiply apparent_power approx_equal]))

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

(declare abs to_radians sin_taylor cos_taylor rect multiply apparent_power approx_equal)

(def ^:dynamic apparent_power_irad nil)

(def ^:dynamic apparent_power_irect nil)

(def ^:dynamic apparent_power_result nil)

(def ^:dynamic apparent_power_vrad nil)

(def ^:dynamic apparent_power_vrect nil)

(def ^:dynamic cos_taylor_i nil)

(def ^:dynamic cos_taylor_k1 nil)

(def ^:dynamic cos_taylor_k2 nil)

(def ^:dynamic cos_taylor_sum nil)

(def ^:dynamic cos_taylor_term nil)

(def ^:dynamic rect_c nil)

(def ^:dynamic rect_s nil)

(def ^:dynamic sin_taylor_i nil)

(def ^:dynamic sin_taylor_k1 nil)

(def ^:dynamic sin_taylor_k2 nil)

(def ^:dynamic sin_taylor_sum nil)

(def ^:dynamic sin_taylor_term nil)

(def ^:dynamic main_PI 3.141592653589793)

(defn abs [abs_x]
  (try (if (< abs_x 0.0) (- abs_x) abs_x) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn to_radians [to_radians_deg]
  (try (throw (ex-info "return" {:v (/ (* to_radians_deg main_PI) 180.0)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn sin_taylor [sin_taylor_x]
  (binding [sin_taylor_i nil sin_taylor_k1 nil sin_taylor_k2 nil sin_taylor_sum nil sin_taylor_term nil] (try (do (set! sin_taylor_term sin_taylor_x) (set! sin_taylor_sum sin_taylor_x) (set! sin_taylor_i 1) (while (< sin_taylor_i 10) (do (set! sin_taylor_k1 (* 2.0 (double sin_taylor_i))) (set! sin_taylor_k2 (+ sin_taylor_k1 1.0)) (set! sin_taylor_term (quot (* (* (- sin_taylor_term) sin_taylor_x) sin_taylor_x) (* sin_taylor_k1 sin_taylor_k2))) (set! sin_taylor_sum (+ sin_taylor_sum sin_taylor_term)) (set! sin_taylor_i (+ sin_taylor_i 1)))) (throw (ex-info "return" {:v sin_taylor_sum}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn cos_taylor [cos_taylor_x]
  (binding [cos_taylor_i nil cos_taylor_k1 nil cos_taylor_k2 nil cos_taylor_sum nil cos_taylor_term nil] (try (do (set! cos_taylor_term 1.0) (set! cos_taylor_sum 1.0) (set! cos_taylor_i 1) (while (< cos_taylor_i 10) (do (set! cos_taylor_k1 (- (* 2.0 (double cos_taylor_i)) 1.0)) (set! cos_taylor_k2 (* 2.0 (double cos_taylor_i))) (set! cos_taylor_term (quot (* (* (- cos_taylor_term) cos_taylor_x) cos_taylor_x) (* cos_taylor_k1 cos_taylor_k2))) (set! cos_taylor_sum (+ cos_taylor_sum cos_taylor_term)) (set! cos_taylor_i (+ cos_taylor_i 1)))) (throw (ex-info "return" {:v cos_taylor_sum}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn rect [rect_mag rect_angle]
  (binding [rect_c nil rect_s nil] (try (do (set! rect_c (cos_taylor rect_angle)) (set! rect_s (sin_taylor rect_angle)) (throw (ex-info "return" {:v [(* rect_mag rect_c) (* rect_mag rect_s)]}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn multiply [multiply_a multiply_b]
  (try (throw (ex-info "return" {:v [(- (* (nth multiply_a 0) (nth multiply_b 0)) (* (nth multiply_a 1) (nth multiply_b 1))) (+ (* (nth multiply_a 0) (nth multiply_b 1)) (* (nth multiply_a 1) (nth multiply_b 0)))]})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn apparent_power [apparent_power_voltage apparent_power_current apparent_power_voltage_angle apparent_power_current_angle]
  (binding [apparent_power_irad nil apparent_power_irect nil apparent_power_result nil apparent_power_vrad nil apparent_power_vrect nil] (try (do (set! apparent_power_vrad (to_radians apparent_power_voltage_angle)) (set! apparent_power_irad (to_radians apparent_power_current_angle)) (set! apparent_power_vrect (rect apparent_power_voltage apparent_power_vrad)) (set! apparent_power_irect (rect apparent_power_current apparent_power_irad)) (set! apparent_power_result (multiply apparent_power_vrect apparent_power_irect)) (throw (ex-info "return" {:v apparent_power_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn approx_equal [approx_equal_a approx_equal_b approx_equal_eps]
  (try (throw (ex-info "return" {:v (and (< (abs (- (nth approx_equal_a 0) (nth approx_equal_b 0))) approx_equal_eps) (< (abs (- (nth approx_equal_a 1) (nth approx_equal_b 1))) approx_equal_eps))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]

      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
