(ns main (:refer-clojure :exclude [to_radians sin_taylor cos_taylor tan_approx sqrtApprox atanApprox atan2Approx asinApprox haversine_distance]))

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

(declare to_radians sin_taylor cos_taylor tan_approx sqrtApprox atanApprox atan2Approx asinApprox haversine_distance)

(def ^:dynamic asinApprox_denom nil)

(def ^:dynamic asinApprox_res nil)

(def ^:dynamic atan2Approx_val nil)

(def ^:dynamic cos_taylor_i nil)

(def ^:dynamic cos_taylor_k1 nil)

(def ^:dynamic cos_taylor_k2 nil)

(def ^:dynamic cos_taylor_sum nil)

(def ^:dynamic cos_taylor_term nil)

(def ^:dynamic haversine_distance_flattening nil)

(def ^:dynamic haversine_distance_h_value nil)

(def ^:dynamic haversine_distance_lambda_1 nil)

(def ^:dynamic haversine_distance_lambda_2 nil)

(def ^:dynamic haversine_distance_phi_1 nil)

(def ^:dynamic haversine_distance_phi_2 nil)

(def ^:dynamic haversine_distance_sin_sq_lambda nil)

(def ^:dynamic haversine_distance_sin_sq_phi nil)

(def ^:dynamic sin_taylor_i nil)

(def ^:dynamic sin_taylor_k1 nil)

(def ^:dynamic sin_taylor_k2 nil)

(def ^:dynamic sin_taylor_sum nil)

(def ^:dynamic sin_taylor_term nil)

(def ^:dynamic sqrtApprox_guess nil)

(def ^:dynamic sqrtApprox_i nil)

(def ^:dynamic main_PI 3.141592653589793)

(def ^:dynamic main_AXIS_A 6378137.0)

(def ^:dynamic main_AXIS_B 6356752.314245)

(def ^:dynamic main_RADIUS 6378137.0)

(defn to_radians [to_radians_deg]
  (try (throw (ex-info "return" {:v (/ (* to_radians_deg main_PI) 180.0)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn sin_taylor [sin_taylor_x]
  (binding [sin_taylor_i nil sin_taylor_k1 nil sin_taylor_k2 nil sin_taylor_sum nil sin_taylor_term nil] (try (do (set! sin_taylor_term sin_taylor_x) (set! sin_taylor_sum sin_taylor_x) (set! sin_taylor_i 1) (while (< sin_taylor_i 10) (do (set! sin_taylor_k1 (* 2.0 (double sin_taylor_i))) (set! sin_taylor_k2 (+ sin_taylor_k1 1.0)) (set! sin_taylor_term (quot (* (* (- sin_taylor_term) sin_taylor_x) sin_taylor_x) (* sin_taylor_k1 sin_taylor_k2))) (set! sin_taylor_sum (+ sin_taylor_sum sin_taylor_term)) (set! sin_taylor_i (+ sin_taylor_i 1)))) (throw (ex-info "return" {:v sin_taylor_sum}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn cos_taylor [cos_taylor_x]
  (binding [cos_taylor_i nil cos_taylor_k1 nil cos_taylor_k2 nil cos_taylor_sum nil cos_taylor_term nil] (try (do (set! cos_taylor_term 1.0) (set! cos_taylor_sum 1.0) (set! cos_taylor_i 1) (while (< cos_taylor_i 10) (do (set! cos_taylor_k1 (- (* 2.0 (double cos_taylor_i)) 1.0)) (set! cos_taylor_k2 (* 2.0 (double cos_taylor_i))) (set! cos_taylor_term (quot (* (* (- cos_taylor_term) cos_taylor_x) cos_taylor_x) (* cos_taylor_k1 cos_taylor_k2))) (set! cos_taylor_sum (+ cos_taylor_sum cos_taylor_term)) (set! cos_taylor_i (+ cos_taylor_i 1)))) (throw (ex-info "return" {:v cos_taylor_sum}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn tan_approx [tan_approx_x]
  (try (throw (ex-info "return" {:v (quot (sin_taylor tan_approx_x) (cos_taylor tan_approx_x))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn sqrtApprox [sqrtApprox_x]
  (binding [sqrtApprox_guess nil sqrtApprox_i nil] (try (do (set! sqrtApprox_guess (/ sqrtApprox_x 2.0)) (set! sqrtApprox_i 0) (while (< sqrtApprox_i 20) (do (set! sqrtApprox_guess (/ (+ sqrtApprox_guess (quot sqrtApprox_x sqrtApprox_guess)) 2.0)) (set! sqrtApprox_i (+ sqrtApprox_i 1)))) (throw (ex-info "return" {:v sqrtApprox_guess}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn atanApprox [atanApprox_x]
  (try (do (when (> atanApprox_x 1.0) (throw (ex-info "return" {:v (- (/ main_PI 2.0) (quot atanApprox_x (+ (* atanApprox_x atanApprox_x) 0.28)))}))) (if (< atanApprox_x (- 1.0)) (- (/ (- main_PI) 2.0) (quot atanApprox_x (+ (* atanApprox_x atanApprox_x) 0.28))) (quot atanApprox_x (+ 1.0 (* (* 0.28 atanApprox_x) atanApprox_x))))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn atan2Approx [atan2Approx_y atan2Approx_x]
  (binding [atan2Approx_val nil] (try (do (when (> atan2Approx_x 0.0) (do (set! atan2Approx_val (atanApprox (quot atan2Approx_y atan2Approx_x))) (throw (ex-info "return" {:v atan2Approx_val})))) (when (< atan2Approx_x 0.0) (do (when (>= atan2Approx_y 0.0) (throw (ex-info "return" {:v (+ (atanApprox (quot atan2Approx_y atan2Approx_x)) main_PI)}))) (throw (ex-info "return" {:v (- (atanApprox (quot atan2Approx_y atan2Approx_x)) main_PI)})))) (when (> atan2Approx_y 0.0) (throw (ex-info "return" {:v (/ main_PI 2.0)}))) (if (< atan2Approx_y 0.0) (/ (- main_PI) 2.0) 0.0)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn asinApprox [asinApprox_x]
  (binding [asinApprox_denom nil asinApprox_res nil] (try (do (set! asinApprox_denom (sqrtApprox (- 1.0 (* asinApprox_x asinApprox_x)))) (set! asinApprox_res (atan2Approx asinApprox_x asinApprox_denom)) (throw (ex-info "return" {:v asinApprox_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn haversine_distance [haversine_distance_lat1 haversine_distance_lon1 haversine_distance_lat2 haversine_distance_lon2]
  (binding [haversine_distance_flattening nil haversine_distance_h_value nil haversine_distance_lambda_1 nil haversine_distance_lambda_2 nil haversine_distance_phi_1 nil haversine_distance_phi_2 nil haversine_distance_sin_sq_lambda nil haversine_distance_sin_sq_phi nil] (try (do (set! haversine_distance_flattening (/ (- main_AXIS_A main_AXIS_B) main_AXIS_A)) (set! haversine_distance_phi_1 (atanApprox (* (- 1.0 haversine_distance_flattening) (tan_approx (to_radians haversine_distance_lat1))))) (set! haversine_distance_phi_2 (atanApprox (* (- 1.0 haversine_distance_flattening) (tan_approx (to_radians haversine_distance_lat2))))) (set! haversine_distance_lambda_1 (to_radians haversine_distance_lon1)) (set! haversine_distance_lambda_2 (to_radians haversine_distance_lon2)) (set! haversine_distance_sin_sq_phi (sin_taylor (/ (- haversine_distance_phi_2 haversine_distance_phi_1) 2.0))) (set! haversine_distance_sin_sq_lambda (sin_taylor (/ (- haversine_distance_lambda_2 haversine_distance_lambda_1) 2.0))) (set! haversine_distance_sin_sq_phi (* haversine_distance_sin_sq_phi haversine_distance_sin_sq_phi)) (set! haversine_distance_sin_sq_lambda (* haversine_distance_sin_sq_lambda haversine_distance_sin_sq_lambda)) (set! haversine_distance_h_value (sqrtApprox (+ haversine_distance_sin_sq_phi (* (* (cos_taylor haversine_distance_phi_1) (cos_taylor haversine_distance_phi_2)) haversine_distance_sin_sq_lambda)))) (throw (ex-info "return" {:v (* (* 2.0 main_RADIUS) (asinApprox haversine_distance_h_value))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_SAN_FRANCISCO [37.774856 (- 122.424227)])

(def ^:dynamic main_YOSEMITE [37.864742 (- 119.537521)])

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (haversine_distance (nth main_SAN_FRANCISCO 0) (nth main_SAN_FRANCISCO 1) (nth main_YOSEMITE 0) (nth main_YOSEMITE 1))))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
