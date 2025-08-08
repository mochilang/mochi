(ns main (:refer-clojure :exclude [to_radians sin_approx cos_approx sqrt_approx lamberts_ellipsoidal_distance]))

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

(declare to_radians sin_approx cos_approx sqrt_approx lamberts_ellipsoidal_distance)

(def ^:dynamic cos_approx_i nil)

(def ^:dynamic cos_approx_k1 nil)

(def ^:dynamic cos_approx_k2 nil)

(def ^:dynamic cos_approx_sum nil)

(def ^:dynamic cos_approx_term nil)

(def ^:dynamic lamberts_ellipsoidal_distance_lambda1 nil)

(def ^:dynamic lamberts_ellipsoidal_distance_lambda2 nil)

(def ^:dynamic lamberts_ellipsoidal_distance_phi1 nil)

(def ^:dynamic lamberts_ellipsoidal_distance_phi2 nil)

(def ^:dynamic lamberts_ellipsoidal_distance_x nil)

(def ^:dynamic lamberts_ellipsoidal_distance_y nil)

(def ^:dynamic sin_approx_i nil)

(def ^:dynamic sin_approx_k1 nil)

(def ^:dynamic sin_approx_k2 nil)

(def ^:dynamic sin_approx_sum nil)

(def ^:dynamic sin_approx_term nil)

(def ^:dynamic sqrt_approx_guess nil)

(def ^:dynamic sqrt_approx_i nil)

(def ^:dynamic main_PI 3.141592653589793)

(def ^:dynamic main_EQUATORIAL_RADIUS 6378137.0)

(defn to_radians [to_radians_deg]
  (try (throw (ex-info "return" {:v (/ (* to_radians_deg main_PI) 180.0)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn sin_approx [sin_approx_x]
  (binding [sin_approx_i nil sin_approx_k1 nil sin_approx_k2 nil sin_approx_sum nil sin_approx_term nil] (try (do (set! sin_approx_term sin_approx_x) (set! sin_approx_sum sin_approx_x) (set! sin_approx_i 1) (while (< sin_approx_i 10) (do (set! sin_approx_k1 (* 2.0 (double sin_approx_i))) (set! sin_approx_k2 (+ sin_approx_k1 1.0)) (set! sin_approx_term (quot (* (* (- sin_approx_term) sin_approx_x) sin_approx_x) (* sin_approx_k1 sin_approx_k2))) (set! sin_approx_sum (+ sin_approx_sum sin_approx_term)) (set! sin_approx_i (+ sin_approx_i 1)))) (throw (ex-info "return" {:v sin_approx_sum}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn cos_approx [cos_approx_x]
  (binding [cos_approx_i nil cos_approx_k1 nil cos_approx_k2 nil cos_approx_sum nil cos_approx_term nil] (try (do (set! cos_approx_term 1.0) (set! cos_approx_sum 1.0) (set! cos_approx_i 1) (while (< cos_approx_i 10) (do (set! cos_approx_k1 (- (* 2.0 (double cos_approx_i)) 1.0)) (set! cos_approx_k2 (* 2.0 (double cos_approx_i))) (set! cos_approx_term (quot (* (* (- cos_approx_term) cos_approx_x) cos_approx_x) (* cos_approx_k1 cos_approx_k2))) (set! cos_approx_sum (+ cos_approx_sum cos_approx_term)) (set! cos_approx_i (+ cos_approx_i 1)))) (throw (ex-info "return" {:v cos_approx_sum}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn sqrt_approx [sqrt_approx_x]
  (binding [sqrt_approx_guess nil sqrt_approx_i nil] (try (do (when (<= sqrt_approx_x 0.0) (throw (ex-info "return" {:v 0.0}))) (set! sqrt_approx_guess (/ sqrt_approx_x 2.0)) (set! sqrt_approx_i 0) (while (< sqrt_approx_i 20) (do (set! sqrt_approx_guess (/ (+ sqrt_approx_guess (quot sqrt_approx_x sqrt_approx_guess)) 2.0)) (set! sqrt_approx_i (+ sqrt_approx_i 1)))) (throw (ex-info "return" {:v sqrt_approx_guess}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn lamberts_ellipsoidal_distance [lamberts_ellipsoidal_distance_lat1 lamberts_ellipsoidal_distance_lon1 lamberts_ellipsoidal_distance_lat2 lamberts_ellipsoidal_distance_lon2]
  (binding [lamberts_ellipsoidal_distance_lambda1 nil lamberts_ellipsoidal_distance_lambda2 nil lamberts_ellipsoidal_distance_phi1 nil lamberts_ellipsoidal_distance_phi2 nil lamberts_ellipsoidal_distance_x nil lamberts_ellipsoidal_distance_y nil] (try (do (set! lamberts_ellipsoidal_distance_phi1 (to_radians lamberts_ellipsoidal_distance_lat1)) (set! lamberts_ellipsoidal_distance_phi2 (to_radians lamberts_ellipsoidal_distance_lat2)) (set! lamberts_ellipsoidal_distance_lambda1 (to_radians lamberts_ellipsoidal_distance_lon1)) (set! lamberts_ellipsoidal_distance_lambda2 (to_radians lamberts_ellipsoidal_distance_lon2)) (set! lamberts_ellipsoidal_distance_x (* (- lamberts_ellipsoidal_distance_lambda2 lamberts_ellipsoidal_distance_lambda1) (cos_approx (/ (+ lamberts_ellipsoidal_distance_phi1 lamberts_ellipsoidal_distance_phi2) 2.0)))) (set! lamberts_ellipsoidal_distance_y (- lamberts_ellipsoidal_distance_phi2 lamberts_ellipsoidal_distance_phi1)) (throw (ex-info "return" {:v (* main_EQUATORIAL_RADIUS (sqrt_approx (+ (* lamberts_ellipsoidal_distance_x lamberts_ellipsoidal_distance_x) (* lamberts_ellipsoidal_distance_y lamberts_ellipsoidal_distance_y))))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (lamberts_ellipsoidal_distance 37.774856 (- 122.424227) 37.864742 (- 119.537521)))
      (println (lamberts_ellipsoidal_distance 37.774856 (- 122.424227) 40.713019 (- 74.012647)))
      (println (lamberts_ellipsoidal_distance 37.774856 (- 122.424227) 45.443012 12.313071))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
