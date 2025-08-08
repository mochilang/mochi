(ns main (:refer-clojure :exclude [floor modf sin_taylor cos_taylor convert_to_2d rotate]))

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

(declare floor modf sin_taylor cos_taylor convert_to_2d rotate)

(def ^:dynamic convert_to_2d_projected_x nil)

(def ^:dynamic convert_to_2d_projected_y nil)

(def ^:dynamic cos_taylor_i nil)

(def ^:dynamic cos_taylor_k1 nil)

(def ^:dynamic cos_taylor_k2 nil)

(def ^:dynamic cos_taylor_sum nil)

(def ^:dynamic cos_taylor_term nil)

(def ^:dynamic floor_i nil)

(def ^:dynamic rotate_angle nil)

(def ^:dynamic rotate_new_x nil)

(def ^:dynamic rotate_new_y nil)

(def ^:dynamic rotate_new_z nil)

(def ^:dynamic sin_taylor_i nil)

(def ^:dynamic sin_taylor_k1 nil)

(def ^:dynamic sin_taylor_k2 nil)

(def ^:dynamic sin_taylor_sum nil)

(def ^:dynamic sin_taylor_term nil)

(def ^:dynamic main_PI 3.141592653589793)

(defn floor [floor_x]
  (binding [floor_i nil] (try (do (set! floor_i (long floor_x)) (when (> (double floor_i) floor_x) (set! floor_i (- floor_i 1))) (throw (ex-info "return" {:v (double floor_i)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn modf [modf_x modf_m]
  (try (throw (ex-info "return" {:v (- modf_x (* (floor (quot modf_x modf_m)) modf_m))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn sin_taylor [sin_taylor_x]
  (binding [sin_taylor_i nil sin_taylor_k1 nil sin_taylor_k2 nil sin_taylor_sum nil sin_taylor_term nil] (try (do (set! sin_taylor_term sin_taylor_x) (set! sin_taylor_sum sin_taylor_x) (set! sin_taylor_i 1) (while (< sin_taylor_i 10) (do (set! sin_taylor_k1 (* 2.0 (double sin_taylor_i))) (set! sin_taylor_k2 (+ sin_taylor_k1 1.0)) (set! sin_taylor_term (quot (* (* (- sin_taylor_term) sin_taylor_x) sin_taylor_x) (* sin_taylor_k1 sin_taylor_k2))) (set! sin_taylor_sum (+ sin_taylor_sum sin_taylor_term)) (set! sin_taylor_i (+ sin_taylor_i 1)))) (throw (ex-info "return" {:v sin_taylor_sum}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn cos_taylor [cos_taylor_x]
  (binding [cos_taylor_i nil cos_taylor_k1 nil cos_taylor_k2 nil cos_taylor_sum nil cos_taylor_term nil] (try (do (set! cos_taylor_term 1.0) (set! cos_taylor_sum 1.0) (set! cos_taylor_i 1) (while (< cos_taylor_i 10) (do (set! cos_taylor_k1 (- (* 2.0 (double cos_taylor_i)) 1.0)) (set! cos_taylor_k2 (* 2.0 (double cos_taylor_i))) (set! cos_taylor_term (quot (* (* (- cos_taylor_term) cos_taylor_x) cos_taylor_x) (* cos_taylor_k1 cos_taylor_k2))) (set! cos_taylor_sum (+ cos_taylor_sum cos_taylor_term)) (set! cos_taylor_i (+ cos_taylor_i 1)))) (throw (ex-info "return" {:v cos_taylor_sum}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn convert_to_2d [convert_to_2d_x convert_to_2d_y convert_to_2d_z convert_to_2d_scale convert_to_2d_distance]
  (binding [convert_to_2d_projected_x nil convert_to_2d_projected_y nil] (try (do (set! convert_to_2d_projected_x (* (quot (* convert_to_2d_x convert_to_2d_distance) (+ convert_to_2d_z convert_to_2d_distance)) convert_to_2d_scale)) (set! convert_to_2d_projected_y (* (quot (* convert_to_2d_y convert_to_2d_distance) (+ convert_to_2d_z convert_to_2d_distance)) convert_to_2d_scale)) (throw (ex-info "return" {:v [convert_to_2d_projected_x convert_to_2d_projected_y]}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn rotate [rotate_x rotate_y rotate_z rotate_axis rotate_angle_p]
  (binding [rotate_angle nil rotate_new_x nil rotate_new_y nil rotate_new_z nil] (try (do (set! rotate_angle rotate_angle_p) (set! rotate_angle (/ (* (/ (modf rotate_angle 360.0) 450.0) 180.0) main_PI)) (set! rotate_angle (modf rotate_angle (* 2.0 main_PI))) (when (> rotate_angle main_PI) (set! rotate_angle (- rotate_angle (* 2.0 main_PI)))) (when (= rotate_axis "z") (do (set! rotate_new_x (- (* rotate_x (cos_taylor rotate_angle)) (* rotate_y (sin_taylor rotate_angle)))) (set! rotate_new_y (+ (* rotate_y (cos_taylor rotate_angle)) (* rotate_x (sin_taylor rotate_angle)))) (set! rotate_new_z rotate_z) (throw (ex-info "return" {:v [rotate_new_x rotate_new_y rotate_new_z]})))) (when (= rotate_axis "x") (do (set! rotate_new_y (- (* rotate_y (cos_taylor rotate_angle)) (* rotate_z (sin_taylor rotate_angle)))) (set! rotate_new_z (+ (* rotate_z (cos_taylor rotate_angle)) (* rotate_y (sin_taylor rotate_angle)))) (set! rotate_new_x rotate_x) (throw (ex-info "return" {:v [rotate_new_x rotate_new_y rotate_new_z]})))) (when (= rotate_axis "y") (do (set! rotate_new_x (- (* rotate_x (cos_taylor rotate_angle)) (* rotate_z (sin_taylor rotate_angle)))) (set! rotate_new_z (+ (* rotate_z (cos_taylor rotate_angle)) (* rotate_x (sin_taylor rotate_angle)))) (set! rotate_new_y rotate_y) (throw (ex-info "return" {:v [rotate_new_x rotate_new_y rotate_new_z]})))) (println "not a valid axis, choose one of 'x', 'y', 'z'") (throw (ex-info "return" {:v [0.0 0.0 0.0]}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (convert_to_2d 1.0 2.0 3.0 10.0 10.0)))
      (println (str (rotate 1.0 2.0 3.0 "y" 90.0)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
