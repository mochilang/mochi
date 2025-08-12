(ns main (:refer-clojure :exclude [_mod sin_approx cos_approx polar_force abs_float in_static_equilibrium]))

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

(defn _fetch [url]
  {:data [{:from "" :intensity {:actual 0 :forecast 0 :index ""} :to ""}]})

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare _mod sin_approx cos_approx polar_force abs_float in_static_equilibrium)

(def ^:dynamic cos_approx_y nil)

(def ^:dynamic cos_approx_y2 nil)

(def ^:dynamic cos_approx_y4 nil)

(def ^:dynamic cos_approx_y6 nil)

(def ^:dynamic in_static_equilibrium_f nil)

(def ^:dynamic in_static_equilibrium_i nil)

(def ^:dynamic in_static_equilibrium_moment nil)

(def ^:dynamic in_static_equilibrium_n nil)

(def ^:dynamic in_static_equilibrium_r nil)

(def ^:dynamic in_static_equilibrium_sum_moments nil)

(def ^:dynamic polar_force_theta nil)

(def ^:dynamic sin_approx_y nil)

(def ^:dynamic sin_approx_y2 nil)

(def ^:dynamic sin_approx_y3 nil)

(def ^:dynamic sin_approx_y5 nil)

(def ^:dynamic sin_approx_y7 nil)

(def ^:dynamic main_PI 3.141592653589793)

(def ^:dynamic main_TWO_PI 6.283185307179586)

(defn _mod [_mod_x _mod_m]
  (try (throw (ex-info "return" {:v (- _mod_x (* (double (toi (/ _mod_x _mod_m))) _mod_m))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn sin_approx [sin_approx_x]
  (binding [sin_approx_y nil sin_approx_y2 nil sin_approx_y3 nil sin_approx_y5 nil sin_approx_y7 nil] (try (do (set! sin_approx_y (- (_mod (+ sin_approx_x main_PI) main_TWO_PI) main_PI)) (set! sin_approx_y2 (* sin_approx_y sin_approx_y)) (set! sin_approx_y3 (* sin_approx_y2 sin_approx_y)) (set! sin_approx_y5 (* sin_approx_y3 sin_approx_y2)) (set! sin_approx_y7 (* sin_approx_y5 sin_approx_y2)) (throw (ex-info "return" {:v (- (+ (- sin_approx_y (/ sin_approx_y3 6.0)) (/ sin_approx_y5 120.0)) (/ sin_approx_y7 5040.0))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn cos_approx [cos_approx_x]
  (binding [cos_approx_y nil cos_approx_y2 nil cos_approx_y4 nil cos_approx_y6 nil] (try (do (set! cos_approx_y (- (_mod (+ cos_approx_x main_PI) main_TWO_PI) main_PI)) (set! cos_approx_y2 (* cos_approx_y cos_approx_y)) (set! cos_approx_y4 (* cos_approx_y2 cos_approx_y2)) (set! cos_approx_y6 (* cos_approx_y4 cos_approx_y2)) (throw (ex-info "return" {:v (- (+ (- 1.0 (/ cos_approx_y2 2.0)) (/ cos_approx_y4 24.0)) (/ cos_approx_y6 720.0))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn polar_force [polar_force_magnitude polar_force_angle polar_force_radian_mode]
  (binding [polar_force_theta nil] (try (do (set! polar_force_theta (if polar_force_radian_mode polar_force_angle (/ (* polar_force_angle main_PI) 180.0))) (throw (ex-info "return" {:v [(* polar_force_magnitude (cos_approx polar_force_theta)) (* polar_force_magnitude (sin_approx polar_force_theta))]}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn abs_float [abs_float_x]
  (try (if (< abs_float_x 0.0) (throw (ex-info "return" {:v (- abs_float_x)})) (throw (ex-info "return" {:v abs_float_x}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn in_static_equilibrium [in_static_equilibrium_forces in_static_equilibrium_location in_static_equilibrium_eps]
  (binding [in_static_equilibrium_f nil in_static_equilibrium_i nil in_static_equilibrium_moment nil in_static_equilibrium_n nil in_static_equilibrium_r nil in_static_equilibrium_sum_moments nil] (try (do (set! in_static_equilibrium_sum_moments 0.0) (set! in_static_equilibrium_i 0) (set! in_static_equilibrium_n (count in_static_equilibrium_forces)) (while (< in_static_equilibrium_i in_static_equilibrium_n) (do (set! in_static_equilibrium_r (nth in_static_equilibrium_location in_static_equilibrium_i)) (set! in_static_equilibrium_f (nth in_static_equilibrium_forces in_static_equilibrium_i)) (set! in_static_equilibrium_moment (- (* (nth in_static_equilibrium_r 0) (nth in_static_equilibrium_f 1)) (* (nth in_static_equilibrium_r 1) (nth in_static_equilibrium_f 0)))) (set! in_static_equilibrium_sum_moments (+ in_static_equilibrium_sum_moments in_static_equilibrium_moment)) (set! in_static_equilibrium_i (+ in_static_equilibrium_i 1)))) (throw (ex-info "return" {:v (< (abs_float in_static_equilibrium_sum_moments) in_static_equilibrium_eps)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_forces1 [[1.0 1.0] [(- 1.0) 2.0]])

(def ^:dynamic main_location1 [[1.0 0.0] [10.0 0.0]])

(def ^:dynamic main_forces2 [(polar_force 718.4 150.0 false) (polar_force 879.54 45.0 false) (polar_force 100.0 (- 90.0) false)])

(def ^:dynamic main_location2 [[0.0 0.0] [0.0 0.0] [0.0 0.0]])

(def ^:dynamic main_forces3 [(polar_force (* 30.0 9.81) 15.0 false) (polar_force 215.0 135.0 false) (polar_force 264.0 60.0 false)])

(def ^:dynamic main_location3 [[0.0 0.0] [0.0 0.0] [0.0 0.0]])

(def ^:dynamic main_forces4 [[0.0 (- 2000.0)] [0.0 (- 1200.0)] [0.0 15600.0] [0.0 (- 12400.0)]])

(def ^:dynamic main_location4 [[0.0 0.0] [6.0 0.0] [10.0 0.0] [12.0 0.0]])

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (in_static_equilibrium main_forces1 main_location1 0.1)))
      (println (str (in_static_equilibrium main_forces2 main_location2 0.1)))
      (println (str (in_static_equilibrium main_forces3 main_location3 0.1)))
      (println (str (in_static_equilibrium main_forces4 main_location4 0.1)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
