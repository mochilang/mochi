(ns main (:refer-clojure :exclude [abs_float validate_inputs list_to_string adams_bashforth_step2 adams_bashforth_step3 adams_bashforth_step4 adams_bashforth_step5 f_x f_xy]))

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

(declare abs_float validate_inputs list_to_string adams_bashforth_step2 adams_bashforth_step3 adams_bashforth_step4 adams_bashforth_step5 f_x f_xy)

(def ^:dynamic adams_bashforth_step2_i nil)

(def ^:dynamic adams_bashforth_step2_n nil)

(def ^:dynamic adams_bashforth_step2_term nil)

(def ^:dynamic adams_bashforth_step2_x0 nil)

(def ^:dynamic adams_bashforth_step2_x1 nil)

(def ^:dynamic adams_bashforth_step2_y nil)

(def ^:dynamic adams_bashforth_step2_y_next nil)

(def ^:dynamic adams_bashforth_step3_i nil)

(def ^:dynamic adams_bashforth_step3_n nil)

(def ^:dynamic adams_bashforth_step3_term nil)

(def ^:dynamic adams_bashforth_step3_x0 nil)

(def ^:dynamic adams_bashforth_step3_x1 nil)

(def ^:dynamic adams_bashforth_step3_x2 nil)

(def ^:dynamic adams_bashforth_step3_y nil)

(def ^:dynamic adams_bashforth_step3_y_next nil)

(def ^:dynamic adams_bashforth_step4_i nil)

(def ^:dynamic adams_bashforth_step4_n nil)

(def ^:dynamic adams_bashforth_step4_term nil)

(def ^:dynamic adams_bashforth_step4_x0 nil)

(def ^:dynamic adams_bashforth_step4_x1 nil)

(def ^:dynamic adams_bashforth_step4_x2 nil)

(def ^:dynamic adams_bashforth_step4_x3 nil)

(def ^:dynamic adams_bashforth_step4_y nil)

(def ^:dynamic adams_bashforth_step4_y_next nil)

(def ^:dynamic adams_bashforth_step5_i nil)

(def ^:dynamic adams_bashforth_step5_n nil)

(def ^:dynamic adams_bashforth_step5_term nil)

(def ^:dynamic adams_bashforth_step5_x0 nil)

(def ^:dynamic adams_bashforth_step5_x1 nil)

(def ^:dynamic adams_bashforth_step5_x2 nil)

(def ^:dynamic adams_bashforth_step5_x3 nil)

(def ^:dynamic adams_bashforth_step5_x4 nil)

(def ^:dynamic adams_bashforth_step5_y nil)

(def ^:dynamic adams_bashforth_step5_y_next nil)

(def ^:dynamic list_to_string_i nil)

(def ^:dynamic list_to_string_s nil)

(def ^:dynamic validate_inputs_diff nil)

(def ^:dynamic validate_inputs_i nil)

(defn abs_float [abs_float_x]
  (try (if (< abs_float_x 0.0) (throw (ex-info "return" {:v (- abs_float_x)})) (throw (ex-info "return" {:v abs_float_x}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn validate_inputs [validate_inputs_x_initials validate_inputs_step_size validate_inputs_x_final]
  (binding [validate_inputs_diff nil validate_inputs_i nil] (do (when (>= (nth validate_inputs_x_initials (- (count validate_inputs_x_initials) 1)) validate_inputs_x_final) (throw (Exception. "The final value of x must be greater than the initial values of x."))) (when (<= validate_inputs_step_size 0.0) (throw (Exception. "Step size must be positive."))) (set! validate_inputs_i 0) (while (< validate_inputs_i (- (count validate_inputs_x_initials) 1)) (do (set! validate_inputs_diff (- (nth validate_inputs_x_initials (+ validate_inputs_i 1)) (nth validate_inputs_x_initials validate_inputs_i))) (when (> (abs_float (- validate_inputs_diff validate_inputs_step_size)) 0.0000000001) (throw (Exception. "x-values must be equally spaced according to step size."))) (set! validate_inputs_i (+ validate_inputs_i 1)))) validate_inputs_x_initials)))

(defn list_to_string [list_to_string_xs]
  (binding [list_to_string_i nil list_to_string_s nil] (try (do (set! list_to_string_s "[") (set! list_to_string_i 0) (while (< list_to_string_i (count list_to_string_xs)) (do (set! list_to_string_s (str list_to_string_s (str (nth list_to_string_xs list_to_string_i)))) (when (< (+ list_to_string_i 1) (count list_to_string_xs)) (set! list_to_string_s (str list_to_string_s ", "))) (set! list_to_string_i (+ list_to_string_i 1)))) (set! list_to_string_s (str list_to_string_s "]")) (throw (ex-info "return" {:v list_to_string_s}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn adams_bashforth_step2 [adams_bashforth_step2_f adams_bashforth_step2_x_initials adams_bashforth_step2_y_initials adams_bashforth_step2_step_size adams_bashforth_step2_x_final]
  (binding [adams_bashforth_step2_i nil adams_bashforth_step2_n nil adams_bashforth_step2_term nil adams_bashforth_step2_x0 nil adams_bashforth_step2_x1 nil adams_bashforth_step2_y nil adams_bashforth_step2_y_next nil] (try (do (validate_inputs adams_bashforth_step2_x_initials adams_bashforth_step2_step_size adams_bashforth_step2_x_final) (when (or (not= (count adams_bashforth_step2_x_initials) 2) (not= (count adams_bashforth_step2_y_initials) 2)) (throw (Exception. "Insufficient initial points information."))) (set! adams_bashforth_step2_x0 (nth adams_bashforth_step2_x_initials 0)) (set! adams_bashforth_step2_x1 (nth adams_bashforth_step2_x_initials 1)) (set! adams_bashforth_step2_y []) (set! adams_bashforth_step2_y (conj adams_bashforth_step2_y (nth adams_bashforth_step2_y_initials 0))) (set! adams_bashforth_step2_y (conj adams_bashforth_step2_y (nth adams_bashforth_step2_y_initials 1))) (set! adams_bashforth_step2_n (long (quot (- adams_bashforth_step2_x_final adams_bashforth_step2_x1) adams_bashforth_step2_step_size))) (set! adams_bashforth_step2_i 0) (while (< adams_bashforth_step2_i adams_bashforth_step2_n) (do (set! adams_bashforth_step2_term (- (* 3.0 (adams_bashforth_step2_f adams_bashforth_step2_x1 (nth adams_bashforth_step2_y (+ adams_bashforth_step2_i 1)))) (adams_bashforth_step2_f adams_bashforth_step2_x0 (nth adams_bashforth_step2_y adams_bashforth_step2_i)))) (set! adams_bashforth_step2_y_next (+ (nth adams_bashforth_step2_y (+ adams_bashforth_step2_i 1)) (* (/ adams_bashforth_step2_step_size 2.0) adams_bashforth_step2_term))) (set! adams_bashforth_step2_y (conj adams_bashforth_step2_y adams_bashforth_step2_y_next)) (set! adams_bashforth_step2_x0 adams_bashforth_step2_x1) (set! adams_bashforth_step2_x1 (+ adams_bashforth_step2_x1 adams_bashforth_step2_step_size)) (set! adams_bashforth_step2_i (+ adams_bashforth_step2_i 1)))) (throw (ex-info "return" {:v adams_bashforth_step2_y}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn adams_bashforth_step3 [adams_bashforth_step3_f adams_bashforth_step3_x_initials adams_bashforth_step3_y_initials adams_bashforth_step3_step_size adams_bashforth_step3_x_final]
  (binding [adams_bashforth_step3_i nil adams_bashforth_step3_n nil adams_bashforth_step3_term nil adams_bashforth_step3_x0 nil adams_bashforth_step3_x1 nil adams_bashforth_step3_x2 nil adams_bashforth_step3_y nil adams_bashforth_step3_y_next nil] (try (do (validate_inputs adams_bashforth_step3_x_initials adams_bashforth_step3_step_size adams_bashforth_step3_x_final) (when (or (not= (count adams_bashforth_step3_x_initials) 3) (not= (count adams_bashforth_step3_y_initials) 3)) (throw (Exception. "Insufficient initial points information."))) (set! adams_bashforth_step3_x0 (nth adams_bashforth_step3_x_initials 0)) (set! adams_bashforth_step3_x1 (nth adams_bashforth_step3_x_initials 1)) (set! adams_bashforth_step3_x2 (nth adams_bashforth_step3_x_initials 2)) (set! adams_bashforth_step3_y []) (set! adams_bashforth_step3_y (conj adams_bashforth_step3_y (nth adams_bashforth_step3_y_initials 0))) (set! adams_bashforth_step3_y (conj adams_bashforth_step3_y (nth adams_bashforth_step3_y_initials 1))) (set! adams_bashforth_step3_y (conj adams_bashforth_step3_y (nth adams_bashforth_step3_y_initials 2))) (set! adams_bashforth_step3_n (long (quot (- adams_bashforth_step3_x_final adams_bashforth_step3_x2) adams_bashforth_step3_step_size))) (set! adams_bashforth_step3_i 0) (while (<= adams_bashforth_step3_i adams_bashforth_step3_n) (do (set! adams_bashforth_step3_term (+ (- (* 23.0 (adams_bashforth_step3_f adams_bashforth_step3_x2 (nth adams_bashforth_step3_y (+ adams_bashforth_step3_i 2)))) (* 16.0 (adams_bashforth_step3_f adams_bashforth_step3_x1 (nth adams_bashforth_step3_y (+ adams_bashforth_step3_i 1))))) (* 5.0 (adams_bashforth_step3_f adams_bashforth_step3_x0 (nth adams_bashforth_step3_y adams_bashforth_step3_i))))) (set! adams_bashforth_step3_y_next (+ (nth adams_bashforth_step3_y (+ adams_bashforth_step3_i 2)) (* (/ adams_bashforth_step3_step_size 12.0) adams_bashforth_step3_term))) (set! adams_bashforth_step3_y (conj adams_bashforth_step3_y adams_bashforth_step3_y_next)) (set! adams_bashforth_step3_x0 adams_bashforth_step3_x1) (set! adams_bashforth_step3_x1 adams_bashforth_step3_x2) (set! adams_bashforth_step3_x2 (+ adams_bashforth_step3_x2 adams_bashforth_step3_step_size)) (set! adams_bashforth_step3_i (+ adams_bashforth_step3_i 1)))) (throw (ex-info "return" {:v adams_bashforth_step3_y}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn adams_bashforth_step4 [adams_bashforth_step4_f adams_bashforth_step4_x_initials adams_bashforth_step4_y_initials adams_bashforth_step4_step_size adams_bashforth_step4_x_final]
  (binding [adams_bashforth_step4_i nil adams_bashforth_step4_n nil adams_bashforth_step4_term nil adams_bashforth_step4_x0 nil adams_bashforth_step4_x1 nil adams_bashforth_step4_x2 nil adams_bashforth_step4_x3 nil adams_bashforth_step4_y nil adams_bashforth_step4_y_next nil] (try (do (validate_inputs adams_bashforth_step4_x_initials adams_bashforth_step4_step_size adams_bashforth_step4_x_final) (when (or (not= (count adams_bashforth_step4_x_initials) 4) (not= (count adams_bashforth_step4_y_initials) 4)) (throw (Exception. "Insufficient initial points information."))) (set! adams_bashforth_step4_x0 (nth adams_bashforth_step4_x_initials 0)) (set! adams_bashforth_step4_x1 (nth adams_bashforth_step4_x_initials 1)) (set! adams_bashforth_step4_x2 (nth adams_bashforth_step4_x_initials 2)) (set! adams_bashforth_step4_x3 (nth adams_bashforth_step4_x_initials 3)) (set! adams_bashforth_step4_y []) (set! adams_bashforth_step4_y (conj adams_bashforth_step4_y (nth adams_bashforth_step4_y_initials 0))) (set! adams_bashforth_step4_y (conj adams_bashforth_step4_y (nth adams_bashforth_step4_y_initials 1))) (set! adams_bashforth_step4_y (conj adams_bashforth_step4_y (nth adams_bashforth_step4_y_initials 2))) (set! adams_bashforth_step4_y (conj adams_bashforth_step4_y (nth adams_bashforth_step4_y_initials 3))) (set! adams_bashforth_step4_n (long (quot (- adams_bashforth_step4_x_final adams_bashforth_step4_x3) adams_bashforth_step4_step_size))) (set! adams_bashforth_step4_i 0) (while (< adams_bashforth_step4_i adams_bashforth_step4_n) (do (set! adams_bashforth_step4_term (- (+ (- (* 55.0 (adams_bashforth_step4_f adams_bashforth_step4_x3 (nth adams_bashforth_step4_y (+ adams_bashforth_step4_i 3)))) (* 59.0 (adams_bashforth_step4_f adams_bashforth_step4_x2 (nth adams_bashforth_step4_y (+ adams_bashforth_step4_i 2))))) (* 37.0 (adams_bashforth_step4_f adams_bashforth_step4_x1 (nth adams_bashforth_step4_y (+ adams_bashforth_step4_i 1))))) (* 9.0 (adams_bashforth_step4_f adams_bashforth_step4_x0 (nth adams_bashforth_step4_y adams_bashforth_step4_i))))) (set! adams_bashforth_step4_y_next (+ (nth adams_bashforth_step4_y (+ adams_bashforth_step4_i 3)) (* (/ adams_bashforth_step4_step_size 24.0) adams_bashforth_step4_term))) (set! adams_bashforth_step4_y (conj adams_bashforth_step4_y adams_bashforth_step4_y_next)) (set! adams_bashforth_step4_x0 adams_bashforth_step4_x1) (set! adams_bashforth_step4_x1 adams_bashforth_step4_x2) (set! adams_bashforth_step4_x2 adams_bashforth_step4_x3) (set! adams_bashforth_step4_x3 (+ adams_bashforth_step4_x3 adams_bashforth_step4_step_size)) (set! adams_bashforth_step4_i (+ adams_bashforth_step4_i 1)))) (throw (ex-info "return" {:v adams_bashforth_step4_y}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn adams_bashforth_step5 [adams_bashforth_step5_f adams_bashforth_step5_x_initials adams_bashforth_step5_y_initials adams_bashforth_step5_step_size adams_bashforth_step5_x_final]
  (binding [adams_bashforth_step5_i nil adams_bashforth_step5_n nil adams_bashforth_step5_term nil adams_bashforth_step5_x0 nil adams_bashforth_step5_x1 nil adams_bashforth_step5_x2 nil adams_bashforth_step5_x3 nil adams_bashforth_step5_x4 nil adams_bashforth_step5_y nil adams_bashforth_step5_y_next nil] (try (do (validate_inputs adams_bashforth_step5_x_initials adams_bashforth_step5_step_size adams_bashforth_step5_x_final) (when (or (not= (count adams_bashforth_step5_x_initials) 5) (not= (count adams_bashforth_step5_y_initials) 5)) (throw (Exception. "Insufficient initial points information."))) (set! adams_bashforth_step5_x0 (nth adams_bashforth_step5_x_initials 0)) (set! adams_bashforth_step5_x1 (nth adams_bashforth_step5_x_initials 1)) (set! adams_bashforth_step5_x2 (nth adams_bashforth_step5_x_initials 2)) (set! adams_bashforth_step5_x3 (nth adams_bashforth_step5_x_initials 3)) (set! adams_bashforth_step5_x4 (nth adams_bashforth_step5_x_initials 4)) (set! adams_bashforth_step5_y []) (set! adams_bashforth_step5_y (conj adams_bashforth_step5_y (nth adams_bashforth_step5_y_initials 0))) (set! adams_bashforth_step5_y (conj adams_bashforth_step5_y (nth adams_bashforth_step5_y_initials 1))) (set! adams_bashforth_step5_y (conj adams_bashforth_step5_y (nth adams_bashforth_step5_y_initials 2))) (set! adams_bashforth_step5_y (conj adams_bashforth_step5_y (nth adams_bashforth_step5_y_initials 3))) (set! adams_bashforth_step5_y (conj adams_bashforth_step5_y (nth adams_bashforth_step5_y_initials 4))) (set! adams_bashforth_step5_n (long (quot (- adams_bashforth_step5_x_final adams_bashforth_step5_x4) adams_bashforth_step5_step_size))) (set! adams_bashforth_step5_i 0) (while (<= adams_bashforth_step5_i adams_bashforth_step5_n) (do (set! adams_bashforth_step5_term (+ (- (- (- (* 1901.0 (adams_bashforth_step5_f adams_bashforth_step5_x4 (nth adams_bashforth_step5_y (+ adams_bashforth_step5_i 4)))) (* 2774.0 (adams_bashforth_step5_f adams_bashforth_step5_x3 (nth adams_bashforth_step5_y (+ adams_bashforth_step5_i 3))))) (* 2616.0 (adams_bashforth_step5_f adams_bashforth_step5_x2 (nth adams_bashforth_step5_y (+ adams_bashforth_step5_i 2))))) (* 1274.0 (adams_bashforth_step5_f adams_bashforth_step5_x1 (nth adams_bashforth_step5_y (+ adams_bashforth_step5_i 1))))) (* 251.0 (adams_bashforth_step5_f adams_bashforth_step5_x0 (nth adams_bashforth_step5_y adams_bashforth_step5_i))))) (set! adams_bashforth_step5_y_next (+ (nth adams_bashforth_step5_y (+ adams_bashforth_step5_i 4)) (* (/ adams_bashforth_step5_step_size 720.0) adams_bashforth_step5_term))) (set! adams_bashforth_step5_y (conj adams_bashforth_step5_y adams_bashforth_step5_y_next)) (set! adams_bashforth_step5_x0 adams_bashforth_step5_x1) (set! adams_bashforth_step5_x1 adams_bashforth_step5_x2) (set! adams_bashforth_step5_x2 adams_bashforth_step5_x3) (set! adams_bashforth_step5_x3 adams_bashforth_step5_x4) (set! adams_bashforth_step5_x4 (+ adams_bashforth_step5_x4 adams_bashforth_step5_step_size)) (set! adams_bashforth_step5_i (+ adams_bashforth_step5_i 1)))) (throw (ex-info "return" {:v adams_bashforth_step5_y}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn f_x [f_x_x f_x_y]
  (try (throw (ex-info "return" {:v f_x_x})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn f_xy [f_xy_x f_xy_y]
  (try (throw (ex-info "return" {:v (+ f_xy_x f_xy_y)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(def ^:dynamic main_y2 (adams_bashforth_step2 f_x [0.0 0.2] [0.0 0.0] 0.2 1.0))

(def ^:dynamic main_y3 (adams_bashforth_step3 f_xy [0.0 0.2 0.4] [0.0 0.0 0.04] 0.2 1.0))

(def ^:dynamic main_y4 (adams_bashforth_step4 f_xy [0.0 0.2 0.4 0.6] [0.0 0.0 0.04 0.128] 0.2 1.0))

(def ^:dynamic main_y5 (adams_bashforth_step5 f_xy [0.0 0.2 0.4 0.6 0.8] [0.0 0.0214 0.0214 0.22211 0.42536] 0.2 1.0))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (list_to_string main_y2))
      (println (str (nth main_y3 3)))
      (println (str (nth main_y4 4)))
      (println (str (nth main_y4 5)))
      (println (str (nth main_y5 (- (count main_y5) 1))))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
