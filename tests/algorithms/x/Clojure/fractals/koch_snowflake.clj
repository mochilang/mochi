(ns main (:refer-clojure :exclude [_mod sin cos rotate iteration_step iterate vec_to_string vec_list_to_string]))

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

(declare _mod sin cos rotate iteration_step iterate vec_to_string vec_list_to_string)

(def ^:dynamic cos_y nil)

(def ^:dynamic cos_y2 nil)

(def ^:dynamic cos_y4 nil)

(def ^:dynamic cos_y6 nil)

(def ^:dynamic iterate_i nil)

(def ^:dynamic iterate_vectors nil)

(def ^:dynamic iteration_step_dx nil)

(def ^:dynamic iteration_step_dy nil)

(def ^:dynamic iteration_step_end nil)

(def ^:dynamic iteration_step_i nil)

(def ^:dynamic iteration_step_mid nil)

(def ^:dynamic iteration_step_new_vectors nil)

(def ^:dynamic iteration_step_one_third nil)

(def ^:dynamic iteration_step_peak nil)

(def ^:dynamic iteration_step_start nil)

(def ^:dynamic iteration_step_two_third nil)

(def ^:dynamic rotate_c nil)

(def ^:dynamic rotate_s nil)

(def ^:dynamic rotate_theta nil)

(def ^:dynamic sin_y nil)

(def ^:dynamic sin_y2 nil)

(def ^:dynamic sin_y3 nil)

(def ^:dynamic sin_y5 nil)

(def ^:dynamic sin_y7 nil)

(def ^:dynamic vec_list_to_string_i nil)

(def ^:dynamic vec_list_to_string_res nil)

(def ^:dynamic main_PI 3.141592653589793)

(def ^:dynamic main_TWO_PI 6.283185307179586)

(defn _mod [_mod_x _mod_m]
  (try (throw (ex-info "return" {:v (- _mod_x (* (double (Integer/parseInt (quot _mod_x _mod_m))) _mod_m))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn sin [sin_x]
  (binding [sin_y nil sin_y2 nil sin_y3 nil sin_y5 nil sin_y7 nil] (try (do (set! sin_y (- (_mod (+ sin_x main_PI) main_TWO_PI) main_PI)) (set! sin_y2 (* sin_y sin_y)) (set! sin_y3 (* sin_y2 sin_y)) (set! sin_y5 (* sin_y3 sin_y2)) (set! sin_y7 (* sin_y5 sin_y2)) (throw (ex-info "return" {:v (- (+ (- sin_y (/ sin_y3 6.0)) (/ sin_y5 120.0)) (/ sin_y7 5040.0))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn cos [cos_x]
  (binding [cos_y nil cos_y2 nil cos_y4 nil cos_y6 nil] (try (do (set! cos_y (- (_mod (+ cos_x main_PI) main_TWO_PI) main_PI)) (set! cos_y2 (* cos_y cos_y)) (set! cos_y4 (* cos_y2 cos_y2)) (set! cos_y6 (* cos_y4 cos_y2)) (throw (ex-info "return" {:v (- (+ (- 1.0 (/ cos_y2 2.0)) (/ cos_y4 24.0)) (/ cos_y6 720.0))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn rotate [rotate_v rotate_angle_deg]
  (binding [rotate_c nil rotate_s nil rotate_theta nil] (try (do (set! rotate_theta (/ (* rotate_angle_deg main_PI) 180.0)) (set! rotate_c (cos rotate_theta)) (set! rotate_s (sin rotate_theta)) (throw (ex-info "return" {:v {:x (- (* (:x rotate_v) rotate_c) (* (:y rotate_v) rotate_s)) :y (+ (* (:x rotate_v) rotate_s) (* (:y rotate_v) rotate_c))}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn iteration_step [iteration_step_vectors]
  (binding [iteration_step_dx nil iteration_step_dy nil iteration_step_end nil iteration_step_i nil iteration_step_mid nil iteration_step_new_vectors nil iteration_step_one_third nil iteration_step_peak nil iteration_step_start nil iteration_step_two_third nil] (try (do (set! iteration_step_new_vectors []) (set! iteration_step_i 0) (while (< iteration_step_i (- (count iteration_step_vectors) 1)) (do (set! iteration_step_start (nth iteration_step_vectors iteration_step_i)) (set! iteration_step_end (nth iteration_step_vectors (+ iteration_step_i 1))) (set! iteration_step_new_vectors (conj iteration_step_new_vectors iteration_step_start)) (set! iteration_step_dx (- (:x iteration_step_end) (:x iteration_step_start))) (set! iteration_step_dy (- (:y iteration_step_end) (:y iteration_step_start))) (set! iteration_step_one_third {:x (+ (:x iteration_step_start) (/ iteration_step_dx 3.0)) :y (+ (:y iteration_step_start) (/ iteration_step_dy 3.0))}) (set! iteration_step_mid (rotate {:x (/ iteration_step_dx 3.0) :y (/ iteration_step_dy 3.0)} 60.0)) (set! iteration_step_peak {:x (+ (:x iteration_step_one_third) (:x iteration_step_mid)) :y (+ (:y iteration_step_one_third) (:y iteration_step_mid))}) (set! iteration_step_two_third {:x (+ (:x iteration_step_start) (/ (* iteration_step_dx 2.0) 3.0)) :y (+ (:y iteration_step_start) (/ (* iteration_step_dy 2.0) 3.0))}) (set! iteration_step_new_vectors (conj iteration_step_new_vectors iteration_step_one_third)) (set! iteration_step_new_vectors (conj iteration_step_new_vectors iteration_step_peak)) (set! iteration_step_new_vectors (conj iteration_step_new_vectors iteration_step_two_third)) (set! iteration_step_i (+ iteration_step_i 1)))) (set! iteration_step_new_vectors (conj iteration_step_new_vectors (nth iteration_step_vectors (- (count iteration_step_vectors) 1)))) (throw (ex-info "return" {:v iteration_step_new_vectors}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn iterate [iterate_initial iterate_steps]
  (binding [iterate_i nil iterate_vectors nil] (try (do (set! iterate_vectors iterate_initial) (set! iterate_i 0) (while (< iterate_i iterate_steps) (do (set! iterate_vectors (iteration_step iterate_vectors)) (set! iterate_i (+ iterate_i 1)))) (throw (ex-info "return" {:v iterate_vectors}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn vec_to_string [vec_to_string_v]
  (try (throw (ex-info "return" {:v (str (str (str (str "(" (str (:x vec_to_string_v))) ", ") (str (:y vec_to_string_v))) ")")})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn vec_list_to_string [vec_list_to_string_lst]
  (binding [vec_list_to_string_i nil vec_list_to_string_res nil] (try (do (set! vec_list_to_string_res "[") (set! vec_list_to_string_i 0) (while (< vec_list_to_string_i (count vec_list_to_string_lst)) (do (set! vec_list_to_string_res (str vec_list_to_string_res (vec_to_string (nth vec_list_to_string_lst vec_list_to_string_i)))) (when (< vec_list_to_string_i (- (count vec_list_to_string_lst) 1)) (set! vec_list_to_string_res (str vec_list_to_string_res ", "))) (set! vec_list_to_string_i (+ vec_list_to_string_i 1)))) (set! vec_list_to_string_res (str vec_list_to_string_res "]")) (throw (ex-info "return" {:v vec_list_to_string_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_VECTOR_1 {:x 0.0 :y 0.0})

(def ^:dynamic main_VECTOR_2 {:x 0.5 :y 0.8660254})

(def ^:dynamic main_VECTOR_3 {:x 1.0 :y 0.0})

(def ^:dynamic main_INITIAL_VECTORS [main_VECTOR_1 main_VECTOR_2 main_VECTOR_3 main_VECTOR_1])

(def ^:dynamic main_example (iterate [main_VECTOR_1 main_VECTOR_3] 1))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (vec_list_to_string main_example))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
