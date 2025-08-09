(ns main (:refer-clojure :exclude [floor modf sin_taylor cos_taylor matrix_to_string scaling rotation projection reflection]))

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

(declare floor modf sin_taylor cos_taylor matrix_to_string scaling rotation projection reflection)

(def ^:dynamic cos_taylor_i nil)

(def ^:dynamic cos_taylor_k1 nil)

(def ^:dynamic cos_taylor_k2 nil)

(def ^:dynamic cos_taylor_sum nil)

(def ^:dynamic cos_taylor_term nil)

(def ^:dynamic cos_taylor_x nil)

(def ^:dynamic floor_i nil)

(def ^:dynamic matrix_to_string_i nil)

(def ^:dynamic matrix_to_string_j nil)

(def ^:dynamic matrix_to_string_row nil)

(def ^:dynamic matrix_to_string_s nil)

(def ^:dynamic projection_c nil)

(def ^:dynamic projection_cs nil)

(def ^:dynamic projection_s nil)

(def ^:dynamic reflection_c nil)

(def ^:dynamic reflection_cs nil)

(def ^:dynamic reflection_s nil)

(def ^:dynamic rotation_c nil)

(def ^:dynamic rotation_s nil)

(def ^:dynamic sin_taylor_i nil)

(def ^:dynamic sin_taylor_k1 nil)

(def ^:dynamic sin_taylor_k2 nil)

(def ^:dynamic sin_taylor_sum nil)

(def ^:dynamic sin_taylor_term nil)

(def ^:dynamic sin_taylor_x nil)

(def ^:dynamic main_PI 3.141592653589793)

(defn floor [floor_x]
  (binding [floor_i nil] (try (do (set! floor_i (long floor_x)) (when (> (double floor_i) floor_x) (set! floor_i (- floor_i 1))) (throw (ex-info "return" {:v (double floor_i)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn modf [modf_x modf_m]
  (try (throw (ex-info "return" {:v (- modf_x (* (floor (quot modf_x modf_m)) modf_m))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn sin_taylor [sin_taylor_angle]
  (binding [sin_taylor_i nil sin_taylor_k1 nil sin_taylor_k2 nil sin_taylor_sum nil sin_taylor_term nil sin_taylor_x nil] (try (do (set! sin_taylor_x (modf sin_taylor_angle (* 2.0 main_PI))) (when (> sin_taylor_x main_PI) (set! sin_taylor_x (- sin_taylor_x (* 2.0 main_PI)))) (set! sin_taylor_term sin_taylor_x) (set! sin_taylor_sum sin_taylor_x) (set! sin_taylor_i 1) (while (< sin_taylor_i 10) (do (set! sin_taylor_k1 (* 2.0 (double sin_taylor_i))) (set! sin_taylor_k2 (+ sin_taylor_k1 1.0)) (set! sin_taylor_term (quot (* (* (- sin_taylor_term) sin_taylor_x) sin_taylor_x) (* sin_taylor_k1 sin_taylor_k2))) (set! sin_taylor_sum (+ sin_taylor_sum sin_taylor_term)) (set! sin_taylor_i (+ sin_taylor_i 1)))) (throw (ex-info "return" {:v sin_taylor_sum}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn cos_taylor [cos_taylor_angle]
  (binding [cos_taylor_i nil cos_taylor_k1 nil cos_taylor_k2 nil cos_taylor_sum nil cos_taylor_term nil cos_taylor_x nil] (try (do (set! cos_taylor_x (modf cos_taylor_angle (* 2.0 main_PI))) (when (> cos_taylor_x main_PI) (set! cos_taylor_x (- cos_taylor_x (* 2.0 main_PI)))) (set! cos_taylor_term 1.0) (set! cos_taylor_sum 1.0) (set! cos_taylor_i 1) (while (< cos_taylor_i 10) (do (set! cos_taylor_k1 (- (* 2.0 (double cos_taylor_i)) 1.0)) (set! cos_taylor_k2 (* 2.0 (double cos_taylor_i))) (set! cos_taylor_term (quot (* (* (- cos_taylor_term) cos_taylor_x) cos_taylor_x) (* cos_taylor_k1 cos_taylor_k2))) (set! cos_taylor_sum (+ cos_taylor_sum cos_taylor_term)) (set! cos_taylor_i (+ cos_taylor_i 1)))) (throw (ex-info "return" {:v cos_taylor_sum}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn matrix_to_string [matrix_to_string_m]
  (binding [matrix_to_string_i nil matrix_to_string_j nil matrix_to_string_row nil matrix_to_string_s nil] (try (do (set! matrix_to_string_s "[") (set! matrix_to_string_i 0) (while (< matrix_to_string_i (count matrix_to_string_m)) (do (set! matrix_to_string_row (nth matrix_to_string_m matrix_to_string_i)) (set! matrix_to_string_s (str matrix_to_string_s "[")) (set! matrix_to_string_j 0) (while (< matrix_to_string_j (count matrix_to_string_row)) (do (set! matrix_to_string_s (str matrix_to_string_s (str (nth matrix_to_string_row matrix_to_string_j)))) (when (< matrix_to_string_j (- (count matrix_to_string_row) 1)) (set! matrix_to_string_s (str matrix_to_string_s ", "))) (set! matrix_to_string_j (+ matrix_to_string_j 1)))) (set! matrix_to_string_s (str matrix_to_string_s "]")) (when (< matrix_to_string_i (- (count matrix_to_string_m) 1)) (set! matrix_to_string_s (str matrix_to_string_s ", "))) (set! matrix_to_string_i (+ matrix_to_string_i 1)))) (set! matrix_to_string_s (str matrix_to_string_s "]")) (throw (ex-info "return" {:v matrix_to_string_s}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn scaling [scaling_f]
  (try (throw (ex-info "return" {:v [[scaling_f 0.0] [0.0 scaling_f]]})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn rotation [rotation_angle]
  (binding [rotation_c nil rotation_s nil] (try (do (set! rotation_c (cos_taylor rotation_angle)) (set! rotation_s (sin_taylor rotation_angle)) (throw (ex-info "return" {:v [[rotation_c (- rotation_s)] [rotation_s rotation_c]]}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn projection [projection_angle]
  (binding [projection_c nil projection_cs nil projection_s nil] (try (do (set! projection_c (cos_taylor projection_angle)) (set! projection_s (sin_taylor projection_angle)) (set! projection_cs (* projection_c projection_s)) (throw (ex-info "return" {:v [[(* projection_c projection_c) projection_cs] [projection_cs (* projection_s projection_s)]]}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn reflection [reflection_angle]
  (binding [reflection_c nil reflection_cs nil reflection_s nil] (try (do (set! reflection_c (cos_taylor reflection_angle)) (set! reflection_s (sin_taylor reflection_angle)) (set! reflection_cs (* reflection_c reflection_s)) (throw (ex-info "return" {:v [[(- (* 2.0 reflection_c) 1.0) (* 2.0 reflection_cs)] [(* 2.0 reflection_cs) (- (* 2.0 reflection_s) 1.0)]]}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str "    scaling(5) = " (matrix_to_string (scaling 5.0))))
      (println (str "  rotation(45) = " (matrix_to_string (rotation 45.0))))
      (println (str "projection(45) = " (matrix_to_string (projection 45.0))))
      (println (str "reflection(45) = " (matrix_to_string (reflection 45.0))))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
