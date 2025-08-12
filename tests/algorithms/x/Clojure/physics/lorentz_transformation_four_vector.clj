(ns main (:refer-clojure :exclude [sqrtApprox beta gamma transformation_matrix mat_vec_mul transform]))

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

(declare sqrtApprox beta gamma transformation_matrix mat_vec_mul transform)

(def ^:dynamic gamma_b nil)

(def ^:dynamic mat_vec_mul_i nil)

(def ^:dynamic mat_vec_mul_res nil)

(def ^:dynamic mat_vec_mul_row nil)

(def ^:dynamic mat_vec_mul_value nil)

(def ^:dynamic sqrtApprox_guess nil)

(def ^:dynamic sqrtApprox_i nil)

(def ^:dynamic transform_b nil)

(def ^:dynamic transform_ct nil)

(def ^:dynamic transform_g nil)

(def ^:dynamic transform_x nil)

(def ^:dynamic transformation_matrix_b nil)

(def ^:dynamic transformation_matrix_g nil)

(def ^:dynamic main_c 299792458.0)

(defn sqrtApprox [sqrtApprox_x]
  (binding [sqrtApprox_guess nil sqrtApprox_i nil] (try (do (when (<= sqrtApprox_x 0.0) (throw (ex-info "return" {:v 0.0}))) (set! sqrtApprox_guess (/ sqrtApprox_x 2.0)) (set! sqrtApprox_i 0) (while (< sqrtApprox_i 20) (do (set! sqrtApprox_guess (/ (+ sqrtApprox_guess (/ sqrtApprox_x sqrtApprox_guess)) 2.0)) (set! sqrtApprox_i (+ sqrtApprox_i 1)))) (throw (ex-info "return" {:v sqrtApprox_guess}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn beta [beta_velocity]
  (try (do (when (> beta_velocity main_c) (throw (Exception. "Speed must not exceed light speed 299,792,458 [m/s]!"))) (when (< beta_velocity 1.0) (throw (Exception. "Speed must be greater than or equal to 1!"))) (throw (ex-info "return" {:v (/ beta_velocity main_c)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn gamma [gamma_velocity]
  (binding [gamma_b nil] (try (do (set! gamma_b (beta gamma_velocity)) (throw (ex-info "return" {:v (/ 1.0 (sqrtApprox (- 1.0 (* gamma_b gamma_b))))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn transformation_matrix [transformation_matrix_velocity]
  (binding [transformation_matrix_b nil transformation_matrix_g nil] (try (do (set! transformation_matrix_g (gamma transformation_matrix_velocity)) (set! transformation_matrix_b (beta transformation_matrix_velocity)) (throw (ex-info "return" {:v [[transformation_matrix_g (* (- transformation_matrix_g) transformation_matrix_b) 0.0 0.0] [(* (- transformation_matrix_g) transformation_matrix_b) transformation_matrix_g 0.0 0.0] [0.0 0.0 1.0 0.0] [0.0 0.0 0.0 1.0]]}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn mat_vec_mul [mat_vec_mul_mat mat_vec_mul_vec]
  (binding [mat_vec_mul_i nil mat_vec_mul_res nil mat_vec_mul_row nil mat_vec_mul_value nil] (try (do (set! mat_vec_mul_res []) (set! mat_vec_mul_i 0) (while (< mat_vec_mul_i 4) (do (set! mat_vec_mul_row (nth mat_vec_mul_mat mat_vec_mul_i)) (set! mat_vec_mul_value (+ (+ (+ (* (nth mat_vec_mul_row 0) (nth mat_vec_mul_vec 0)) (* (nth mat_vec_mul_row 1) (nth mat_vec_mul_vec 1))) (* (nth mat_vec_mul_row 2) (nth mat_vec_mul_vec 2))) (* (nth mat_vec_mul_row 3) (nth mat_vec_mul_vec 3)))) (set! mat_vec_mul_res (vec (concat mat_vec_mul_res [mat_vec_mul_value]))) (set! mat_vec_mul_i (+ mat_vec_mul_i 1)))) (throw (ex-info "return" {:v mat_vec_mul_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn transform [transform_velocity transform_event]
  (binding [transform_b nil transform_ct nil transform_g nil transform_x nil] (try (do (set! transform_g (gamma transform_velocity)) (set! transform_b (beta transform_velocity)) (set! transform_ct (* (nth transform_event 0) main_c)) (set! transform_x (nth transform_event 1)) (throw (ex-info "return" {:v [(- (* transform_g transform_ct) (* (* transform_g transform_b) transform_x)) (+ (* (* (- transform_g) transform_b) transform_ct) (* transform_g transform_x)) (nth transform_event 2) (nth transform_event 3)]}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_v (transform 29979245.0 [1.0 2.0 3.0 4.0]))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (beta main_c)))
      (println (str (beta 199792458.0)))
      (println (str (beta 100000.0)))
      (println (str (gamma 4.0)))
      (println (str (gamma 100000.0)))
      (println (str (gamma 30000000.0)))
      (println (str (transformation_matrix 29979245.0)))
      (println (str main_v))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
