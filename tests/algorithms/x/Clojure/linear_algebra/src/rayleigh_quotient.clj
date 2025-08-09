(ns main (:refer-clojure :exclude [complex_conj complex_eq complex_add complex_mul conj_vector vec_mat_mul dot is_hermitian rayleigh_quotient]))

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

(declare complex_conj complex_eq complex_add complex_mul conj_vector vec_mat_mul dot is_hermitian rayleigh_quotient)

(def ^:dynamic complex_mul_imag nil)

(def ^:dynamic complex_mul_real nil)

(def ^:dynamic conj_vector_i nil)

(def ^:dynamic conj_vector_res nil)

(def ^:dynamic dot_i nil)

(def ^:dynamic dot_sum nil)

(def ^:dynamic is_hermitian_i nil)

(def ^:dynamic is_hermitian_j nil)

(def ^:dynamic rayleigh_quotient_den nil)

(def ^:dynamic rayleigh_quotient_num nil)

(def ^:dynamic rayleigh_quotient_v_star nil)

(def ^:dynamic rayleigh_quotient_v_star_dot nil)

(def ^:dynamic vec_mat_mul_col nil)

(def ^:dynamic vec_mat_mul_result nil)

(def ^:dynamic vec_mat_mul_row nil)

(def ^:dynamic vec_mat_mul_sum nil)

(defn complex_conj [complex_conj_z]
  (try (throw (ex-info "return" {:v {:im (- (:im complex_conj_z)) :re (:re complex_conj_z)}})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn complex_eq [complex_eq_a complex_eq_b]
  (try (throw (ex-info "return" {:v (and (= (:re complex_eq_a) (:re complex_eq_b)) (= (:im complex_eq_a) (:im complex_eq_b)))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn complex_add [complex_add_a complex_add_b]
  (try (throw (ex-info "return" {:v {:im (+ (:im complex_add_a) (:im complex_add_b)) :re (+ (:re complex_add_a) (:re complex_add_b))}})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn complex_mul [complex_mul_a complex_mul_b]
  (binding [complex_mul_imag nil complex_mul_real nil] (try (do (set! complex_mul_real (- (* (:re complex_mul_a) (:re complex_mul_b)) (* (:im complex_mul_a) (:im complex_mul_b)))) (set! complex_mul_imag (+ (* (:re complex_mul_a) (:im complex_mul_b)) (* (:im complex_mul_a) (:re complex_mul_b)))) (throw (ex-info "return" {:v {:im complex_mul_imag :re complex_mul_real}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn conj_vector [conj_vector_v]
  (binding [conj_vector_i nil conj_vector_res nil] (try (do (set! conj_vector_res []) (set! conj_vector_i 0) (while (< conj_vector_i (count conj_vector_v)) (do (set! conj_vector_res (conj conj_vector_res (complex_conj (nth conj_vector_v conj_vector_i)))) (set! conj_vector_i (+ conj_vector_i 1)))) (throw (ex-info "return" {:v conj_vector_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn vec_mat_mul [vec_mat_mul_v vec_mat_mul_m]
  (binding [vec_mat_mul_col nil vec_mat_mul_result nil vec_mat_mul_row nil vec_mat_mul_sum nil] (try (do (set! vec_mat_mul_result []) (set! vec_mat_mul_col 0) (while (< vec_mat_mul_col (count (nth vec_mat_mul_m 0))) (do (set! vec_mat_mul_sum {:im 0.0 :re 0.0}) (set! vec_mat_mul_row 0) (while (< vec_mat_mul_row (count vec_mat_mul_v)) (do (set! vec_mat_mul_sum (complex_add vec_mat_mul_sum (complex_mul (nth vec_mat_mul_v vec_mat_mul_row) (nth (nth vec_mat_mul_m vec_mat_mul_row) vec_mat_mul_col)))) (set! vec_mat_mul_row (+ vec_mat_mul_row 1)))) (set! vec_mat_mul_result (conj vec_mat_mul_result vec_mat_mul_sum)) (set! vec_mat_mul_col (+ vec_mat_mul_col 1)))) (throw (ex-info "return" {:v vec_mat_mul_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn dot [dot_a dot_b]
  (binding [dot_i nil dot_sum nil] (try (do (set! dot_sum {:im 0.0 :re 0.0}) (set! dot_i 0) (while (< dot_i (count dot_a)) (do (set! dot_sum (complex_add dot_sum (complex_mul (nth dot_a dot_i) (nth dot_b dot_i)))) (set! dot_i (+ dot_i 1)))) (throw (ex-info "return" {:v dot_sum}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn is_hermitian [is_hermitian_m]
  (binding [is_hermitian_i nil is_hermitian_j nil] (try (do (set! is_hermitian_i 0) (while (< is_hermitian_i (count is_hermitian_m)) (do (set! is_hermitian_j 0) (while (< is_hermitian_j (count is_hermitian_m)) (do (when (not (complex_eq (nth (nth is_hermitian_m is_hermitian_i) is_hermitian_j) (complex_conj (nth (nth is_hermitian_m is_hermitian_j) is_hermitian_i)))) (throw (ex-info "return" {:v false}))) (set! is_hermitian_j (+ is_hermitian_j 1)))) (set! is_hermitian_i (+ is_hermitian_i 1)))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn rayleigh_quotient [rayleigh_quotient_a rayleigh_quotient_v]
  (binding [rayleigh_quotient_den nil rayleigh_quotient_num nil rayleigh_quotient_v_star nil rayleigh_quotient_v_star_dot nil] (try (do (set! rayleigh_quotient_v_star (conj_vector rayleigh_quotient_v)) (set! rayleigh_quotient_v_star_dot (vec_mat_mul rayleigh_quotient_v_star rayleigh_quotient_a)) (set! rayleigh_quotient_num (dot rayleigh_quotient_v_star_dot rayleigh_quotient_v)) (set! rayleigh_quotient_den (dot rayleigh_quotient_v_star rayleigh_quotient_v)) (throw (ex-info "return" {:v (quot (:re rayleigh_quotient_num) (:re rayleigh_quotient_den))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_a [[{:im 0.0 :re 2.0} {:im 1.0 :re 2.0} {:im 0.0 :re 4.0}] [{:im (- 1.0) :re 2.0} {:im 0.0 :re 3.0} {:im 1.0 :re 0.0}] [{:im 0.0 :re 4.0} {:im (- 1.0) :re 0.0} {:im 0.0 :re 1.0}]])

(def ^:dynamic main_v [{:im 0.0 :re 1.0} {:im 0.0 :re 2.0} {:im 0.0 :re 3.0}])

(def ^:dynamic main_b [[{:im 0.0 :re 1.0} {:im 0.0 :re 2.0} {:im 0.0 :re 4.0}] [{:im 0.0 :re 2.0} {:im 0.0 :re 3.0} {:im 0.0 :re (- 1.0)}] [{:im 0.0 :re 4.0} {:im 0.0 :re (- 1.0)} {:im 0.0 :re 1.0}]])

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (when (is_hermitian main_a) (do (def ^:dynamic main_r1 (rayleigh_quotient main_a main_v)) (println main_r1) (println "\n")))
      (when (is_hermitian main_b) (do (def ^:dynamic main_r2 (rayleigh_quotient main_b main_v)) (println main_r2)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
