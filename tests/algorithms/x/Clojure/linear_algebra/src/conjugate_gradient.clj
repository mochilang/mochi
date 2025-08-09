(ns main (:refer-clojure :exclude [zeros dot mat_vec_mul vec_add vec_sub scalar_mul sqrtApprox norm conjugate_gradient]))

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

(declare zeros dot mat_vec_mul vec_add vec_sub scalar_mul sqrtApprox norm conjugate_gradient)

(def ^:dynamic conjugate_gradient_Ap nil)

(def ^:dynamic conjugate_gradient_alpha nil)

(def ^:dynamic conjugate_gradient_beta nil)

(def ^:dynamic conjugate_gradient_i nil)

(def ^:dynamic conjugate_gradient_n nil)

(def ^:dynamic conjugate_gradient_p nil)

(def ^:dynamic conjugate_gradient_r nil)

(def ^:dynamic conjugate_gradient_rs_new nil)

(def ^:dynamic conjugate_gradient_rs_old nil)

(def ^:dynamic conjugate_gradient_x nil)

(def ^:dynamic dot_i nil)

(def ^:dynamic dot_sum nil)

(def ^:dynamic mat_vec_mul_i nil)

(def ^:dynamic mat_vec_mul_j nil)

(def ^:dynamic mat_vec_mul_res nil)

(def ^:dynamic mat_vec_mul_s nil)

(def ^:dynamic scalar_mul_i nil)

(def ^:dynamic scalar_mul_res nil)

(def ^:dynamic sqrtApprox_guess nil)

(def ^:dynamic sqrtApprox_i nil)

(def ^:dynamic vec_add_i nil)

(def ^:dynamic vec_add_res nil)

(def ^:dynamic vec_sub_i nil)

(def ^:dynamic vec_sub_res nil)

(def ^:dynamic zeros_i nil)

(def ^:dynamic zeros_res nil)

(defn zeros [zeros_n]
  (binding [zeros_i nil zeros_res nil] (try (do (set! zeros_res []) (set! zeros_i 0) (while (< zeros_i zeros_n) (do (set! zeros_res (conj zeros_res 0.0)) (set! zeros_i (+ zeros_i 1)))) (throw (ex-info "return" {:v zeros_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn dot [dot_a dot_b]
  (binding [dot_i nil dot_sum nil] (try (do (set! dot_sum 0.0) (set! dot_i 0) (while (< dot_i (count dot_a)) (do (set! dot_sum (+ dot_sum (* (nth dot_a dot_i) (nth dot_b dot_i)))) (set! dot_i (+ dot_i 1)))) (throw (ex-info "return" {:v dot_sum}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn mat_vec_mul [mat_vec_mul_m mat_vec_mul_v]
  (binding [mat_vec_mul_i nil mat_vec_mul_j nil mat_vec_mul_res nil mat_vec_mul_s nil] (try (do (set! mat_vec_mul_res []) (set! mat_vec_mul_i 0) (while (< mat_vec_mul_i (count mat_vec_mul_m)) (do (set! mat_vec_mul_s 0.0) (set! mat_vec_mul_j 0) (while (< mat_vec_mul_j (count (nth mat_vec_mul_m mat_vec_mul_i))) (do (set! mat_vec_mul_s (+ mat_vec_mul_s (* (nth (nth mat_vec_mul_m mat_vec_mul_i) mat_vec_mul_j) (nth mat_vec_mul_v mat_vec_mul_j)))) (set! mat_vec_mul_j (+ mat_vec_mul_j 1)))) (set! mat_vec_mul_res (conj mat_vec_mul_res mat_vec_mul_s)) (set! mat_vec_mul_i (+ mat_vec_mul_i 1)))) (throw (ex-info "return" {:v mat_vec_mul_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn vec_add [vec_add_a vec_add_b]
  (binding [vec_add_i nil vec_add_res nil] (try (do (set! vec_add_res []) (set! vec_add_i 0) (while (< vec_add_i (count vec_add_a)) (do (set! vec_add_res (conj vec_add_res (+ (nth vec_add_a vec_add_i) (nth vec_add_b vec_add_i)))) (set! vec_add_i (+ vec_add_i 1)))) (throw (ex-info "return" {:v vec_add_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn vec_sub [vec_sub_a vec_sub_b]
  (binding [vec_sub_i nil vec_sub_res nil] (try (do (set! vec_sub_res []) (set! vec_sub_i 0) (while (< vec_sub_i (count vec_sub_a)) (do (set! vec_sub_res (conj vec_sub_res (- (nth vec_sub_a vec_sub_i) (nth vec_sub_b vec_sub_i)))) (set! vec_sub_i (+ vec_sub_i 1)))) (throw (ex-info "return" {:v vec_sub_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn scalar_mul [scalar_mul_s scalar_mul_v]
  (binding [scalar_mul_i nil scalar_mul_res nil] (try (do (set! scalar_mul_res []) (set! scalar_mul_i 0) (while (< scalar_mul_i (count scalar_mul_v)) (do (set! scalar_mul_res (conj scalar_mul_res (* scalar_mul_s (nth scalar_mul_v scalar_mul_i)))) (set! scalar_mul_i (+ scalar_mul_i 1)))) (throw (ex-info "return" {:v scalar_mul_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn sqrtApprox [sqrtApprox_x]
  (binding [sqrtApprox_guess nil sqrtApprox_i nil] (try (do (when (<= sqrtApprox_x 0.0) (throw (ex-info "return" {:v 0.0}))) (set! sqrtApprox_guess sqrtApprox_x) (set! sqrtApprox_i 0) (while (< sqrtApprox_i 20) (do (set! sqrtApprox_guess (/ (+ sqrtApprox_guess (quot sqrtApprox_x sqrtApprox_guess)) 2.0)) (set! sqrtApprox_i (+ sqrtApprox_i 1)))) (throw (ex-info "return" {:v sqrtApprox_guess}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn norm [norm_v]
  (try (throw (ex-info "return" {:v (sqrtApprox (dot norm_v norm_v))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn conjugate_gradient [conjugate_gradient_A conjugate_gradient_b conjugate_gradient_max_iterations conjugate_gradient_tol]
  (binding [conjugate_gradient_Ap nil conjugate_gradient_alpha nil conjugate_gradient_beta nil conjugate_gradient_i nil conjugate_gradient_n nil conjugate_gradient_p nil conjugate_gradient_r nil conjugate_gradient_rs_new nil conjugate_gradient_rs_old nil conjugate_gradient_x nil] (try (do (set! conjugate_gradient_n (count conjugate_gradient_b)) (set! conjugate_gradient_x (zeros conjugate_gradient_n)) (set! conjugate_gradient_r (vec_sub conjugate_gradient_b (mat_vec_mul conjugate_gradient_A conjugate_gradient_x))) (set! conjugate_gradient_p conjugate_gradient_r) (set! conjugate_gradient_rs_old (dot conjugate_gradient_r conjugate_gradient_r)) (set! conjugate_gradient_i 0) (loop [while_flag_1 true] (when (and while_flag_1 (< conjugate_gradient_i conjugate_gradient_max_iterations)) (do (set! conjugate_gradient_Ap (mat_vec_mul conjugate_gradient_A conjugate_gradient_p)) (set! conjugate_gradient_alpha (quot conjugate_gradient_rs_old (dot conjugate_gradient_p conjugate_gradient_Ap))) (set! conjugate_gradient_x (vec_add conjugate_gradient_x (scalar_mul conjugate_gradient_alpha conjugate_gradient_p))) (set! conjugate_gradient_r (vec_sub conjugate_gradient_r (scalar_mul conjugate_gradient_alpha conjugate_gradient_Ap))) (set! conjugate_gradient_rs_new (dot conjugate_gradient_r conjugate_gradient_r)) (cond (< (sqrtApprox conjugate_gradient_rs_new) conjugate_gradient_tol) (recur false) :else (do (set! conjugate_gradient_beta (quot conjugate_gradient_rs_new conjugate_gradient_rs_old)) (set! conjugate_gradient_p (vec_add conjugate_gradient_r (scalar_mul conjugate_gradient_beta conjugate_gradient_p))) (set! conjugate_gradient_rs_old conjugate_gradient_rs_new) (set! conjugate_gradient_i (+ conjugate_gradient_i 1)) (recur while_flag_1)))))) (throw (ex-info "return" {:v conjugate_gradient_x}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_A [[8.73256573 (- 5.02034289) (- 2.68709226)] [(- 5.02034289) 3.78188322 0.91980451] [(- 2.68709226) 0.91980451 1.94746467]])

(def ^:dynamic main_b [(- 5.80872761) 3.23807431 1.95381422])

(def ^:dynamic main_x (conjugate_gradient main_A main_b 1000 0.00000001))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (nth main_x 0)))
      (println (str (nth main_x 1)))
      (println (str (nth main_x 2)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
