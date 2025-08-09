(ns main (:refer-clojure :exclude [copy_list polynomial_new add neg sub mul power evaluate poly_to_string derivative integral equals not_equals test_polynomial main]))

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

(declare copy_list polynomial_new add neg sub mul power evaluate poly_to_string derivative integral equals not_equals test_polynomial main)

(def ^:dynamic add_coeffs nil)

(def ^:dynamic add_i nil)

(def ^:dynamic copy_list_i nil)

(def ^:dynamic copy_list_res nil)

(def ^:dynamic derivative_coeffs nil)

(def ^:dynamic derivative_i nil)

(def ^:dynamic equals_i nil)

(def ^:dynamic evaluate_i nil)

(def ^:dynamic evaluate_result nil)

(def ^:dynamic integral_coeffs nil)

(def ^:dynamic integral_i nil)

(def ^:dynamic main_d nil)

(def ^:dynamic main_p nil)

(def ^:dynamic mul_coeffs nil)

(def ^:dynamic mul_i nil)

(def ^:dynamic mul_j nil)

(def ^:dynamic mul_size nil)

(def ^:dynamic neg_coeffs nil)

(def ^:dynamic neg_i nil)

(def ^:dynamic poly_to_string_abs_coeff nil)

(def ^:dynamic poly_to_string_coeff nil)

(def ^:dynamic poly_to_string_i nil)

(def ^:dynamic poly_to_string_s nil)

(def ^:dynamic power_i nil)

(def ^:dynamic power_result nil)

(def ^:dynamic test_polynomial_integ nil)

(def ^:dynamic test_polynomial_p nil)

(def ^:dynamic test_polynomial_q nil)

(defn copy_list [copy_list_xs]
  (binding [copy_list_i nil copy_list_res nil] (try (do (set! copy_list_res []) (set! copy_list_i 0) (while (< copy_list_i (count copy_list_xs)) (do (set! copy_list_res (conj copy_list_res (nth copy_list_xs copy_list_i))) (set! copy_list_i (+ copy_list_i 1)))) (throw (ex-info "return" {:v copy_list_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn polynomial_new [polynomial_new_degree polynomial_new_coeffs]
  (try (do (when (not= (count polynomial_new_coeffs) (+ polynomial_new_degree 1)) (throw (Exception. "The number of coefficients should be equal to the degree + 1."))) (throw (ex-info "return" {:v {:coefficients (copy_list polynomial_new_coeffs) :degree polynomial_new_degree}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn add [add_p add_q]
  (binding [add_coeffs nil add_i nil] (try (if (> (:degree add_p) (:degree add_q)) (do (set! add_coeffs (copy_list (:coefficients add_p))) (set! add_i 0) (while (<= add_i (:degree add_q)) (do (set! add_coeffs (assoc add_coeffs add_i (+ (nth add_coeffs add_i) (get (:coefficients add_q) add_i)))) (set! add_i (+ add_i 1)))) (throw (ex-info "return" {:v {:coefficients add_coeffs :degree (:degree add_p)}}))) (do (set! add_coeffs (copy_list (:coefficients add_q))) (set! add_i 0) (while (<= add_i (:degree add_p)) (do (set! add_coeffs (assoc add_coeffs add_i (+ (nth add_coeffs add_i) (get (:coefficients add_p) add_i)))) (set! add_i (+ add_i 1)))) (throw (ex-info "return" {:v {:coefficients add_coeffs :degree (:degree add_q)}})))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn neg [neg_p]
  (binding [neg_coeffs nil neg_i nil] (try (do (set! neg_coeffs []) (set! neg_i 0) (while (<= neg_i (:degree neg_p)) (do (set! neg_coeffs (conj neg_coeffs (- (get (:coefficients neg_p) neg_i)))) (set! neg_i (+ neg_i 1)))) (throw (ex-info "return" {:v {:coefficients neg_coeffs :degree (:degree neg_p)}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn sub [sub_p sub_q]
  (try (throw (ex-info "return" {:v (add sub_p (neg sub_q))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn mul [mul_p mul_q]
  (binding [mul_coeffs nil mul_i nil mul_j nil mul_size nil] (try (do (set! mul_size (+ (+ (:degree mul_p) (:degree mul_q)) 1)) (set! mul_coeffs []) (set! mul_i 0) (while (< mul_i mul_size) (do (set! mul_coeffs (conj mul_coeffs 0.0)) (set! mul_i (+ mul_i 1)))) (set! mul_i 0) (while (<= mul_i (:degree mul_p)) (do (set! mul_j 0) (while (<= mul_j (:degree mul_q)) (do (set! mul_coeffs (assoc mul_coeffs (+ mul_i mul_j) (+ (nth mul_coeffs (+ mul_i mul_j)) (* (get (:coefficients mul_p) mul_i) (get (:coefficients mul_q) mul_j))))) (set! mul_j (+ mul_j 1)))) (set! mul_i (+ mul_i 1)))) (throw (ex-info "return" {:v {:coefficients mul_coeffs :degree (+ (:degree mul_p) (:degree mul_q))}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn power [power_base power_exp]
  (binding [power_i nil power_result nil] (try (do (set! power_result 1.0) (set! power_i 0) (while (< power_i power_exp) (do (set! power_result (* power_result power_base)) (set! power_i (+ power_i 1)))) (throw (ex-info "return" {:v power_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn evaluate [evaluate_p evaluate_x]
  (binding [evaluate_i nil evaluate_result nil] (try (do (set! evaluate_result 0.0) (set! evaluate_i 0) (while (<= evaluate_i (:degree evaluate_p)) (do (set! evaluate_result (+ evaluate_result (* (get (:coefficients evaluate_p) evaluate_i) (power evaluate_x evaluate_i)))) (set! evaluate_i (+ evaluate_i 1)))) (throw (ex-info "return" {:v evaluate_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn poly_to_string [poly_to_string_p]
  (binding [poly_to_string_abs_coeff nil poly_to_string_coeff nil poly_to_string_i nil poly_to_string_s nil] (try (do (set! poly_to_string_s "") (set! poly_to_string_i (:degree poly_to_string_p)) (while (>= poly_to_string_i 0) (do (set! poly_to_string_coeff (get (:coefficients poly_to_string_p) poly_to_string_i)) (when (not= poly_to_string_coeff 0.0) (do (if (> (count poly_to_string_s) 0) (if (> poly_to_string_coeff 0.0) (set! poly_to_string_s (str poly_to_string_s " + ")) (set! poly_to_string_s (str poly_to_string_s " - "))) (when (< poly_to_string_coeff 0.0) (set! poly_to_string_s (str poly_to_string_s "-")))) (set! poly_to_string_abs_coeff (if (< poly_to_string_coeff 0.0) (- poly_to_string_coeff) poly_to_string_coeff)) (if (= poly_to_string_i 0) (set! poly_to_string_s (str poly_to_string_s (str poly_to_string_abs_coeff))) (if (= poly_to_string_i 1) (set! poly_to_string_s (str (str poly_to_string_s (str poly_to_string_abs_coeff)) "x")) (set! poly_to_string_s (str (str (str poly_to_string_s (str poly_to_string_abs_coeff)) "x^") (str poly_to_string_i))))))) (set! poly_to_string_i (- poly_to_string_i 1)))) (when (= poly_to_string_s "") (set! poly_to_string_s "0")) (throw (ex-info "return" {:v poly_to_string_s}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn derivative [derivative_p]
  (binding [derivative_coeffs nil derivative_i nil] (try (do (when (= (:degree derivative_p) 0) (throw (ex-info "return" {:v {:coefficients [0.0] :degree 0}}))) (set! derivative_coeffs []) (set! derivative_i 0) (while (< derivative_i (:degree derivative_p)) (do (set! derivative_coeffs (conj derivative_coeffs (* (get (:coefficients derivative_p) (+ derivative_i 1)) (float (+ derivative_i 1))))) (set! derivative_i (+ derivative_i 1)))) (throw (ex-info "return" {:v {:coefficients derivative_coeffs :degree (- (:degree derivative_p) 1)}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn integral [integral_p integral_constant]
  (binding [integral_coeffs nil integral_i nil] (try (do (set! integral_coeffs [integral_constant]) (set! integral_i 0) (while (<= integral_i (:degree integral_p)) (do (set! integral_coeffs (conj integral_coeffs (quot (get (:coefficients integral_p) integral_i) (float (+ integral_i 1))))) (set! integral_i (+ integral_i 1)))) (throw (ex-info "return" {:v {:coefficients integral_coeffs :degree (+ (:degree integral_p) 1)}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn equals [equals_p equals_q]
  (binding [equals_i nil] (try (do (when (not= (:degree equals_p) (:degree equals_q)) (throw (ex-info "return" {:v false}))) (set! equals_i 0) (while (<= equals_i (:degree equals_p)) (do (when (not= (get (:coefficients equals_p) equals_i) (get (:coefficients equals_q) equals_i)) (throw (ex-info "return" {:v false}))) (set! equals_i (+ equals_i 1)))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn not_equals [not_equals_p not_equals_q]
  (try (throw (ex-info "return" {:v (not (equals not_equals_p not_equals_q))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn test_polynomial []
  (binding [test_polynomial_integ nil test_polynomial_p nil test_polynomial_q nil] (do (set! test_polynomial_p (polynomial_new 2 [1.0 2.0 3.0])) (set! test_polynomial_q (polynomial_new 2 [1.0 2.0 3.0])) (when (not= (poly_to_string (add test_polynomial_p test_polynomial_q)) "6x^2 + 4x + 2") (throw (Exception. "add failed"))) (when (not= (poly_to_string (sub test_polynomial_p test_polynomial_q)) "0") (throw (Exception. "sub failed"))) (when (not= (evaluate test_polynomial_p 2.0) 17.0) (throw (Exception. "evaluate failed"))) (when (not= (poly_to_string (derivative test_polynomial_p)) "6x + 2") (throw (Exception. "derivative failed"))) (set! test_polynomial_integ (poly_to_string (integral test_polynomial_p 0.0))) (when (not= test_polynomial_integ "1x^3 + 1x^2 + 1x") (throw (Exception. "integral failed"))) (when (not (equals test_polynomial_p test_polynomial_q)) (throw (Exception. "equals failed"))) (when (not_equals test_polynomial_p test_polynomial_q) (throw (Exception. "not_equals failed"))))))

(defn main []
  (binding [main_d nil main_p nil] (do (test_polynomial) (set! main_p (polynomial_new 2 [1.0 2.0 3.0])) (set! main_d (derivative main_p)) (println (poly_to_string main_d)))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (main)
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
