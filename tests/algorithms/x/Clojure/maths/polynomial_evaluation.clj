(ns main (:refer-clojure :exclude [pow_float evaluate_poly horner test_polynomial_evaluation main]))

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

(declare pow_float evaluate_poly horner test_polynomial_evaluation main)

(def ^:dynamic evaluate_poly_i nil)

(def ^:dynamic evaluate_poly_total nil)

(def ^:dynamic horner_i nil)

(def ^:dynamic horner_result nil)

(def ^:dynamic main_poly nil)

(def ^:dynamic main_x nil)

(def ^:dynamic pow_float_exp nil)

(def ^:dynamic pow_float_i nil)

(def ^:dynamic pow_float_result nil)

(def ^:dynamic test_polynomial_evaluation_poly nil)

(def ^:dynamic test_polynomial_evaluation_x nil)

(defn pow_float [pow_float_base pow_float_exponent]
  (binding [pow_float_exp nil pow_float_i nil pow_float_result nil] (try (do (set! pow_float_exp pow_float_exponent) (set! pow_float_result 1.0) (set! pow_float_i 0) (while (< pow_float_i pow_float_exp) (do (set! pow_float_result (* pow_float_result pow_float_base)) (set! pow_float_i (+ pow_float_i 1)))) (throw (ex-info "return" {:v pow_float_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn evaluate_poly [evaluate_poly_poly evaluate_poly_x]
  (binding [evaluate_poly_i nil evaluate_poly_total nil] (try (do (set! evaluate_poly_total 0.0) (set! evaluate_poly_i 0) (while (< evaluate_poly_i (count evaluate_poly_poly)) (do (set! evaluate_poly_total (+ evaluate_poly_total (* (nth evaluate_poly_poly evaluate_poly_i) (pow_float evaluate_poly_x evaluate_poly_i)))) (set! evaluate_poly_i (+ evaluate_poly_i 1)))) (throw (ex-info "return" {:v evaluate_poly_total}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn horner [horner_poly horner_x]
  (binding [horner_i nil horner_result nil] (try (do (set! horner_result 0.0) (set! horner_i (- (count horner_poly) 1)) (while (>= horner_i 0) (do (set! horner_result (+ (* horner_result horner_x) (nth horner_poly horner_i))) (set! horner_i (- horner_i 1)))) (throw (ex-info "return" {:v horner_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn test_polynomial_evaluation []
  (binding [test_polynomial_evaluation_poly nil test_polynomial_evaluation_x nil] (do (set! test_polynomial_evaluation_poly [0.0 0.0 5.0 9.3 7.0]) (set! test_polynomial_evaluation_x 10.0) (when (not= (evaluate_poly test_polynomial_evaluation_poly test_polynomial_evaluation_x) 79800.0) (throw (Exception. "evaluate_poly failed"))) (when (not= (horner test_polynomial_evaluation_poly test_polynomial_evaluation_x) 79800.0) (throw (Exception. "horner failed"))))))

(defn main []
  (binding [main_poly nil main_x nil] (do (test_polynomial_evaluation) (set! main_poly [0.0 0.0 5.0 9.3 7.0]) (set! main_x 10.0) (println (evaluate_poly main_poly main_x)) (println (horner main_poly main_x)))))

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
