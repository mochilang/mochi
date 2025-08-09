(ns main (:refer-clojure :exclude [abs_float fail calc_derivative newton_raphson poly]))

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

(declare abs_float fail calc_derivative newton_raphson poly)

(def ^:dynamic newton_raphson_a nil)

(def ^:dynamic newton_raphson_der nil)

(def ^:dynamic newton_raphson_err nil)

(def ^:dynamic newton_raphson_i nil)

(def ^:dynamic newton_raphson_steps nil)

(defn abs_float [abs_float_x]
  (try (if (< abs_float_x 0.0) (throw (ex-info "return" {:v (- abs_float_x)})) (throw (ex-info "return" {:v abs_float_x}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn fail [fail_msg]
  (do (println (str "error: " fail_msg)) fail_msg))

(defn calc_derivative [calc_derivative_f calc_derivative_x calc_derivative_delta_x]
  (try (throw (ex-info "return" {:v (/ (- (calc_derivative_f (+ calc_derivative_x (/ calc_derivative_delta_x 2.0))) (calc_derivative_f (- calc_derivative_x (/ calc_derivative_delta_x 2.0)))) calc_derivative_delta_x)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn newton_raphson [newton_raphson_f newton_raphson_x0 newton_raphson_max_iter newton_raphson_step newton_raphson_max_error newton_raphson_log_steps]
  (binding [newton_raphson_a nil newton_raphson_der nil newton_raphson_err nil newton_raphson_i nil newton_raphson_steps nil] (try (do (set! newton_raphson_a newton_raphson_x0) (set! newton_raphson_steps []) (set! newton_raphson_i 0) (while (< newton_raphson_i newton_raphson_max_iter) (do (when newton_raphson_log_steps (set! newton_raphson_steps (conj newton_raphson_steps newton_raphson_a))) (set! newton_raphson_err (abs_float (newton_raphson_f newton_raphson_a))) (when (< newton_raphson_err newton_raphson_max_error) (throw (ex-info "return" {:v {:error newton_raphson_err :root newton_raphson_a :steps newton_raphson_steps}}))) (set! newton_raphson_der (calc_derivative newton_raphson_f newton_raphson_a newton_raphson_step)) (when (= newton_raphson_der 0.0) (do (fail "No converging solution found, zero derivative") (throw (ex-info "return" {:v {:error newton_raphson_err :root newton_raphson_a :steps newton_raphson_steps}})))) (set! newton_raphson_a (- newton_raphson_a (quot (newton_raphson_f newton_raphson_a) newton_raphson_der))) (set! newton_raphson_i (+ newton_raphson_i 1)))) (fail "No converging solution found, iteration limit reached") (throw (ex-info "return" {:v {:error (abs_float (newton_raphson_f newton_raphson_a)) :root newton_raphson_a :steps newton_raphson_steps}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn poly [poly_x]
  (try (throw (ex-info "return" {:v (+ (- (* poly_x poly_x) (* 5.0 poly_x)) 2.0)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(def ^:dynamic main_result (newton_raphson poly 0.4 20 0.000001 0.000001 false))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (str (str "root = " (str (:root main_result))) ", error = ") (str (:error main_result))))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
