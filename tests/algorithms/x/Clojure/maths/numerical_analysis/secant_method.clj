(ns main (:refer-clojure :exclude [exp_approx f secant_method]))

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

(declare exp_approx f secant_method)

(def ^:dynamic exp_approx_i nil)

(def ^:dynamic exp_approx_sum nil)

(def ^:dynamic exp_approx_term nil)

(def ^:dynamic secant_method_fx0 nil)

(def ^:dynamic secant_method_fx1 nil)

(def ^:dynamic secant_method_i nil)

(def ^:dynamic secant_method_new_x nil)

(def ^:dynamic secant_method_x0 nil)

(def ^:dynamic secant_method_x1 nil)

(defn exp_approx [exp_approx_x]
  (binding [exp_approx_i nil exp_approx_sum nil exp_approx_term nil] (try (do (set! exp_approx_sum 1.0) (set! exp_approx_term 1.0) (set! exp_approx_i 1) (while (<= exp_approx_i 20) (do (set! exp_approx_term (quot (* exp_approx_term exp_approx_x) exp_approx_i)) (set! exp_approx_sum (+ exp_approx_sum exp_approx_term)) (set! exp_approx_i (+ exp_approx_i 1)))) (throw (ex-info "return" {:v exp_approx_sum}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn f [f_x]
  (try (throw (ex-info "return" {:v (- (* 8.0 f_x) (* 2.0 (exp_approx (- f_x))))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn secant_method [secant_method_lower_bound secant_method_upper_bound secant_method_repeats]
  (binding [secant_method_fx0 nil secant_method_fx1 nil secant_method_i nil secant_method_new_x nil secant_method_x0 nil secant_method_x1 nil] (try (do (set! secant_method_x0 secant_method_lower_bound) (set! secant_method_x1 secant_method_upper_bound) (set! secant_method_i 0) (while (< secant_method_i secant_method_repeats) (do (set! secant_method_fx1 (f secant_method_x1)) (set! secant_method_fx0 (f secant_method_x0)) (set! secant_method_new_x (- secant_method_x1 (quot (* secant_method_fx1 (- secant_method_x1 secant_method_x0)) (- secant_method_fx1 secant_method_fx0)))) (set! secant_method_x0 secant_method_x1) (set! secant_method_x1 secant_method_new_x) (set! secant_method_i (+ secant_method_i 1)))) (throw (ex-info "return" {:v secant_method_x1}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (secant_method 1.0 3.0 2)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
