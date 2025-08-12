(ns main (:refer-clojure :exclude [exp scaled_exponential_linear_unit]))

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

(declare exp scaled_exponential_linear_unit)

(def ^:dynamic exp_n nil)

(def ^:dynamic exp_sum nil)

(def ^:dynamic exp_term nil)

(def ^:dynamic scaled_exponential_linear_unit_i nil)

(def ^:dynamic scaled_exponential_linear_unit_result nil)

(def ^:dynamic scaled_exponential_linear_unit_x nil)

(def ^:dynamic scaled_exponential_linear_unit_y nil)

(defn exp [exp_x]
  (binding [exp_n nil exp_sum nil exp_term nil] (try (do (set! exp_term 1.0) (set! exp_sum 1.0) (set! exp_n 1) (while (< exp_n 20) (do (set! exp_term (/ (* exp_term exp_x) (double exp_n))) (set! exp_sum (+ exp_sum exp_term)) (set! exp_n (+ exp_n 1)))) (throw (ex-info "return" {:v exp_sum}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn scaled_exponential_linear_unit [scaled_exponential_linear_unit_vector scaled_exponential_linear_unit_alpha scaled_exponential_linear_unit_lambda_]
  (binding [scaled_exponential_linear_unit_i nil scaled_exponential_linear_unit_result nil scaled_exponential_linear_unit_x nil scaled_exponential_linear_unit_y nil] (try (do (set! scaled_exponential_linear_unit_result []) (set! scaled_exponential_linear_unit_i 0) (while (< scaled_exponential_linear_unit_i (count scaled_exponential_linear_unit_vector)) (do (set! scaled_exponential_linear_unit_x (nth scaled_exponential_linear_unit_vector scaled_exponential_linear_unit_i)) (set! scaled_exponential_linear_unit_y (if (> scaled_exponential_linear_unit_x 0.0) (* scaled_exponential_linear_unit_lambda_ scaled_exponential_linear_unit_x) (* (* scaled_exponential_linear_unit_lambda_ scaled_exponential_linear_unit_alpha) (- (exp scaled_exponential_linear_unit_x) 1.0)))) (set! scaled_exponential_linear_unit_result (conj scaled_exponential_linear_unit_result scaled_exponential_linear_unit_y)) (set! scaled_exponential_linear_unit_i (+ scaled_exponential_linear_unit_i 1)))) (throw (ex-info "return" {:v scaled_exponential_linear_unit_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (scaled_exponential_linear_unit [1.3 3.7 2.4] 1.6732 1.0507))
      (println (scaled_exponential_linear_unit [1.3 4.7 8.2] 1.6732 1.0507))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
