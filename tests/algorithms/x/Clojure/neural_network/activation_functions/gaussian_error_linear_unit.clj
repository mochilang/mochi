(ns main (:refer-clojure :exclude [exp_taylor sigmoid gaussian_error_linear_unit]))

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

(defn mochi_str [v]
  (cond (float? v) (let [s (str v)] (if (clojure.string/ends-with? s ".0") (subs s 0 (- (count s) 2)) s)) :else (str v)))

(defn _fetch [url]
  {:data [{:from "" :intensity {:actual 0 :forecast 0 :index ""} :to ""}]})

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare exp_taylor sigmoid gaussian_error_linear_unit)

(def ^:dynamic exp_taylor_i nil)

(def ^:dynamic exp_taylor_sum nil)

(def ^:dynamic exp_taylor_term nil)

(def ^:dynamic gaussian_error_linear_unit_gelu nil)

(def ^:dynamic gaussian_error_linear_unit_i nil)

(def ^:dynamic gaussian_error_linear_unit_result nil)

(def ^:dynamic gaussian_error_linear_unit_x nil)

(def ^:dynamic sigmoid_i nil)

(def ^:dynamic sigmoid_result nil)

(def ^:dynamic sigmoid_value nil)

(def ^:dynamic sigmoid_x nil)

(defn exp_taylor [exp_taylor_x]
  (binding [exp_taylor_i nil exp_taylor_sum nil exp_taylor_term nil] (try (do (set! exp_taylor_term 1.0) (set! exp_taylor_sum 1.0) (set! exp_taylor_i 1.0) (while (< exp_taylor_i 20.0) (do (set! exp_taylor_term (quot (* exp_taylor_term exp_taylor_x) exp_taylor_i)) (set! exp_taylor_sum (+ exp_taylor_sum exp_taylor_term)) (set! exp_taylor_i (+ exp_taylor_i 1.0)))) (throw (ex-info "return" {:v exp_taylor_sum}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn sigmoid [sigmoid_vector]
  (binding [sigmoid_i nil sigmoid_result nil sigmoid_value nil sigmoid_x nil] (try (do (set! sigmoid_result []) (set! sigmoid_i 0) (while (< sigmoid_i (count sigmoid_vector)) (do (set! sigmoid_x (nth sigmoid_vector sigmoid_i)) (set! sigmoid_value (/ 1.0 (+ 1.0 (exp_taylor (- sigmoid_x))))) (set! sigmoid_result (conj sigmoid_result sigmoid_value)) (set! sigmoid_i (+ sigmoid_i 1)))) (throw (ex-info "return" {:v sigmoid_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn gaussian_error_linear_unit [gaussian_error_linear_unit_vector]
  (binding [gaussian_error_linear_unit_gelu nil gaussian_error_linear_unit_i nil gaussian_error_linear_unit_result nil gaussian_error_linear_unit_x nil] (try (do (set! gaussian_error_linear_unit_result []) (set! gaussian_error_linear_unit_i 0) (while (< gaussian_error_linear_unit_i (count gaussian_error_linear_unit_vector)) (do (set! gaussian_error_linear_unit_x (nth gaussian_error_linear_unit_vector gaussian_error_linear_unit_i)) (set! gaussian_error_linear_unit_gelu (* gaussian_error_linear_unit_x (/ 1.0 (+ 1.0 (exp_taylor (* (- 1.702) gaussian_error_linear_unit_x)))))) (set! gaussian_error_linear_unit_result (conj gaussian_error_linear_unit_result gaussian_error_linear_unit_gelu)) (set! gaussian_error_linear_unit_i (+ gaussian_error_linear_unit_i 1)))) (throw (ex-info "return" {:v gaussian_error_linear_unit_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_sample nil)

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (alter-var-root (var main_sample) (constantly [(- 1.0) 1.0 2.0]))
      (println (sigmoid main_sample))
      (println (gaussian_error_linear_unit main_sample))
      (println (gaussian_error_linear_unit [(- 3.0)]))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
