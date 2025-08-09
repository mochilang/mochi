(ns main (:refer-clojure :exclude [exp_approx sigmoid]))

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

(declare exp_approx sigmoid)

(def ^:dynamic exp_approx_i nil)

(def ^:dynamic exp_approx_sum nil)

(def ^:dynamic exp_approx_term nil)

(def ^:dynamic sigmoid_i nil)

(def ^:dynamic sigmoid_result nil)

(def ^:dynamic sigmoid_s nil)

(def ^:dynamic sigmoid_v nil)

(defn exp_approx [exp_approx_x]
  (binding [exp_approx_i nil exp_approx_sum nil exp_approx_term nil] (try (do (set! exp_approx_sum 1.0) (set! exp_approx_term 1.0) (set! exp_approx_i 1) (while (<= exp_approx_i 10) (do (set! exp_approx_term (quot (* exp_approx_term exp_approx_x) (double exp_approx_i))) (set! exp_approx_sum (+ exp_approx_sum exp_approx_term)) (set! exp_approx_i (+ exp_approx_i 1)))) (throw (ex-info "return" {:v exp_approx_sum}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn sigmoid [sigmoid_vector]
  (binding [sigmoid_i nil sigmoid_result nil sigmoid_s nil sigmoid_v nil] (try (do (set! sigmoid_result []) (set! sigmoid_i 0) (while (< sigmoid_i (count sigmoid_vector)) (do (set! sigmoid_v (nth sigmoid_vector sigmoid_i)) (set! sigmoid_s (/ 1.0 (+ 1.0 (exp_approx (- sigmoid_v))))) (set! sigmoid_result (conj sigmoid_result sigmoid_s)) (set! sigmoid_i (+ sigmoid_i 1)))) (throw (ex-info "return" {:v sigmoid_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (sigmoid [(- 1.0) 1.0 2.0])))
      (println (str (sigmoid [0.0])))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
