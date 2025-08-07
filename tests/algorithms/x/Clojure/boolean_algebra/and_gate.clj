(ns main (:refer-clojure :exclude [and_gate n_input_and_gate]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(defn indexOf [s sub]
  (let [idx (clojure.string/index-of s sub)] (if (nil? idx) -1 idx)))

(defn split [s sep]
  (clojure.string/split s (re-pattern sep)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare and_gate n_input_and_gate)

(def ^:dynamic n_input_and_gate_i nil)

(defn and_gate [and_gate_input_1 and_gate_input_2]
  (try (if (and (not= and_gate_input_1 0) (not= and_gate_input_2 0)) 1 0) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn n_input_and_gate [n_input_and_gate_inputs]
  (binding [n_input_and_gate_i nil] (try (do (set! n_input_and_gate_i 0) (while (< n_input_and_gate_i (count n_input_and_gate_inputs)) (do (when (= (nth n_input_and_gate_inputs n_input_and_gate_i) 0) (throw (ex-info "return" {:v 0}))) (set! n_input_and_gate_i (+ n_input_and_gate_i 1)))) (throw (ex-info "return" {:v 1}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (and_gate 0 0))
      (println (and_gate 0 1))
      (println (and_gate 1 0))
      (println (and_gate 1 1))
      (println (n_input_and_gate [1 0 1 1 0]))
      (println (n_input_and_gate [1 1 1 1 1]))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
