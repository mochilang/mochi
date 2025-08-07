(ns main (:refer-clojure :exclude [xor_gate]))

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

(declare xor_gate)

(def ^:dynamic xor_gate_zeros nil)

(defn xor_gate [xor_gate_input_1 xor_gate_input_2]
  (binding [xor_gate_zeros nil] (try (do (set! xor_gate_zeros 0) (when (= xor_gate_input_1 0) (set! xor_gate_zeros (+ xor_gate_zeros 1))) (when (= xor_gate_input_2 0) (set! xor_gate_zeros (+ xor_gate_zeros 1))) (throw (ex-info "return" {:v (mod xor_gate_zeros 2)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (xor_gate 0 0))
      (println (xor_gate 0 1))
      (println (xor_gate 1 0))
      (println (xor_gate 1 1))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
