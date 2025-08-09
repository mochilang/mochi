(ns main (:refer-clojure :exclude [multiplication_table]))

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

(declare multiplication_table)

(def ^:dynamic multiplication_table_i nil)

(def ^:dynamic multiplication_table_result nil)

(defn multiplication_table [multiplication_table_number multiplication_table_number_of_terms]
  (binding [multiplication_table_i nil multiplication_table_result nil] (try (do (set! multiplication_table_i 1) (set! multiplication_table_result "") (while (<= multiplication_table_i multiplication_table_number_of_terms) (do (set! multiplication_table_result (str (str (str (str (str multiplication_table_result (str multiplication_table_number)) " * ") (str multiplication_table_i)) " = ") (str (* multiplication_table_number multiplication_table_i)))) (when (< multiplication_table_i multiplication_table_number_of_terms) (set! multiplication_table_result (str multiplication_table_result "\n"))) (set! multiplication_table_i (+ multiplication_table_i 1)))) (throw (ex-info "return" {:v multiplication_table_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (multiplication_table 5 10))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
