(ns main (:refer-clojure :exclude [aliquot_sum]))

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

(declare aliquot_sum)

(def ^:dynamic aliquot_sum_divisor nil)

(def ^:dynamic aliquot_sum_total nil)

(defn aliquot_sum [aliquot_sum_n]
  (binding [aliquot_sum_divisor nil aliquot_sum_total nil] (try (do (when (<= aliquot_sum_n 0) (throw (Exception. "Input must be positive"))) (set! aliquot_sum_total 0) (set! aliquot_sum_divisor 1) (while (<= aliquot_sum_divisor (quot aliquot_sum_n 2)) (do (when (= (mod aliquot_sum_n aliquot_sum_divisor) 0) (set! aliquot_sum_total (+ aliquot_sum_total aliquot_sum_divisor))) (set! aliquot_sum_divisor (+ aliquot_sum_divisor 1)))) (throw (ex-info "return" {:v aliquot_sum_total}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (aliquot_sum 15)))
      (println (str (aliquot_sum 6)))
      (println (str (aliquot_sum 12)))
      (println (str (aliquot_sum 1)))
      (println (str (aliquot_sum 19)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
