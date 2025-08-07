(ns main (:refer-clojure :exclude [actual_power power]))

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

(declare actual_power power)

(def ^:dynamic actual_power_half nil)

(defn actual_power [actual_power_a actual_power_b]
  (binding [actual_power_half nil] (try (do (when (= actual_power_b 0) (throw (ex-info "return" {:v 1}))) (set! actual_power_half (actual_power actual_power_a (quot actual_power_b 2))) (if (= (mod actual_power_b 2) 0) (* actual_power_half actual_power_half) (* (* actual_power_a actual_power_half) actual_power_half))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn power [power_a power_b]
  (try (if (< power_b 0) (/ 1.0 (* 1.0 (actual_power power_a (- power_b)))) (* 1.0 (actual_power power_a power_b))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (power 4 6)))
      (println (str (power 2 3)))
      (println (str (power (- 2) 3)))
      (println (str (power 2 (- 3))))
      (println (str (power (- 2) (- 3))))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
