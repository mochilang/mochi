(ns main (:refer-clojure :exclude [is_power_of_two]))

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

(declare is_power_of_two)

(def ^:dynamic is_power_of_two_n nil)

(defn is_power_of_two [is_power_of_two_number]
  (binding [is_power_of_two_n nil] (try (do (when (< is_power_of_two_number 0) (throw (Exception. "number must not be negative"))) (set! is_power_of_two_n is_power_of_two_number) (when (= is_power_of_two_n 0) (throw (ex-info "return" {:v true}))) (while (= (mod is_power_of_two_n 2) 0) (set! is_power_of_two_n (quot is_power_of_two_n 2))) (throw (ex-info "return" {:v (= is_power_of_two_n 1)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]

      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
