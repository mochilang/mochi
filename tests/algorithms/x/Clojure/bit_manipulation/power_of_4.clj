(ns main (:refer-clojure :exclude [power_of_4]))

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

(declare power_of_4)

(def ^:dynamic power_of_4_n nil)

(defn power_of_4 [power_of_4_number]
  (binding [power_of_4_n nil] (try (do (when (<= power_of_4_number 0) (throw (ex-info "return" {:v false}))) (set! power_of_4_n power_of_4_number) (while (= (mod power_of_4_n 4) 0) (set! power_of_4_n (quot power_of_4_n 4))) (throw (ex-info "return" {:v (= power_of_4_n 1)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (power_of_4 1)))
      (println (str (power_of_4 2)))
      (println (str (power_of_4 4)))
      (println (str (power_of_4 6)))
      (println (str (power_of_4 64)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
