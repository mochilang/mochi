(ns main (:refer-clojure :exclude [perfect]))

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

(declare perfect)

(def ^:dynamic perfect_divisor nil)

(def ^:dynamic perfect_total nil)

(defn perfect [perfect_n]
  (binding [perfect_divisor nil perfect_total nil] (try (do (when (<= perfect_n 0) (throw (ex-info "return" {:v false}))) (set! perfect_total 0) (set! perfect_divisor 1) (while (<= perfect_divisor (quot perfect_n 2)) (do (when (= (mod perfect_n perfect_divisor) 0) (set! perfect_total (+ perfect_total perfect_divisor))) (set! perfect_divisor (+ perfect_divisor 1)))) (throw (ex-info "return" {:v (= perfect_total perfect_n)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (perfect 27)))
      (println (str (perfect 28)))
      (println (str (perfect 29)))
      (println (str (perfect 6)))
      (println (str (perfect 12)))
      (println (str (perfect 496)))
      (println (str (perfect 8128)))
      (println (str (perfect 0)))
      (println (str (perfect (- 1))))
      (println (str (perfect 33550336)))
      (println (str (perfect 33550337)))
      (println (str (perfect 1)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
