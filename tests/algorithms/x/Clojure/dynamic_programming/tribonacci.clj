(ns main (:refer-clojure :exclude [tribonacci]))

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

(declare tribonacci)

(def ^:dynamic tribonacci_dp nil)

(def ^:dynamic tribonacci_i nil)

(def ^:dynamic tribonacci_t nil)

(defn tribonacci [tribonacci_num]
  (binding [tribonacci_dp nil tribonacci_i nil tribonacci_t nil] (try (do (set! tribonacci_dp []) (set! tribonacci_i 0) (while (< tribonacci_i tribonacci_num) (do (if (or (= tribonacci_i 0) (= tribonacci_i 1)) (set! tribonacci_dp (conj tribonacci_dp 0)) (if (= tribonacci_i 2) (set! tribonacci_dp (conj tribonacci_dp 1)) (do (set! tribonacci_t (+ (+ (nth tribonacci_dp (- tribonacci_i 1)) (nth tribonacci_dp (- tribonacci_i 2))) (nth tribonacci_dp (- tribonacci_i 3)))) (set! tribonacci_dp (conj tribonacci_dp tribonacci_t))))) (set! tribonacci_i (+ tribonacci_i 1)))) (throw (ex-info "return" {:v tribonacci_dp}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (tribonacci 8))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
