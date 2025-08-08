(ns main (:refer-clojure :exclude [factorial]))

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

(declare factorial)

(def ^:dynamic factorial_i nil)

(def ^:dynamic factorial_m nil)

(def ^:dynamic main_results nil)

(def ^:dynamic main_memo [1 1])

(defn factorial [factorial_num]
  (binding [factorial_i nil factorial_m nil] (try (do (when (< factorial_num 0) (do (println "Number should not be negative.") (throw (ex-info "return" {:v 0})))) (set! factorial_m main_memo) (set! factorial_i (count factorial_m)) (while (<= factorial_i factorial_num) (do (set! factorial_m (conj factorial_m (* factorial_i (nth factorial_m (- factorial_i 1))))) (set! factorial_i (+ factorial_i 1)))) (alter-var-root (var main_memo) (fn [_] factorial_m)) (throw (ex-info "return" {:v (nth factorial_m factorial_num)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_results [])

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (factorial 7)))
      (factorial (- 1))
      (dotimes [i 10] (def main_results (conj main_results (factorial i))))
      (println (str main_results))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
