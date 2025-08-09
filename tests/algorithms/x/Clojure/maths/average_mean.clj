(ns main (:refer-clojure :exclude [mean]))

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

(declare mean)

(def ^:dynamic mean_i nil)

(def ^:dynamic mean_total nil)

(defn mean [mean_nums]
  (binding [mean_i nil mean_total nil] (try (do (when (= (count mean_nums) 0) (throw (Exception. "List is empty"))) (set! mean_total 0.0) (set! mean_i 0) (while (< mean_i (count mean_nums)) (do (set! mean_total (+ mean_total (nth mean_nums mean_i))) (set! mean_i (+ mean_i 1)))) (throw (ex-info "return" {:v (quot mean_total (double (count mean_nums)))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (mean [3.0 6.0 9.0 12.0 15.0 18.0 21.0])))
      (println (str (mean [5.0 10.0 15.0 20.0 25.0 30.0 35.0])))
      (println (str (mean [1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0])))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
