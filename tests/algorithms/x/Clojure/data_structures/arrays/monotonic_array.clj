(ns main (:refer-clojure :exclude [is_monotonic]))

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

(declare is_monotonic)

(def ^:dynamic is_monotonic_decreasing nil)

(def ^:dynamic is_monotonic_i nil)

(def ^:dynamic is_monotonic_increasing nil)

(defn is_monotonic [is_monotonic_nums]
  (binding [is_monotonic_decreasing nil is_monotonic_i nil is_monotonic_increasing nil] (try (do (when (<= (count is_monotonic_nums) 2) (throw (ex-info "return" {:v true}))) (set! is_monotonic_increasing true) (set! is_monotonic_decreasing true) (set! is_monotonic_i 0) (while (< is_monotonic_i (- (count is_monotonic_nums) 1)) (do (when (> (nth is_monotonic_nums is_monotonic_i) (nth is_monotonic_nums (+ is_monotonic_i 1))) (set! is_monotonic_increasing false)) (when (< (nth is_monotonic_nums is_monotonic_i) (nth is_monotonic_nums (+ is_monotonic_i 1))) (set! is_monotonic_decreasing false)) (set! is_monotonic_i (+ is_monotonic_i 1)))) (throw (ex-info "return" {:v (or is_monotonic_increasing is_monotonic_decreasing)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (is_monotonic [1 2 2 3])))
      (println (str (is_monotonic [6 5 4 4])))
      (println (str (is_monotonic [1 3 2])))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
