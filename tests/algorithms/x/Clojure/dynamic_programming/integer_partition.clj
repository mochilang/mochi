(ns main (:refer-clojure :exclude [partition]))

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

(declare partition)

(def ^:dynamic partition_i nil)

(def ^:dynamic partition_j nil)

(def ^:dynamic partition_k nil)

(def ^:dynamic partition_memo nil)

(def ^:dynamic partition_n nil)

(def ^:dynamic partition_row nil)

(defn partition [partition_m]
  (binding [partition_i nil partition_j nil partition_k nil partition_memo nil partition_n nil partition_row nil] (try (do (set! partition_memo []) (set! partition_i 0) (while (< partition_i (+ partition_m 1)) (do (set! partition_row []) (set! partition_j 0) (while (< partition_j partition_m) (do (set! partition_row (conj partition_row 0)) (set! partition_j (+ partition_j 1)))) (set! partition_memo (conj partition_memo partition_row)) (set! partition_i (+ partition_i 1)))) (set! partition_i 0) (while (< partition_i (+ partition_m 1)) (do (set! partition_memo (assoc-in partition_memo [partition_i 0] 1)) (set! partition_i (+ partition_i 1)))) (set! partition_n 0) (while (< partition_n (+ partition_m 1)) (do (set! partition_k 1) (while (< partition_k partition_m) (do (set! partition_memo (assoc-in partition_memo [partition_n partition_k] (+ (nth (nth partition_memo partition_n) partition_k) (nth (nth partition_memo partition_n) (- partition_k 1))))) (when (> (- partition_n partition_k) 0) (set! partition_memo (assoc-in partition_memo [partition_n partition_k] (+ (nth (nth partition_memo partition_n) partition_k) (nth (nth partition_memo (- (- partition_n partition_k) 1)) partition_k))))) (set! partition_k (+ partition_k 1)))) (set! partition_n (+ partition_n 1)))) (throw (ex-info "return" {:v (nth (nth partition_memo partition_m) (- partition_m 1))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (partition 5))
      (println (partition 7))
      (println (partition 100))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
