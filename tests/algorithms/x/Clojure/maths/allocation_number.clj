(ns main (:refer-clojure :exclude [allocation_num]))

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

(declare allocation_num)

(def ^:dynamic allocation_num_allocation_list nil)

(def ^:dynamic allocation_num_bytes_per_partition nil)

(def ^:dynamic allocation_num_end_bytes nil)

(def ^:dynamic allocation_num_i nil)

(def ^:dynamic allocation_num_start_bytes nil)

(defn allocation_num [allocation_num_number_of_bytes allocation_num_partitions]
  (binding [allocation_num_allocation_list nil allocation_num_bytes_per_partition nil allocation_num_end_bytes nil allocation_num_i nil allocation_num_start_bytes nil] (try (do (when (<= allocation_num_partitions 0) (throw (Exception. "partitions must be a positive number!"))) (when (> allocation_num_partitions allocation_num_number_of_bytes) (throw (Exception. "partitions can not > number_of_bytes!"))) (set! allocation_num_bytes_per_partition (quot allocation_num_number_of_bytes allocation_num_partitions)) (set! allocation_num_allocation_list []) (set! allocation_num_i 0) (while (< allocation_num_i allocation_num_partitions) (do (set! allocation_num_start_bytes (+ (* allocation_num_i allocation_num_bytes_per_partition) 1)) (set! allocation_num_end_bytes (if (= allocation_num_i (- allocation_num_partitions 1)) allocation_num_number_of_bytes (* (+ allocation_num_i 1) allocation_num_bytes_per_partition))) (set! allocation_num_allocation_list (conj allocation_num_allocation_list (str (str (str allocation_num_start_bytes) "-") (str allocation_num_end_bytes)))) (set! allocation_num_i (+ allocation_num_i 1)))) (throw (ex-info "return" {:v allocation_num_allocation_list}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (allocation_num 16647 4)))
      (println (str (allocation_num 50000 5)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
