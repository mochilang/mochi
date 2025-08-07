(ns main (:refer-clojure :exclude [partition kth_largest_element]))

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

(declare partition kth_largest_element)

(def ^:dynamic kth_largest_element_high nil)

(def ^:dynamic kth_largest_element_low nil)

(def ^:dynamic kth_largest_element_pivot_index nil)

(def ^:dynamic partition_arr nil)

(def ^:dynamic partition_i nil)

(def ^:dynamic partition_j nil)

(def ^:dynamic partition_k nil)

(def ^:dynamic partition_pivot nil)

(def ^:dynamic partition_tmp nil)

(defn partition [partition_arr_p partition_low partition_high]
  (binding [partition_arr nil partition_i nil partition_j nil partition_k nil partition_pivot nil partition_tmp nil] (try (do (set! partition_arr partition_arr_p) (set! partition_pivot (nth partition_arr partition_high)) (set! partition_i (- partition_low 1)) (set! partition_j partition_low) (while (< partition_j partition_high) (do (when (>= (nth partition_arr partition_j) partition_pivot) (do (set! partition_i (+ partition_i 1)) (set! partition_tmp (nth partition_arr partition_i)) (set! partition_arr (assoc partition_arr partition_i (nth partition_arr partition_j))) (set! partition_arr (assoc partition_arr partition_j partition_tmp)))) (set! partition_j (+ partition_j 1)))) (set! partition_k (+ partition_i 1)) (set! partition_tmp (nth partition_arr partition_k)) (set! partition_arr (assoc partition_arr partition_k (nth partition_arr partition_high))) (set! partition_arr (assoc partition_arr partition_high partition_tmp)) (throw (ex-info "return" {:v partition_k}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn kth_largest_element [kth_largest_element_arr kth_largest_element_position]
  (binding [kth_largest_element_high nil kth_largest_element_low nil kth_largest_element_pivot_index nil] (try (do (when (= (count kth_largest_element_arr) 0) (throw (ex-info "return" {:v (- 1)}))) (when (or (< kth_largest_element_position 1) (> kth_largest_element_position (count kth_largest_element_arr))) (throw (ex-info "return" {:v (- 1)}))) (set! kth_largest_element_low 0) (set! kth_largest_element_high (- (count kth_largest_element_arr) 1)) (while (<= kth_largest_element_low kth_largest_element_high) (do (when (or (> kth_largest_element_low (- (count kth_largest_element_arr) 1)) (< kth_largest_element_high 0)) (throw (ex-info "return" {:v (- 1)}))) (set! kth_largest_element_pivot_index (partition kth_largest_element_arr kth_largest_element_low kth_largest_element_high)) (if (= kth_largest_element_pivot_index (- kth_largest_element_position 1)) (throw (ex-info "return" {:v (nth kth_largest_element_arr kth_largest_element_pivot_index)})) (if (> kth_largest_element_pivot_index (- kth_largest_element_position 1)) (set! kth_largest_element_high (- kth_largest_element_pivot_index 1)) (set! kth_largest_element_low (+ kth_largest_element_pivot_index 1)))))) (throw (ex-info "return" {:v (- 1)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_arr1 [3 1 4 1 5 9 2 6 5 3 5])

(def ^:dynamic main_arr2 [2 5 6 1 9 3 8 4 7 3 5])

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (kth_largest_element main_arr1 3))
      (println "\n")
      (println (kth_largest_element main_arr2 1))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
