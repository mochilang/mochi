(ns main (:refer-clojure :exclude [sortFloats find_median_sorted_arrays]))

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

(declare sortFloats find_median_sorted_arrays)

(def ^:dynamic find_median_sorted_arrays_i nil)

(def ^:dynamic find_median_sorted_arrays_j nil)

(def ^:dynamic find_median_sorted_arrays_merged nil)

(def ^:dynamic find_median_sorted_arrays_middle1 nil)

(def ^:dynamic find_median_sorted_arrays_middle2 nil)

(def ^:dynamic find_median_sorted_arrays_sorted nil)

(def ^:dynamic find_median_sorted_arrays_total nil)

(def ^:dynamic sortFloats_arr nil)

(def ^:dynamic sortFloats_i nil)

(def ^:dynamic sortFloats_j nil)

(def ^:dynamic sortFloats_t nil)

(defn sortFloats [sortFloats_xs]
  (binding [sortFloats_arr nil sortFloats_i nil sortFloats_j nil sortFloats_t nil] (try (do (set! sortFloats_arr sortFloats_xs) (set! sortFloats_i 0) (while (< sortFloats_i (count sortFloats_arr)) (do (set! sortFloats_j 0) (while (< sortFloats_j (- (count sortFloats_arr) 1)) (do (when (> (nth sortFloats_arr sortFloats_j) (nth sortFloats_arr (+ sortFloats_j 1))) (do (set! sortFloats_t (nth sortFloats_arr sortFloats_j)) (set! sortFloats_arr (assoc sortFloats_arr sortFloats_j (nth sortFloats_arr (+ sortFloats_j 1)))) (set! sortFloats_arr (assoc sortFloats_arr (+ sortFloats_j 1) sortFloats_t)))) (set! sortFloats_j (+ sortFloats_j 1)))) (set! sortFloats_i (+ sortFloats_i 1)))) (throw (ex-info "return" {:v sortFloats_arr}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn find_median_sorted_arrays [find_median_sorted_arrays_nums1 find_median_sorted_arrays_nums2]
  (binding [find_median_sorted_arrays_i nil find_median_sorted_arrays_j nil find_median_sorted_arrays_merged nil find_median_sorted_arrays_middle1 nil find_median_sorted_arrays_middle2 nil find_median_sorted_arrays_sorted nil find_median_sorted_arrays_total nil] (try (do (when (and (= (count find_median_sorted_arrays_nums1) 0) (= (count find_median_sorted_arrays_nums2) 0)) (throw (Exception. "Both input arrays are empty."))) (set! find_median_sorted_arrays_merged []) (set! find_median_sorted_arrays_i 0) (while (< find_median_sorted_arrays_i (count find_median_sorted_arrays_nums1)) (do (set! find_median_sorted_arrays_merged (conj find_median_sorted_arrays_merged (nth find_median_sorted_arrays_nums1 find_median_sorted_arrays_i))) (set! find_median_sorted_arrays_i (+ find_median_sorted_arrays_i 1)))) (set! find_median_sorted_arrays_j 0) (while (< find_median_sorted_arrays_j (count find_median_sorted_arrays_nums2)) (do (set! find_median_sorted_arrays_merged (conj find_median_sorted_arrays_merged (nth find_median_sorted_arrays_nums2 find_median_sorted_arrays_j))) (set! find_median_sorted_arrays_j (+ find_median_sorted_arrays_j 1)))) (set! find_median_sorted_arrays_sorted (sortFloats find_median_sorted_arrays_merged)) (set! find_median_sorted_arrays_total (count find_median_sorted_arrays_sorted)) (when (= (mod find_median_sorted_arrays_total 2) 1) (throw (ex-info "return" {:v (nth find_median_sorted_arrays_sorted (quot find_median_sorted_arrays_total 2))}))) (set! find_median_sorted_arrays_middle1 (nth find_median_sorted_arrays_sorted (- (quot find_median_sorted_arrays_total 2) 1))) (set! find_median_sorted_arrays_middle2 (nth find_median_sorted_arrays_sorted (quot find_median_sorted_arrays_total 2))) (throw (ex-info "return" {:v (/ (+ find_median_sorted_arrays_middle1 find_median_sorted_arrays_middle2) 2.0)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (find_median_sorted_arrays [1.0 3.0] [2.0]))
      (println (find_median_sorted_arrays [1.0 2.0] [3.0 4.0]))
      (println (find_median_sorted_arrays [0.0 0.0] [0.0 0.0]))
      (println (find_median_sorted_arrays [] [1.0]))
      (println (find_median_sorted_arrays [(- 1000.0)] [1000.0]))
      (println (find_median_sorted_arrays [(- 1.1) (- 2.2)] [(- 3.3) (- 4.4)]))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
