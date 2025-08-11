(ns main (:refer-clojure :exclude [quick_sort_3partition quick_sort_lomuto_partition lomuto_partition three_way_radix_quicksort]))

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

(defn _fetch [url]
  {:data [{:from "" :intensity {:actual 0 :forecast 0 :index ""} :to ""}]})

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare quick_sort_3partition quick_sort_lomuto_partition lomuto_partition three_way_radix_quicksort)

(def ^:dynamic lomuto_partition_arr nil)

(def ^:dynamic lomuto_partition_i nil)

(def ^:dynamic lomuto_partition_pivot nil)

(def ^:dynamic lomuto_partition_store_index nil)

(def ^:dynamic lomuto_partition_temp nil)

(def ^:dynamic main_array1 nil)

(def ^:dynamic main_array2 nil)

(def ^:dynamic main_array3 nil)

(def ^:dynamic main_nums1 nil)

(def ^:dynamic main_nums2 nil)

(def ^:dynamic main_nums3 nil)

(def ^:dynamic quick_sort_3partition_a nil)

(def ^:dynamic quick_sort_3partition_arr nil)

(def ^:dynamic quick_sort_3partition_b nil)

(def ^:dynamic quick_sort_3partition_i nil)

(def ^:dynamic quick_sort_3partition_pivot nil)

(def ^:dynamic quick_sort_3partition_temp nil)

(def ^:dynamic quick_sort_lomuto_partition_arr nil)

(def ^:dynamic quick_sort_lomuto_partition_pivot_index nil)

(def ^:dynamic three_way_radix_quicksort_equal nil)

(def ^:dynamic three_way_radix_quicksort_greater nil)

(def ^:dynamic three_way_radix_quicksort_i nil)

(def ^:dynamic three_way_radix_quicksort_less nil)

(def ^:dynamic three_way_radix_quicksort_pivot nil)

(def ^:dynamic three_way_radix_quicksort_result nil)

(def ^:dynamic three_way_radix_quicksort_sorted_greater nil)

(def ^:dynamic three_way_radix_quicksort_sorted_less nil)

(def ^:dynamic three_way_radix_quicksort_val nil)

(defn quick_sort_3partition [quick_sort_3partition_arr_p quick_sort_3partition_left quick_sort_3partition_right]
  (binding [quick_sort_3partition_a nil quick_sort_3partition_arr nil quick_sort_3partition_b nil quick_sort_3partition_i nil quick_sort_3partition_pivot nil quick_sort_3partition_temp nil] (try (do (set! quick_sort_3partition_arr quick_sort_3partition_arr_p) (when (<= quick_sort_3partition_right quick_sort_3partition_left) (throw (ex-info "return" {:v quick_sort_3partition_arr}))) (set! quick_sort_3partition_a quick_sort_3partition_left) (set! quick_sort_3partition_i quick_sort_3partition_left) (set! quick_sort_3partition_b quick_sort_3partition_right) (set! quick_sort_3partition_pivot (nth quick_sort_3partition_arr quick_sort_3partition_left)) (while (<= quick_sort_3partition_i quick_sort_3partition_b) (if (< (nth quick_sort_3partition_arr quick_sort_3partition_i) quick_sort_3partition_pivot) (do (set! quick_sort_3partition_temp (nth quick_sort_3partition_arr quick_sort_3partition_a)) (set! quick_sort_3partition_arr (assoc quick_sort_3partition_arr quick_sort_3partition_a (nth quick_sort_3partition_arr quick_sort_3partition_i))) (set! quick_sort_3partition_arr (assoc quick_sort_3partition_arr quick_sort_3partition_i quick_sort_3partition_temp)) (set! quick_sort_3partition_a (+ quick_sort_3partition_a 1)) (set! quick_sort_3partition_i (+ quick_sort_3partition_i 1))) (if (> (nth quick_sort_3partition_arr quick_sort_3partition_i) quick_sort_3partition_pivot) (do (set! quick_sort_3partition_temp (nth quick_sort_3partition_arr quick_sort_3partition_b)) (set! quick_sort_3partition_arr (assoc quick_sort_3partition_arr quick_sort_3partition_b (nth quick_sort_3partition_arr quick_sort_3partition_i))) (set! quick_sort_3partition_arr (assoc quick_sort_3partition_arr quick_sort_3partition_i quick_sort_3partition_temp)) (set! quick_sort_3partition_b (- quick_sort_3partition_b 1))) (set! quick_sort_3partition_i (+ quick_sort_3partition_i 1))))) (set! quick_sort_3partition_arr (quick_sort_3partition quick_sort_3partition_arr quick_sort_3partition_left (- quick_sort_3partition_a 1))) (set! quick_sort_3partition_arr (quick_sort_3partition quick_sort_3partition_arr (+ quick_sort_3partition_b 1) quick_sort_3partition_right)) (throw (ex-info "return" {:v quick_sort_3partition_arr}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn quick_sort_lomuto_partition [quick_sort_lomuto_partition_arr_p quick_sort_lomuto_partition_left quick_sort_lomuto_partition_right]
  (binding [quick_sort_lomuto_partition_arr nil quick_sort_lomuto_partition_pivot_index nil] (try (do (set! quick_sort_lomuto_partition_arr quick_sort_lomuto_partition_arr_p) (when (< quick_sort_lomuto_partition_left quick_sort_lomuto_partition_right) (do (set! quick_sort_lomuto_partition_pivot_index (lomuto_partition quick_sort_lomuto_partition_arr quick_sort_lomuto_partition_left quick_sort_lomuto_partition_right)) (set! quick_sort_lomuto_partition_arr (quick_sort_lomuto_partition quick_sort_lomuto_partition_arr quick_sort_lomuto_partition_left (- quick_sort_lomuto_partition_pivot_index 1))) (set! quick_sort_lomuto_partition_arr (quick_sort_lomuto_partition quick_sort_lomuto_partition_arr (+ quick_sort_lomuto_partition_pivot_index 1) quick_sort_lomuto_partition_right)))) (throw (ex-info "return" {:v quick_sort_lomuto_partition_arr}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn lomuto_partition [lomuto_partition_arr_p lomuto_partition_left lomuto_partition_right]
  (binding [lomuto_partition_arr nil lomuto_partition_i nil lomuto_partition_pivot nil lomuto_partition_store_index nil lomuto_partition_temp nil] (try (do (set! lomuto_partition_arr lomuto_partition_arr_p) (set! lomuto_partition_pivot (nth lomuto_partition_arr lomuto_partition_right)) (set! lomuto_partition_store_index lomuto_partition_left) (set! lomuto_partition_i lomuto_partition_left) (while (< lomuto_partition_i lomuto_partition_right) (do (when (< (nth lomuto_partition_arr lomuto_partition_i) lomuto_partition_pivot) (do (set! lomuto_partition_temp (nth lomuto_partition_arr lomuto_partition_store_index)) (set! lomuto_partition_arr (assoc lomuto_partition_arr lomuto_partition_store_index (nth lomuto_partition_arr lomuto_partition_i))) (set! lomuto_partition_arr (assoc lomuto_partition_arr lomuto_partition_i lomuto_partition_temp)) (set! lomuto_partition_store_index (+ lomuto_partition_store_index 1)))) (set! lomuto_partition_i (+ lomuto_partition_i 1)))) (set! lomuto_partition_temp (nth lomuto_partition_arr lomuto_partition_right)) (set! lomuto_partition_arr (assoc lomuto_partition_arr lomuto_partition_right (nth lomuto_partition_arr lomuto_partition_store_index))) (set! lomuto_partition_arr (assoc lomuto_partition_arr lomuto_partition_store_index lomuto_partition_temp)) (throw (ex-info "return" {:v lomuto_partition_store_index}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn three_way_radix_quicksort [three_way_radix_quicksort_arr]
  (binding [three_way_radix_quicksort_equal nil three_way_radix_quicksort_greater nil three_way_radix_quicksort_i nil three_way_radix_quicksort_less nil three_way_radix_quicksort_pivot nil three_way_radix_quicksort_result nil three_way_radix_quicksort_sorted_greater nil three_way_radix_quicksort_sorted_less nil three_way_radix_quicksort_val nil] (try (do (when (<= (count three_way_radix_quicksort_arr) 1) (throw (ex-info "return" {:v three_way_radix_quicksort_arr}))) (set! three_way_radix_quicksort_pivot (nth three_way_radix_quicksort_arr 0)) (set! three_way_radix_quicksort_less []) (set! three_way_radix_quicksort_equal []) (set! three_way_radix_quicksort_greater []) (set! three_way_radix_quicksort_i 0) (while (< three_way_radix_quicksort_i (count three_way_radix_quicksort_arr)) (do (set! three_way_radix_quicksort_val (nth three_way_radix_quicksort_arr three_way_radix_quicksort_i)) (if (< three_way_radix_quicksort_val three_way_radix_quicksort_pivot) (set! three_way_radix_quicksort_less (conj three_way_radix_quicksort_less three_way_radix_quicksort_val)) (if (> three_way_radix_quicksort_val three_way_radix_quicksort_pivot) (set! three_way_radix_quicksort_greater (conj three_way_radix_quicksort_greater three_way_radix_quicksort_val)) (set! three_way_radix_quicksort_equal (conj three_way_radix_quicksort_equal three_way_radix_quicksort_val)))) (set! three_way_radix_quicksort_i (+ three_way_radix_quicksort_i 1)))) (set! three_way_radix_quicksort_sorted_less (three_way_radix_quicksort three_way_radix_quicksort_less)) (set! three_way_radix_quicksort_sorted_greater (three_way_radix_quicksort three_way_radix_quicksort_greater)) (set! three_way_radix_quicksort_result (concat three_way_radix_quicksort_sorted_less three_way_radix_quicksort_equal)) (set! three_way_radix_quicksort_result (concat three_way_radix_quicksort_result three_way_radix_quicksort_sorted_greater)) (throw (ex-info "return" {:v three_way_radix_quicksort_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_array1 [5 (- 1) (- 1) 5 5 24 0])

(def ^:dynamic main_array2 [9 0 2 6])

(def ^:dynamic main_array3 [])

(def ^:dynamic main_nums1 [0 5 3 1 2])

(def ^:dynamic main_nums2 [])

(def ^:dynamic main_nums3 [(- 2) 5 0 (- 4)])

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (def main_array1 (quick_sort_3partition main_array1 0 (- (count main_array1) 1)))
      (println (str main_array1))
      (def main_array2 (quick_sort_3partition main_array2 0 (- (count main_array2) 1)))
      (println (str main_array2))
      (def main_array3 (quick_sort_3partition main_array3 0 (- (count main_array3) 1)))
      (println (str main_array3))
      (def main_nums1 (quick_sort_lomuto_partition main_nums1 0 (- (count main_nums1) 1)))
      (println (str main_nums1))
      (def main_nums2 (quick_sort_lomuto_partition main_nums2 0 (- (count main_nums2) 1)))
      (println (str main_nums2))
      (def main_nums3 (quick_sort_lomuto_partition main_nums3 0 (- (count main_nums3) 1)))
      (println (str main_nums3))
      (println (str (three_way_radix_quicksort [])))
      (println (str (three_way_radix_quicksort [1])))
      (println (str (three_way_radix_quicksort [(- 5) (- 2) 1 (- 2) 0 1])))
      (println (str (three_way_radix_quicksort [1 2 5 1 2 0 0 5 2 (- 1)])))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
