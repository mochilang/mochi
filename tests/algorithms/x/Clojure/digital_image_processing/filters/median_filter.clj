(ns main (:refer-clojure :exclude [insertion_sort median_filter main]))

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

(declare insertion_sort median_filter main)

(def ^:dynamic insertion_sort_a nil)

(def ^:dynamic insertion_sort_i nil)

(def ^:dynamic insertion_sort_j nil)

(def ^:dynamic insertion_sort_key nil)

(def ^:dynamic main_filtered nil)

(def ^:dynamic main_img nil)

(def ^:dynamic median_filter_bd nil)

(def ^:dynamic median_filter_cols nil)

(def ^:dynamic median_filter_i nil)

(def ^:dynamic median_filter_idx nil)

(def ^:dynamic median_filter_j nil)

(def ^:dynamic median_filter_kernel nil)

(def ^:dynamic median_filter_result nil)

(def ^:dynamic median_filter_row nil)

(def ^:dynamic median_filter_rows nil)

(def ^:dynamic median_filter_x nil)

(def ^:dynamic median_filter_y nil)

(defn insertion_sort [insertion_sort_a_p]
  (binding [insertion_sort_a nil insertion_sort_i nil insertion_sort_j nil insertion_sort_key nil] (try (do (set! insertion_sort_a insertion_sort_a_p) (set! insertion_sort_i 1) (while (< insertion_sort_i (count insertion_sort_a)) (do (set! insertion_sort_key (nth insertion_sort_a insertion_sort_i)) (set! insertion_sort_j (- insertion_sort_i 1)) (while (and (>= insertion_sort_j 0) (> (nth insertion_sort_a insertion_sort_j) insertion_sort_key)) (do (set! insertion_sort_a (assoc insertion_sort_a (+ insertion_sort_j 1) (nth insertion_sort_a insertion_sort_j))) (set! insertion_sort_j (- insertion_sort_j 1)))) (set! insertion_sort_a (assoc insertion_sort_a (+ insertion_sort_j 1) insertion_sort_key)) (set! insertion_sort_i (+ insertion_sort_i 1)))) (throw (ex-info "return" {:v insertion_sort_a}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn median_filter [median_filter_gray_img median_filter_mask]
  (binding [median_filter_bd nil median_filter_cols nil median_filter_i nil median_filter_idx nil median_filter_j nil median_filter_kernel nil median_filter_result nil median_filter_row nil median_filter_rows nil median_filter_x nil median_filter_y nil] (try (do (set! median_filter_rows (count median_filter_gray_img)) (set! median_filter_cols (count (nth median_filter_gray_img 0))) (set! median_filter_bd (quot median_filter_mask 2)) (set! median_filter_result []) (set! median_filter_i 0) (while (< median_filter_i median_filter_rows) (do (set! median_filter_row []) (set! median_filter_j 0) (while (< median_filter_j median_filter_cols) (do (set! median_filter_row (conj median_filter_row 0)) (set! median_filter_j (+ median_filter_j 1)))) (set! median_filter_result (conj median_filter_result median_filter_row)) (set! median_filter_i (+ median_filter_i 1)))) (set! median_filter_i median_filter_bd) (while (< median_filter_i (- median_filter_rows median_filter_bd)) (do (set! median_filter_j median_filter_bd) (while (< median_filter_j (- median_filter_cols median_filter_bd)) (do (set! median_filter_kernel []) (set! median_filter_x (- median_filter_i median_filter_bd)) (while (<= median_filter_x (+ median_filter_i median_filter_bd)) (do (set! median_filter_y (- median_filter_j median_filter_bd)) (while (<= median_filter_y (+ median_filter_j median_filter_bd)) (do (set! median_filter_kernel (conj median_filter_kernel (nth (nth median_filter_gray_img median_filter_x) median_filter_y))) (set! median_filter_y (+ median_filter_y 1)))) (set! median_filter_x (+ median_filter_x 1)))) (set! median_filter_kernel (insertion_sort median_filter_kernel)) (set! median_filter_idx (quot (* median_filter_mask median_filter_mask) 2)) (set! median_filter_result (assoc-in median_filter_result [median_filter_i median_filter_j] (nth median_filter_kernel median_filter_idx))) (set! median_filter_j (+ median_filter_j 1)))) (set! median_filter_i (+ median_filter_i 1)))) (throw (ex-info "return" {:v median_filter_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_filtered nil main_img nil] (do (set! main_img [[10 10 10 10 10] [10 255 10 255 10] [10 10 10 10 10] [10 255 10 255 10] [10 10 10 10 10]]) (set! main_filtered (median_filter main_img 3)) (println main_filtered))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (main)
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
