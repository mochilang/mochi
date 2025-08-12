(ns main (:refer-clojure :exclude [bubble_sort median]))

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

(declare bubble_sort median)

(def ^:dynamic bubble_sort_arr nil)

(def ^:dynamic bubble_sort_i nil)

(def ^:dynamic bubble_sort_j nil)

(def ^:dynamic bubble_sort_n nil)

(def ^:dynamic bubble_sort_temp nil)

(def ^:dynamic median_i nil)

(def ^:dynamic median_j nil)

(def ^:dynamic median_linear nil)

(def ^:dynamic median_mid nil)

(def ^:dynamic median_row nil)

(def ^:dynamic median_sorted nil)

(defn bubble_sort [bubble_sort_a]
  (binding [bubble_sort_arr nil bubble_sort_i nil bubble_sort_j nil bubble_sort_n nil bubble_sort_temp nil] (try (do (set! bubble_sort_arr bubble_sort_a) (set! bubble_sort_n (count bubble_sort_arr)) (set! bubble_sort_i 0) (while (< bubble_sort_i bubble_sort_n) (do (set! bubble_sort_j 0) (while (< (+ bubble_sort_j 1) (- bubble_sort_n bubble_sort_i)) (do (when (> (nth bubble_sort_arr bubble_sort_j) (nth bubble_sort_arr (+ bubble_sort_j 1))) (do (set! bubble_sort_temp (nth bubble_sort_arr bubble_sort_j)) (set! bubble_sort_arr (assoc bubble_sort_arr bubble_sort_j (nth bubble_sort_arr (+ bubble_sort_j 1)))) (set! bubble_sort_arr (assoc bubble_sort_arr (+ bubble_sort_j 1) bubble_sort_temp)))) (set! bubble_sort_j (+ bubble_sort_j 1)))) (set! bubble_sort_i (+ bubble_sort_i 1)))) (throw (ex-info "return" {:v bubble_sort_arr}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn median [median_matrix]
  (binding [median_i nil median_j nil median_linear nil median_mid nil median_row nil median_sorted nil] (try (do (set! median_linear []) (set! median_i 0) (while (< median_i (count median_matrix)) (do (set! median_row (nth median_matrix median_i)) (set! median_j 0) (while (< median_j (count median_row)) (do (set! median_linear (conj median_linear (nth median_row median_j))) (set! median_j (+ median_j 1)))) (set! median_i (+ median_i 1)))) (set! median_sorted (bubble_sort median_linear)) (set! median_mid (/ (- (count median_sorted) 1) 2)) (throw (ex-info "return" {:v (nth median_sorted median_mid)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_matrix1 [[1 3 5] [2 6 9] [3 6 9]])

(def ^:dynamic main_matrix2 [[1 2 3] [4 5 6]])

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (median main_matrix1)))
      (println (str (median main_matrix2)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
