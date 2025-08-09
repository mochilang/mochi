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

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare bubble_sort median)

(def ^:dynamic bubble_sort_a nil)

(def ^:dynamic bubble_sort_arr nil)

(def ^:dynamic bubble_sort_b nil)

(def ^:dynamic bubble_sort_i nil)

(def ^:dynamic bubble_sort_j nil)

(def ^:dynamic bubble_sort_n nil)

(def ^:dynamic median_length nil)

(def ^:dynamic median_mid_index nil)

(def ^:dynamic median_sorted_list nil)

(defn bubble_sort [bubble_sort_nums]
  (binding [bubble_sort_a nil bubble_sort_arr nil bubble_sort_b nil bubble_sort_i nil bubble_sort_j nil bubble_sort_n nil] (try (do (set! bubble_sort_arr bubble_sort_nums) (set! bubble_sort_n (count bubble_sort_arr)) (set! bubble_sort_i 0) (while (< bubble_sort_i bubble_sort_n) (do (set! bubble_sort_j 0) (while (< bubble_sort_j (- bubble_sort_n 1)) (do (set! bubble_sort_a (nth bubble_sort_arr bubble_sort_j)) (set! bubble_sort_b (nth bubble_sort_arr (+ bubble_sort_j 1))) (when (> bubble_sort_a bubble_sort_b) (do (set! bubble_sort_arr (assoc bubble_sort_arr bubble_sort_j bubble_sort_b)) (set! bubble_sort_arr (assoc bubble_sort_arr (+ bubble_sort_j 1) bubble_sort_a)))) (set! bubble_sort_j (+ bubble_sort_j 1)))) (set! bubble_sort_i (+ bubble_sort_i 1)))) (throw (ex-info "return" {:v bubble_sort_arr}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn median [median_nums]
  (binding [median_length nil median_mid_index nil median_sorted_list nil] (try (do (set! median_sorted_list (bubble_sort median_nums)) (set! median_length (count median_sorted_list)) (set! median_mid_index (quot median_length 2)) (if (= (mod median_length 2) 0) (throw (ex-info "return" {:v (/ (double (+ (nth median_sorted_list median_mid_index) (nth median_sorted_list (- median_mid_index 1)))) 2.0)})) (throw (ex-info "return" {:v (double (nth median_sorted_list median_mid_index))})))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (median [0])))
      (println (str (median [4 1 3 2])))
      (println (str (median [2 70 6 50 20 8 4])))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
