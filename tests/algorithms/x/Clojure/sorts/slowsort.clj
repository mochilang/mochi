(ns main (:refer-clojure :exclude [swap slowsort_recursive slow_sort]))

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

(declare swap slowsort_recursive slow_sort)

(def ^:dynamic slowsort_recursive_mid nil)

(def ^:dynamic swap_seq nil)

(def ^:dynamic swap_temp nil)

(defn swap [swap_seq_p swap_i swap_j]
  (binding [swap_seq nil swap_temp nil] (do (set! swap_seq swap_seq_p) (set! swap_temp (nth swap_seq swap_i)) (set! swap_seq (assoc swap_seq swap_i (nth swap_seq swap_j))) (set! swap_seq (assoc swap_seq swap_j swap_temp)))))

(defn slowsort_recursive [slowsort_recursive_seq slowsort_recursive_start slowsort_recursive_end_index]
  (binding [slowsort_recursive_mid nil] (try (do (when (>= slowsort_recursive_start slowsort_recursive_end_index) (throw (ex-info "return" {:v nil}))) (set! slowsort_recursive_mid (/ (+ slowsort_recursive_start slowsort_recursive_end_index) 2)) (slowsort_recursive slowsort_recursive_seq slowsort_recursive_start slowsort_recursive_mid) (slowsort_recursive slowsort_recursive_seq (+ slowsort_recursive_mid 1) slowsort_recursive_end_index) (when (< (nth slowsort_recursive_seq slowsort_recursive_end_index) (nth slowsort_recursive_seq slowsort_recursive_mid)) (swap slowsort_recursive_seq slowsort_recursive_end_index slowsort_recursive_mid)) (slowsort_recursive slowsort_recursive_seq slowsort_recursive_start (- slowsort_recursive_end_index 1))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn slow_sort [slow_sort_seq]
  (try (do (when (> (count slow_sort_seq) 0) (slowsort_recursive slow_sort_seq 0 (- (count slow_sort_seq) 1))) (throw (ex-info "return" {:v slow_sort_seq}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(def ^:dynamic main_seq1 [1 6 2 5 3 4 4 5])

(def ^:dynamic main_seq2 [])

(def ^:dynamic main_seq3 [2])

(def ^:dynamic main_seq4 [1 2 3 4])

(def ^:dynamic main_seq5 [4 3 2 1])

(def ^:dynamic main_seq6 [9 8 7 6 5 4 3 2 1 0])

(def ^:dynamic main_seq7 [9 8 7 6 5 4 3 2 1 0])

(def ^:dynamic main_seq8 [9 8 7 6 5 4 3 2 1 0])

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (slow_sort main_seq1)))
      (println (str (slow_sort main_seq2)))
      (println (str (slow_sort main_seq3)))
      (println (str (slow_sort main_seq4)))
      (println (str (slow_sort main_seq5)))
      (slowsort_recursive main_seq6 2 7)
      (println (str main_seq6))
      (slowsort_recursive main_seq7 0 4)
      (println (str main_seq7))
      (slowsort_recursive main_seq8 5 (- (count main_seq8) 1))
      (println (str main_seq8))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
