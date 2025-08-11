(ns main (:refer-clojure :exclude [bubble_sort_iterative bubble_sort_recursive copy_list list_eq test_bubble_sort main]))

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

(declare bubble_sort_iterative bubble_sort_recursive copy_list list_eq test_bubble_sort main)

(def ^:dynamic bubble_sort_iterative_collection nil)

(def ^:dynamic bubble_sort_iterative_j nil)

(def ^:dynamic bubble_sort_iterative_n nil)

(def ^:dynamic bubble_sort_iterative_swapped nil)

(def ^:dynamic bubble_sort_iterative_temp nil)

(def ^:dynamic bubble_sort_recursive_collection nil)

(def ^:dynamic bubble_sort_recursive_i nil)

(def ^:dynamic bubble_sort_recursive_n nil)

(def ^:dynamic bubble_sort_recursive_swapped nil)

(def ^:dynamic bubble_sort_recursive_temp nil)

(def ^:dynamic copy_list_i nil)

(def ^:dynamic copy_list_out nil)

(def ^:dynamic list_eq_k nil)

(def ^:dynamic main_arr nil)

(def ^:dynamic test_bubble_sort_empty nil)

(def ^:dynamic test_bubble_sort_example nil)

(def ^:dynamic test_bubble_sort_expected nil)

(defn bubble_sort_iterative [bubble_sort_iterative_collection_p]
  (binding [bubble_sort_iterative_collection nil bubble_sort_iterative_j nil bubble_sort_iterative_n nil bubble_sort_iterative_swapped nil bubble_sort_iterative_temp nil] (try (do (set! bubble_sort_iterative_collection bubble_sort_iterative_collection_p) (set! bubble_sort_iterative_n (count bubble_sort_iterative_collection)) (loop [while_flag_1 true] (when (and while_flag_1 (> bubble_sort_iterative_n 0)) (do (set! bubble_sort_iterative_swapped false) (set! bubble_sort_iterative_j 0) (while (< bubble_sort_iterative_j (- bubble_sort_iterative_n 1)) (do (when (> (nth bubble_sort_iterative_collection bubble_sort_iterative_j) (nth bubble_sort_iterative_collection (+ bubble_sort_iterative_j 1))) (do (set! bubble_sort_iterative_temp (nth bubble_sort_iterative_collection bubble_sort_iterative_j)) (set! bubble_sort_iterative_collection (assoc bubble_sort_iterative_collection bubble_sort_iterative_j (nth bubble_sort_iterative_collection (+ bubble_sort_iterative_j 1)))) (set! bubble_sort_iterative_collection (assoc bubble_sort_iterative_collection (+ bubble_sort_iterative_j 1) bubble_sort_iterative_temp)) (set! bubble_sort_iterative_swapped true))) (set! bubble_sort_iterative_j (+ bubble_sort_iterative_j 1)))) (cond (not bubble_sort_iterative_swapped) (recur false) :else (do (set! bubble_sort_iterative_n (- bubble_sort_iterative_n 1)) (recur while_flag_1)))))) (throw (ex-info "return" {:v bubble_sort_iterative_collection}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn bubble_sort_recursive [bubble_sort_recursive_collection_p]
  (binding [bubble_sort_recursive_collection nil bubble_sort_recursive_i nil bubble_sort_recursive_n nil bubble_sort_recursive_swapped nil bubble_sort_recursive_temp nil] (try (do (set! bubble_sort_recursive_collection bubble_sort_recursive_collection_p) (set! bubble_sort_recursive_n (count bubble_sort_recursive_collection)) (set! bubble_sort_recursive_swapped false) (set! bubble_sort_recursive_i 0) (while (< bubble_sort_recursive_i (- bubble_sort_recursive_n 1)) (do (when (> (nth bubble_sort_recursive_collection bubble_sort_recursive_i) (nth bubble_sort_recursive_collection (+ bubble_sort_recursive_i 1))) (do (set! bubble_sort_recursive_temp (nth bubble_sort_recursive_collection bubble_sort_recursive_i)) (set! bubble_sort_recursive_collection (assoc bubble_sort_recursive_collection bubble_sort_recursive_i (nth bubble_sort_recursive_collection (+ bubble_sort_recursive_i 1)))) (set! bubble_sort_recursive_collection (assoc bubble_sort_recursive_collection (+ bubble_sort_recursive_i 1) bubble_sort_recursive_temp)) (set! bubble_sort_recursive_swapped true))) (set! bubble_sort_recursive_i (+ bubble_sort_recursive_i 1)))) (if bubble_sort_recursive_swapped (bubble_sort_recursive bubble_sort_recursive_collection) bubble_sort_recursive_collection)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn copy_list [copy_list_xs]
  (binding [copy_list_i nil copy_list_out nil] (try (do (set! copy_list_out []) (set! copy_list_i 0) (while (< copy_list_i (count copy_list_xs)) (do (set! copy_list_out (conj copy_list_out (nth copy_list_xs copy_list_i))) (set! copy_list_i (+ copy_list_i 1)))) (throw (ex-info "return" {:v copy_list_out}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn list_eq [list_eq_a list_eq_b]
  (binding [list_eq_k nil] (try (do (when (not= (count list_eq_a) (count list_eq_b)) (throw (ex-info "return" {:v false}))) (set! list_eq_k 0) (while (< list_eq_k (count list_eq_a)) (do (when (not= (nth list_eq_a list_eq_k) (nth list_eq_b list_eq_k)) (throw (ex-info "return" {:v false}))) (set! list_eq_k (+ list_eq_k 1)))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn test_bubble_sort []
  (binding [test_bubble_sort_empty nil test_bubble_sort_example nil test_bubble_sort_expected nil] (do (set! test_bubble_sort_example [0 5 2 3 2]) (set! test_bubble_sort_expected [0 2 2 3 5]) (when (not (list_eq (bubble_sort_iterative (copy_list test_bubble_sort_example)) test_bubble_sort_expected)) (throw (Exception. "iterative failed"))) (when (not (list_eq (bubble_sort_recursive (copy_list test_bubble_sort_example)) test_bubble_sort_expected)) (throw (Exception. "recursive failed"))) (set! test_bubble_sort_empty []) (when (not= (count (bubble_sort_iterative (copy_list test_bubble_sort_empty))) 0) (throw (Exception. "empty iterative failed"))) (when (not= (count (bubble_sort_recursive (copy_list test_bubble_sort_empty))) 0) (throw (Exception. "empty recursive failed"))))))

(defn main []
  (binding [main_arr nil] (do (test_bubble_sort) (set! main_arr [5 1 4 2 8]) (println (str (bubble_sort_iterative (copy_list main_arr)))) (println (str (bubble_sort_recursive (copy_list main_arr)))))))

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
