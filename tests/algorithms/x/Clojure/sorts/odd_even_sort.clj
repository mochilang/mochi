(ns main (:refer-clojure :exclude [odd_even_sort print_list test_odd_even_sort main]))

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

(declare odd_even_sort print_list test_odd_even_sort main)

(def ^:dynamic main_sample nil)

(def ^:dynamic main_sorted nil)

(def ^:dynamic odd_even_sort_arr nil)

(def ^:dynamic odd_even_sort_i nil)

(def ^:dynamic odd_even_sort_j nil)

(def ^:dynamic odd_even_sort_n nil)

(def ^:dynamic odd_even_sort_sorted nil)

(def ^:dynamic odd_even_sort_tmp nil)

(def ^:dynamic print_list_i nil)

(def ^:dynamic print_list_out nil)

(def ^:dynamic test_odd_even_sort_a nil)

(def ^:dynamic test_odd_even_sort_b nil)

(def ^:dynamic test_odd_even_sort_c nil)

(def ^:dynamic test_odd_even_sort_d nil)

(def ^:dynamic test_odd_even_sort_r1 nil)

(def ^:dynamic test_odd_even_sort_r2 nil)

(def ^:dynamic test_odd_even_sort_r3 nil)

(def ^:dynamic test_odd_even_sort_r4 nil)

(defn odd_even_sort [odd_even_sort_xs]
  (binding [odd_even_sort_arr nil odd_even_sort_i nil odd_even_sort_j nil odd_even_sort_n nil odd_even_sort_sorted nil odd_even_sort_tmp nil] (try (do (set! odd_even_sort_arr []) (set! odd_even_sort_i 0) (while (< odd_even_sort_i (count odd_even_sort_xs)) (do (set! odd_even_sort_arr (conj odd_even_sort_arr (nth odd_even_sort_xs odd_even_sort_i))) (set! odd_even_sort_i (+ odd_even_sort_i 1)))) (set! odd_even_sort_n (count odd_even_sort_arr)) (set! odd_even_sort_sorted false) (while (= odd_even_sort_sorted false) (do (set! odd_even_sort_sorted true) (set! odd_even_sort_j 0) (while (< odd_even_sort_j (- odd_even_sort_n 1)) (do (when (> (nth odd_even_sort_arr odd_even_sort_j) (nth odd_even_sort_arr (+ odd_even_sort_j 1))) (do (set! odd_even_sort_tmp (nth odd_even_sort_arr odd_even_sort_j)) (set! odd_even_sort_arr (assoc odd_even_sort_arr odd_even_sort_j (nth odd_even_sort_arr (+ odd_even_sort_j 1)))) (set! odd_even_sort_arr (assoc odd_even_sort_arr (+ odd_even_sort_j 1) odd_even_sort_tmp)) (set! odd_even_sort_sorted false))) (set! odd_even_sort_j (+ odd_even_sort_j 2)))) (set! odd_even_sort_j 1) (while (< odd_even_sort_j (- odd_even_sort_n 1)) (do (when (> (nth odd_even_sort_arr odd_even_sort_j) (nth odd_even_sort_arr (+ odd_even_sort_j 1))) (do (set! odd_even_sort_tmp (nth odd_even_sort_arr odd_even_sort_j)) (set! odd_even_sort_arr (assoc odd_even_sort_arr odd_even_sort_j (nth odd_even_sort_arr (+ odd_even_sort_j 1)))) (set! odd_even_sort_arr (assoc odd_even_sort_arr (+ odd_even_sort_j 1) odd_even_sort_tmp)) (set! odd_even_sort_sorted false))) (set! odd_even_sort_j (+ odd_even_sort_j 2)))))) (throw (ex-info "return" {:v odd_even_sort_arr}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn print_list [print_list_xs]
  (binding [print_list_i nil print_list_out nil] (do (set! print_list_i 0) (set! print_list_out "") (while (< print_list_i (count print_list_xs)) (do (when (> print_list_i 0) (set! print_list_out (str print_list_out " "))) (set! print_list_out (str print_list_out (str (nth print_list_xs print_list_i)))) (set! print_list_i (+ print_list_i 1)))) (println print_list_out) print_list_xs)))

(defn test_odd_even_sort []
  (binding [test_odd_even_sort_a nil test_odd_even_sort_b nil test_odd_even_sort_c nil test_odd_even_sort_d nil test_odd_even_sort_r1 nil test_odd_even_sort_r2 nil test_odd_even_sort_r3 nil test_odd_even_sort_r4 nil] (do (set! test_odd_even_sort_a [5 4 3 2 1]) (set! test_odd_even_sort_r1 (odd_even_sort test_odd_even_sort_a)) (when (or (or (or (or (not= (nth test_odd_even_sort_r1 0) 1) (not= (nth test_odd_even_sort_r1 1) 2)) (not= (nth test_odd_even_sort_r1 2) 3)) (not= (nth test_odd_even_sort_r1 3) 4)) (not= (nth test_odd_even_sort_r1 4) 5)) (throw (Exception. "case1 failed"))) (set! test_odd_even_sort_b []) (set! test_odd_even_sort_r2 (odd_even_sort test_odd_even_sort_b)) (when (not= (count test_odd_even_sort_r2) 0) (throw (Exception. "case2 failed"))) (set! test_odd_even_sort_c [(- 10) (- 1) 10 2]) (set! test_odd_even_sort_r3 (odd_even_sort test_odd_even_sort_c)) (when (or (or (or (not= (nth test_odd_even_sort_r3 0) (- 10)) (not= (nth test_odd_even_sort_r3 1) (- 1))) (not= (nth test_odd_even_sort_r3 2) 2)) (not= (nth test_odd_even_sort_r3 3) 10)) (throw (Exception. "case3 failed"))) (set! test_odd_even_sort_d [1 2 3 4]) (set! test_odd_even_sort_r4 (odd_even_sort test_odd_even_sort_d)) (when (or (or (or (not= (nth test_odd_even_sort_r4 0) 1) (not= (nth test_odd_even_sort_r4 1) 2)) (not= (nth test_odd_even_sort_r4 2) 3)) (not= (nth test_odd_even_sort_r4 3) 4)) (throw (Exception. "case4 failed"))))))

(defn main []
  (binding [main_sample nil main_sorted nil] (do (test_odd_even_sort) (set! main_sample [5 4 3 2 1]) (set! main_sorted (odd_even_sort main_sample)) (print_list main_sorted))))

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
