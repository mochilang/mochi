(ns main (:refer-clojure :exclude [bubble_sort factors sum_list abundant semi_perfect weird run_tests main]))

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

(declare bubble_sort factors sum_list abundant semi_perfect weird run_tests main)

(def ^:dynamic bubble_sort_arr nil)

(def ^:dynamic bubble_sort_i nil)

(def ^:dynamic bubble_sort_j nil)

(def ^:dynamic bubble_sort_n nil)

(def ^:dynamic bubble_sort_tmp nil)

(def ^:dynamic factors_d nil)

(def ^:dynamic factors_i nil)

(def ^:dynamic factors_values nil)

(def ^:dynamic main_i nil)

(def ^:dynamic main_n nil)

(def ^:dynamic main_nums nil)

(def ^:dynamic semi_perfect_idx nil)

(def ^:dynamic semi_perfect_j nil)

(def ^:dynamic semi_perfect_possible nil)

(def ^:dynamic semi_perfect_s nil)

(def ^:dynamic semi_perfect_v nil)

(def ^:dynamic semi_perfect_values nil)

(def ^:dynamic sum_list_i nil)

(def ^:dynamic sum_list_total nil)

(defn bubble_sort [bubble_sort_xs]
  (binding [bubble_sort_arr nil bubble_sort_i nil bubble_sort_j nil bubble_sort_n nil bubble_sort_tmp nil] (try (do (set! bubble_sort_arr bubble_sort_xs) (set! bubble_sort_n (count bubble_sort_arr)) (set! bubble_sort_i 0) (while (< bubble_sort_i bubble_sort_n) (do (set! bubble_sort_j 0) (while (< bubble_sort_j (- (- bubble_sort_n bubble_sort_i) 1)) (do (when (> (nth bubble_sort_arr bubble_sort_j) (nth bubble_sort_arr (+ bubble_sort_j 1))) (do (set! bubble_sort_tmp (nth bubble_sort_arr bubble_sort_j)) (set! bubble_sort_arr (assoc bubble_sort_arr bubble_sort_j (nth bubble_sort_arr (+ bubble_sort_j 1)))) (set! bubble_sort_arr (assoc bubble_sort_arr (+ bubble_sort_j 1) bubble_sort_tmp)))) (set! bubble_sort_j (+ bubble_sort_j 1)))) (set! bubble_sort_i (+ bubble_sort_i 1)))) (throw (ex-info "return" {:v bubble_sort_arr}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn factors [factors_num]
  (binding [factors_d nil factors_i nil factors_values nil] (try (do (set! factors_values [1]) (set! factors_i 2) (while (<= (* factors_i factors_i) factors_num) (do (when (= (mod factors_num factors_i) 0) (do (set! factors_values (conj factors_values factors_i)) (set! factors_d (quot factors_num factors_i)) (when (not= factors_d factors_i) (set! factors_values (conj factors_values factors_d))))) (set! factors_i (+ factors_i 1)))) (throw (ex-info "return" {:v (bubble_sort factors_values)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn sum_list [sum_list_xs]
  (binding [sum_list_i nil sum_list_total nil] (try (do (set! sum_list_total 0) (set! sum_list_i 0) (while (< sum_list_i (count sum_list_xs)) (do (set! sum_list_total (+ sum_list_total (nth sum_list_xs sum_list_i))) (set! sum_list_i (+ sum_list_i 1)))) (throw (ex-info "return" {:v sum_list_total}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn abundant [abundant_n]
  (try (throw (ex-info "return" {:v (> (sum_list (factors abundant_n)) abundant_n)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn semi_perfect [semi_perfect_number]
  (binding [semi_perfect_idx nil semi_perfect_j nil semi_perfect_possible nil semi_perfect_s nil semi_perfect_v nil semi_perfect_values nil] (try (do (when (<= semi_perfect_number 0) (throw (ex-info "return" {:v true}))) (set! semi_perfect_values (factors semi_perfect_number)) (set! semi_perfect_possible []) (set! semi_perfect_j 0) (while (<= semi_perfect_j semi_perfect_number) (do (set! semi_perfect_possible (conj semi_perfect_possible (= semi_perfect_j 0))) (set! semi_perfect_j (+ semi_perfect_j 1)))) (set! semi_perfect_idx 0) (while (< semi_perfect_idx (count semi_perfect_values)) (do (set! semi_perfect_v (nth semi_perfect_values semi_perfect_idx)) (set! semi_perfect_s semi_perfect_number) (while (>= semi_perfect_s semi_perfect_v) (do (when (nth semi_perfect_possible (- semi_perfect_s semi_perfect_v)) (set! semi_perfect_possible (assoc semi_perfect_possible semi_perfect_s true))) (set! semi_perfect_s (- semi_perfect_s 1)))) (set! semi_perfect_idx (+ semi_perfect_idx 1)))) (throw (ex-info "return" {:v (nth semi_perfect_possible semi_perfect_number)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn weird [weird_number]
  (try (throw (ex-info "return" {:v (and (abundant weird_number) (= (semi_perfect weird_number) false))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn run_tests []
  (do (when (not= (factors 12) [1 2 3 4 6]) (throw (Exception. "factors 12 failed"))) (when (not= (factors 1) [1]) (throw (Exception. "factors 1 failed"))) (when (not= (factors 100) [1 2 4 5 10 20 25 50]) (throw (Exception. "factors 100 failed"))) (when (not= (abundant 0) true) (throw (Exception. "abundant 0 failed"))) (when (not= (abundant 1) false) (throw (Exception. "abundant 1 failed"))) (when (not= (abundant 12) true) (throw (Exception. "abundant 12 failed"))) (when (not= (abundant 13) false) (throw (Exception. "abundant 13 failed"))) (when (not= (abundant 20) true) (throw (Exception. "abundant 20 failed"))) (when (not= (semi_perfect 0) true) (throw (Exception. "semi_perfect 0 failed"))) (when (not= (semi_perfect 1) true) (throw (Exception. "semi_perfect 1 failed"))) (when (not= (semi_perfect 12) true) (throw (Exception. "semi_perfect 12 failed"))) (when (not= (semi_perfect 13) false) (throw (Exception. "semi_perfect 13 failed"))) (when (not= (weird 0) false) (throw (Exception. "weird 0 failed"))) (when (not= (weird 70) true) (throw (Exception. "weird 70 failed"))) (when (not= (weird 77) false) (throw (Exception. "weird 77 failed")))))

(defn main []
  (binding [main_i nil main_n nil main_nums nil] (do (run_tests) (set! main_nums [69 70 71]) (set! main_i 0) (while (< main_i (count main_nums)) (do (set! main_n (nth main_nums main_i)) (if (weird main_n) (println (str (str main_n) " is weird.")) (println (str (str main_n) " is not weird."))) (set! main_i (+ main_i 1)))))))

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
