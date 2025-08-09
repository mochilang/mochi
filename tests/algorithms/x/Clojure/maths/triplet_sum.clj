(ns main (:refer-clojure :exclude [bubble_sort sort3 triplet_sum1 triplet_sum2 list_equal test_triplet_sum main]))

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

(declare bubble_sort sort3 triplet_sum1 triplet_sum2 list_equal test_triplet_sum main)

(def ^:dynamic bubble_sort_a nil)

(def ^:dynamic bubble_sort_arr nil)

(def ^:dynamic bubble_sort_b nil)

(def ^:dynamic bubble_sort_i nil)

(def ^:dynamic bubble_sort_n nil)

(def ^:dynamic bubble_sort_tmp nil)

(def ^:dynamic list_equal_i nil)

(def ^:dynamic main_res nil)

(def ^:dynamic main_sample nil)

(def ^:dynamic sort3_a nil)

(def ^:dynamic sort3_arr nil)

(def ^:dynamic sort3_b nil)

(def ^:dynamic sort3_i nil)

(def ^:dynamic sort3_n nil)

(def ^:dynamic sort3_tmp nil)

(def ^:dynamic test_triplet_sum_arr1 nil)

(def ^:dynamic test_triplet_sum_arr2 nil)

(def ^:dynamic test_triplet_sum_arr3 nil)

(def ^:dynamic triplet_sum1_i nil)

(def ^:dynamic triplet_sum1_j nil)

(def ^:dynamic triplet_sum1_k nil)

(def ^:dynamic triplet_sum2_i nil)

(def ^:dynamic triplet_sum2_left nil)

(def ^:dynamic triplet_sum2_n nil)

(def ^:dynamic triplet_sum2_right nil)

(def ^:dynamic triplet_sum2_s nil)

(def ^:dynamic triplet_sum2_sorted nil)

(defn bubble_sort [bubble_sort_nums]
  (binding [bubble_sort_a nil bubble_sort_arr nil bubble_sort_b nil bubble_sort_i nil bubble_sort_n nil bubble_sort_tmp nil] (try (do (set! bubble_sort_arr []) (set! bubble_sort_i 0) (while (< bubble_sort_i (count bubble_sort_nums)) (do (set! bubble_sort_arr (conj bubble_sort_arr (nth bubble_sort_nums bubble_sort_i))) (set! bubble_sort_i (+ bubble_sort_i 1)))) (set! bubble_sort_n (count bubble_sort_arr)) (set! bubble_sort_a 0) (while (< bubble_sort_a bubble_sort_n) (do (set! bubble_sort_b 0) (while (< bubble_sort_b (- (- bubble_sort_n bubble_sort_a) 1)) (do (when (> (nth bubble_sort_arr bubble_sort_b) (nth bubble_sort_arr (+ bubble_sort_b 1))) (do (set! bubble_sort_tmp (nth bubble_sort_arr bubble_sort_b)) (set! bubble_sort_arr (assoc bubble_sort_arr bubble_sort_b (nth bubble_sort_arr (+ bubble_sort_b 1)))) (set! bubble_sort_arr (assoc bubble_sort_arr (+ bubble_sort_b 1) bubble_sort_tmp)))) (set! bubble_sort_b (+ bubble_sort_b 1)))) (set! bubble_sort_a (+ bubble_sort_a 1)))) (throw (ex-info "return" {:v bubble_sort_arr}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn sort3 [sort3_xs]
  (binding [sort3_a nil sort3_arr nil sort3_b nil sort3_i nil sort3_n nil sort3_tmp nil] (try (do (set! sort3_arr []) (set! sort3_i 0) (while (< sort3_i (count sort3_xs)) (do (set! sort3_arr (conj sort3_arr (nth sort3_xs sort3_i))) (set! sort3_i (+ sort3_i 1)))) (set! sort3_n (count sort3_arr)) (set! sort3_a 0) (while (< sort3_a sort3_n) (do (set! sort3_b 0) (while (< sort3_b (- (- sort3_n sort3_a) 1)) (do (when (> (nth sort3_arr sort3_b) (nth sort3_arr (+ sort3_b 1))) (do (set! sort3_tmp (nth sort3_arr sort3_b)) (set! sort3_arr (assoc sort3_arr sort3_b (nth sort3_arr (+ sort3_b 1)))) (set! sort3_arr (assoc sort3_arr (+ sort3_b 1) sort3_tmp)))) (set! sort3_b (+ sort3_b 1)))) (set! sort3_a (+ sort3_a 1)))) (throw (ex-info "return" {:v sort3_arr}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn triplet_sum1 [triplet_sum1_arr triplet_sum1_target]
  (binding [triplet_sum1_i nil triplet_sum1_j nil triplet_sum1_k nil] (try (do (set! triplet_sum1_i 0) (while (< triplet_sum1_i (- (count triplet_sum1_arr) 2)) (do (set! triplet_sum1_j (+ triplet_sum1_i 1)) (while (< triplet_sum1_j (- (count triplet_sum1_arr) 1)) (do (set! triplet_sum1_k (+ triplet_sum1_j 1)) (while (< triplet_sum1_k (count triplet_sum1_arr)) (do (when (= (+ (+ (nth triplet_sum1_arr triplet_sum1_i) (nth triplet_sum1_arr triplet_sum1_j)) (nth triplet_sum1_arr triplet_sum1_k)) triplet_sum1_target) (throw (ex-info "return" {:v (sort3 [(nth triplet_sum1_arr triplet_sum1_i) (nth triplet_sum1_arr triplet_sum1_j) (nth triplet_sum1_arr triplet_sum1_k)])}))) (set! triplet_sum1_k (+ triplet_sum1_k 1)))) (set! triplet_sum1_j (+ triplet_sum1_j 1)))) (set! triplet_sum1_i (+ triplet_sum1_i 1)))) (throw (ex-info "return" {:v [0 0 0]}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn triplet_sum2 [triplet_sum2_arr triplet_sum2_target]
  (binding [triplet_sum2_i nil triplet_sum2_left nil triplet_sum2_n nil triplet_sum2_right nil triplet_sum2_s nil triplet_sum2_sorted nil] (try (do (set! triplet_sum2_sorted (bubble_sort triplet_sum2_arr)) (set! triplet_sum2_n (count triplet_sum2_sorted)) (set! triplet_sum2_i 0) (while (< triplet_sum2_i (- triplet_sum2_n 2)) (do (set! triplet_sum2_left (+ triplet_sum2_i 1)) (set! triplet_sum2_right (- triplet_sum2_n 1)) (while (< triplet_sum2_left triplet_sum2_right) (do (set! triplet_sum2_s (+ (+ (nth triplet_sum2_sorted triplet_sum2_i) (nth triplet_sum2_sorted triplet_sum2_left)) (nth triplet_sum2_sorted triplet_sum2_right))) (when (= triplet_sum2_s triplet_sum2_target) (throw (ex-info "return" {:v [(nth triplet_sum2_sorted triplet_sum2_i) (nth triplet_sum2_sorted triplet_sum2_left) (nth triplet_sum2_sorted triplet_sum2_right)]}))) (if (< triplet_sum2_s triplet_sum2_target) (set! triplet_sum2_left (+ triplet_sum2_left 1)) (set! triplet_sum2_right (- triplet_sum2_right 1))))) (set! triplet_sum2_i (+ triplet_sum2_i 1)))) (throw (ex-info "return" {:v [0 0 0]}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn list_equal [list_equal_a list_equal_b]
  (binding [list_equal_i nil] (try (do (when (not= (count list_equal_a) (count list_equal_b)) (throw (ex-info "return" {:v false}))) (set! list_equal_i 0) (while (< list_equal_i (count list_equal_a)) (do (when (not= (nth list_equal_a list_equal_i) (nth list_equal_b list_equal_i)) (throw (ex-info "return" {:v false}))) (set! list_equal_i (+ list_equal_i 1)))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn test_triplet_sum []
  (binding [test_triplet_sum_arr1 nil test_triplet_sum_arr2 nil test_triplet_sum_arr3 nil] (do (set! test_triplet_sum_arr1 [13 29 7 23 5]) (when (not (list_equal (triplet_sum1 test_triplet_sum_arr1 35) [5 7 23])) (throw (Exception. "ts1 case1 failed"))) (when (not (list_equal (triplet_sum2 test_triplet_sum_arr1 35) [5 7 23])) (throw (Exception. "ts2 case1 failed"))) (set! test_triplet_sum_arr2 [37 9 19 50 44]) (when (not (list_equal (triplet_sum1 test_triplet_sum_arr2 65) [9 19 37])) (throw (Exception. "ts1 case2 failed"))) (when (not (list_equal (triplet_sum2 test_triplet_sum_arr2 65) [9 19 37])) (throw (Exception. "ts2 case2 failed"))) (set! test_triplet_sum_arr3 [6 47 27 1 15]) (when (not (list_equal (triplet_sum1 test_triplet_sum_arr3 11) [0 0 0])) (throw (Exception. "ts1 case3 failed"))) (when (not (list_equal (triplet_sum2 test_triplet_sum_arr3 11) [0 0 0])) (throw (Exception. "ts2 case3 failed"))))))

(defn main []
  (binding [main_res nil main_sample nil] (do (test_triplet_sum) (set! main_sample [13 29 7 23 5]) (set! main_res (triplet_sum2 main_sample 35)) (println (str (str (str (str (str (nth main_res 0)) " ") (str (nth main_res 1))) " ") (str (nth main_res 2)))))))

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
