(ns main (:refer-clojure :exclude [bubble_sort find_median interquartile_range absf float_equal test_interquartile_range main]))

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

(declare bubble_sort find_median interquartile_range absf float_equal test_interquartile_range main)

(def ^:dynamic bubble_sort_a nil)

(def ^:dynamic bubble_sort_arr nil)

(def ^:dynamic bubble_sort_b nil)

(def ^:dynamic bubble_sort_i nil)

(def ^:dynamic bubble_sort_n nil)

(def ^:dynamic bubble_sort_temp nil)

(def ^:dynamic find_median_div nil)

(def ^:dynamic find_median_length nil)

(def ^:dynamic find_median_mod nil)

(def ^:dynamic float_equal_diff nil)

(def ^:dynamic interquartile_range_div nil)

(def ^:dynamic interquartile_range_i nil)

(def ^:dynamic interquartile_range_j nil)

(def ^:dynamic interquartile_range_length nil)

(def ^:dynamic interquartile_range_lower nil)

(def ^:dynamic interquartile_range_mod nil)

(def ^:dynamic interquartile_range_q1 nil)

(def ^:dynamic interquartile_range_q3 nil)

(def ^:dynamic interquartile_range_sorted nil)

(def ^:dynamic interquartile_range_upper nil)

(defn bubble_sort [bubble_sort_nums]
  (binding [bubble_sort_a nil bubble_sort_arr nil bubble_sort_b nil bubble_sort_i nil bubble_sort_n nil bubble_sort_temp nil] (try (do (set! bubble_sort_arr []) (set! bubble_sort_i 0) (while (< bubble_sort_i (count bubble_sort_nums)) (do (set! bubble_sort_arr (conj bubble_sort_arr (nth bubble_sort_nums bubble_sort_i))) (set! bubble_sort_i (+ bubble_sort_i 1)))) (set! bubble_sort_n (count bubble_sort_arr)) (set! bubble_sort_a 0) (while (< bubble_sort_a bubble_sort_n) (do (set! bubble_sort_b 0) (while (< bubble_sort_b (- (- bubble_sort_n bubble_sort_a) 1)) (do (when (> (nth bubble_sort_arr bubble_sort_b) (nth bubble_sort_arr (+ bubble_sort_b 1))) (do (set! bubble_sort_temp (nth bubble_sort_arr bubble_sort_b)) (set! bubble_sort_arr (assoc bubble_sort_arr bubble_sort_b (nth bubble_sort_arr (+ bubble_sort_b 1)))) (set! bubble_sort_arr (assoc bubble_sort_arr (+ bubble_sort_b 1) bubble_sort_temp)))) (set! bubble_sort_b (+ bubble_sort_b 1)))) (set! bubble_sort_a (+ bubble_sort_a 1)))) (throw (ex-info "return" {:v bubble_sort_arr}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn find_median [find_median_nums]
  (binding [find_median_div nil find_median_length nil find_median_mod nil] (try (do (set! find_median_length (count find_median_nums)) (set! find_median_div (quot find_median_length 2)) (set! find_median_mod (mod find_median_length 2)) (if (not= find_median_mod 0) (nth find_median_nums find_median_div) (/ (+ (nth find_median_nums find_median_div) (nth find_median_nums (- find_median_div 1))) 2.0))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn interquartile_range [interquartile_range_nums]
  (binding [interquartile_range_div nil interquartile_range_i nil interquartile_range_j nil interquartile_range_length nil interquartile_range_lower nil interquartile_range_mod nil interquartile_range_q1 nil interquartile_range_q3 nil interquartile_range_sorted nil interquartile_range_upper nil] (try (do (when (= (count interquartile_range_nums) 0) (throw (Exception. "The list is empty. Provide a non-empty list."))) (set! interquartile_range_sorted (bubble_sort interquartile_range_nums)) (set! interquartile_range_length (count interquartile_range_sorted)) (set! interquartile_range_div (quot interquartile_range_length 2)) (set! interquartile_range_mod (mod interquartile_range_length 2)) (set! interquartile_range_lower []) (set! interquartile_range_i 0) (while (< interquartile_range_i interquartile_range_div) (do (set! interquartile_range_lower (conj interquartile_range_lower (nth interquartile_range_sorted interquartile_range_i))) (set! interquartile_range_i (+ interquartile_range_i 1)))) (set! interquartile_range_upper []) (set! interquartile_range_j (+ interquartile_range_div interquartile_range_mod)) (while (< interquartile_range_j interquartile_range_length) (do (set! interquartile_range_upper (conj interquartile_range_upper (nth interquartile_range_sorted interquartile_range_j))) (set! interquartile_range_j (+ interquartile_range_j 1)))) (set! interquartile_range_q1 (find_median interquartile_range_lower)) (set! interquartile_range_q3 (find_median interquartile_range_upper)) (throw (ex-info "return" {:v (- interquartile_range_q3 interquartile_range_q1)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn absf [absf_x]
  (try (if (< absf_x 0.0) (- absf_x) absf_x) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn float_equal [float_equal_a float_equal_b]
  (binding [float_equal_diff nil] (try (do (set! float_equal_diff (absf (- float_equal_a float_equal_b))) (throw (ex-info "return" {:v (< float_equal_diff 0.0000001)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn test_interquartile_range []
  (do (when (not (float_equal (interquartile_range [4.0 1.0 2.0 3.0 2.0]) 2.0)) (throw (Exception. "interquartile_range case1 failed"))) (when (not (float_equal (interquartile_range [(- 2.0) (- 7.0) (- 10.0) 9.0 8.0 4.0 (- 67.0) 45.0]) 17.0)) (throw (Exception. "interquartile_range case2 failed"))) (when (not (float_equal (interquartile_range [(- 2.1) (- 7.1) (- 10.1) 9.1 8.1 4.1 (- 67.1) 45.1]) 17.2)) (throw (Exception. "interquartile_range case3 failed"))) (when (not (float_equal (interquartile_range [0.0 0.0 0.0 0.0 0.0]) 0.0)) (throw (Exception. "interquartile_range case4 failed")))))

(defn main []
  (do (test_interquartile_range) (println (str (interquartile_range [4.0 1.0 2.0 3.0 2.0])))))

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
