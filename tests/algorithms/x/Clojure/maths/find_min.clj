(ns main (:refer-clojure :exclude [find_min_iterative find_min_recursive test_find_min main]))

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

(declare find_min_iterative find_min_recursive test_find_min main)

(def ^:dynamic find_min_iterative_i nil)

(def ^:dynamic find_min_iterative_min_num nil)

(def ^:dynamic find_min_iterative_num nil)

(def ^:dynamic find_min_recursive_l nil)

(def ^:dynamic find_min_recursive_left_min nil)

(def ^:dynamic find_min_recursive_mid nil)

(def ^:dynamic find_min_recursive_n nil)

(def ^:dynamic find_min_recursive_r nil)

(def ^:dynamic find_min_recursive_right_min nil)

(def ^:dynamic main_sample nil)

(def ^:dynamic test_find_min_a nil)

(def ^:dynamic test_find_min_b nil)

(def ^:dynamic test_find_min_c nil)

(def ^:dynamic test_find_min_d nil)

(defn find_min_iterative [find_min_iterative_nums]
  (binding [find_min_iterative_i nil find_min_iterative_min_num nil find_min_iterative_num nil] (try (do (when (= (count find_min_iterative_nums) 0) (throw (Exception. "find_min_iterative() arg is an empty sequence"))) (set! find_min_iterative_min_num (nth find_min_iterative_nums 0)) (set! find_min_iterative_i 0) (while (< find_min_iterative_i (count find_min_iterative_nums)) (do (set! find_min_iterative_num (nth find_min_iterative_nums find_min_iterative_i)) (when (< find_min_iterative_num find_min_iterative_min_num) (set! find_min_iterative_min_num find_min_iterative_num)) (set! find_min_iterative_i (+ find_min_iterative_i 1)))) (throw (ex-info "return" {:v find_min_iterative_min_num}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn find_min_recursive [find_min_recursive_nums find_min_recursive_left find_min_recursive_right]
  (binding [find_min_recursive_l nil find_min_recursive_left_min nil find_min_recursive_mid nil find_min_recursive_n nil find_min_recursive_r nil find_min_recursive_right_min nil] (try (do (set! find_min_recursive_n (count find_min_recursive_nums)) (when (= find_min_recursive_n 0) (throw (Exception. "find_min_recursive() arg is an empty sequence"))) (when (or (or (or (>= find_min_recursive_left find_min_recursive_n) (< find_min_recursive_left (- 0 find_min_recursive_n))) (>= find_min_recursive_right find_min_recursive_n)) (< find_min_recursive_right (- 0 find_min_recursive_n))) (throw (Exception. "list index out of range"))) (set! find_min_recursive_l find_min_recursive_left) (set! find_min_recursive_r find_min_recursive_right) (when (< find_min_recursive_l 0) (set! find_min_recursive_l (+ find_min_recursive_n find_min_recursive_l))) (when (< find_min_recursive_r 0) (set! find_min_recursive_r (+ find_min_recursive_n find_min_recursive_r))) (when (= find_min_recursive_l find_min_recursive_r) (throw (ex-info "return" {:v (nth find_min_recursive_nums find_min_recursive_l)}))) (set! find_min_recursive_mid (quot (+ find_min_recursive_l find_min_recursive_r) 2)) (set! find_min_recursive_left_min (find_min_recursive find_min_recursive_nums find_min_recursive_l find_min_recursive_mid)) (set! find_min_recursive_right_min (find_min_recursive find_min_recursive_nums (+ find_min_recursive_mid 1) find_min_recursive_r)) (if (<= find_min_recursive_left_min find_min_recursive_right_min) find_min_recursive_left_min find_min_recursive_right_min)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn test_find_min []
  (binding [test_find_min_a nil test_find_min_b nil test_find_min_c nil test_find_min_d nil] (do (set! test_find_min_a [3.0 2.0 1.0]) (when (not= (find_min_iterative test_find_min_a) 1.0) (throw (Exception. "iterative test1 failed"))) (when (not= (find_min_recursive test_find_min_a 0 (- (count test_find_min_a) 1)) 1.0) (throw (Exception. "recursive test1 failed"))) (set! test_find_min_b [(- 3.0) (- 2.0) (- 1.0)]) (when (not= (find_min_iterative test_find_min_b) (- 3.0)) (throw (Exception. "iterative test2 failed"))) (when (not= (find_min_recursive test_find_min_b 0 (- (count test_find_min_b) 1)) (- 3.0)) (throw (Exception. "recursive test2 failed"))) (set! test_find_min_c [3.0 (- 3.0) 0.0]) (when (not= (find_min_iterative test_find_min_c) (- 3.0)) (throw (Exception. "iterative test3 failed"))) (when (not= (find_min_recursive test_find_min_c 0 (- (count test_find_min_c) 1)) (- 3.0)) (throw (Exception. "recursive test3 failed"))) (set! test_find_min_d [1.0 3.0 5.0 7.0 9.0 2.0 4.0 6.0 8.0 10.0]) (when (not= (find_min_recursive test_find_min_d (- 0 (count test_find_min_d)) (- 0 1)) 1.0) (throw (Exception. "negative index test failed"))))))

(defn main []
  (binding [main_sample nil] (do (test_find_min) (set! main_sample [0.0 1.0 2.0 3.0 4.0 5.0 (- 3.0) 24.0 (- 56.0)]) (println (str (find_min_iterative main_sample))))))

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
