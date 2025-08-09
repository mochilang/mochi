(ns main (:refer-clojure :exclude [normalize_index find_max_iterative find_max_recursive test_find_max main]))

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

(declare normalize_index find_max_iterative find_max_recursive test_find_max main)

(def ^:dynamic find_max_iterative_i nil)

(def ^:dynamic find_max_iterative_max_num nil)

(def ^:dynamic find_max_iterative_x nil)

(def ^:dynamic find_max_recursive_l nil)

(def ^:dynamic find_max_recursive_left_max nil)

(def ^:dynamic find_max_recursive_mid nil)

(def ^:dynamic find_max_recursive_n nil)

(def ^:dynamic find_max_recursive_r nil)

(def ^:dynamic find_max_recursive_right_max nil)

(def ^:dynamic main_nums nil)

(def ^:dynamic test_find_max_arr nil)

(defn normalize_index [normalize_index_index normalize_index_n]
  (try (if (< normalize_index_index 0) (+ normalize_index_n normalize_index_index) normalize_index_index) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn find_max_iterative [find_max_iterative_nums]
  (binding [find_max_iterative_i nil find_max_iterative_max_num nil find_max_iterative_x nil] (try (do (when (= (count find_max_iterative_nums) 0) (throw (Exception. "find_max_iterative() arg is an empty sequence"))) (set! find_max_iterative_max_num (nth find_max_iterative_nums 0)) (set! find_max_iterative_i 0) (while (< find_max_iterative_i (count find_max_iterative_nums)) (do (set! find_max_iterative_x (nth find_max_iterative_nums find_max_iterative_i)) (when (> find_max_iterative_x find_max_iterative_max_num) (set! find_max_iterative_max_num find_max_iterative_x)) (set! find_max_iterative_i (+ find_max_iterative_i 1)))) (throw (ex-info "return" {:v find_max_iterative_max_num}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn find_max_recursive [find_max_recursive_nums find_max_recursive_left find_max_recursive_right]
  (binding [find_max_recursive_l nil find_max_recursive_left_max nil find_max_recursive_mid nil find_max_recursive_n nil find_max_recursive_r nil find_max_recursive_right_max nil] (try (do (set! find_max_recursive_n (count find_max_recursive_nums)) (when (= find_max_recursive_n 0) (throw (Exception. "find_max_recursive() arg is an empty sequence"))) (when (or (or (or (>= find_max_recursive_left find_max_recursive_n) (< find_max_recursive_left (- 0 find_max_recursive_n))) (>= find_max_recursive_right find_max_recursive_n)) (< find_max_recursive_right (- 0 find_max_recursive_n))) (throw (Exception. "list index out of range"))) (set! find_max_recursive_l (normalize_index find_max_recursive_left find_max_recursive_n)) (set! find_max_recursive_r (normalize_index find_max_recursive_right find_max_recursive_n)) (when (= find_max_recursive_l find_max_recursive_r) (throw (ex-info "return" {:v (nth find_max_recursive_nums find_max_recursive_l)}))) (set! find_max_recursive_mid (quot (+ find_max_recursive_l find_max_recursive_r) 2)) (set! find_max_recursive_left_max (find_max_recursive find_max_recursive_nums find_max_recursive_l find_max_recursive_mid)) (set! find_max_recursive_right_max (find_max_recursive find_max_recursive_nums (+ find_max_recursive_mid 1) find_max_recursive_r)) (if (>= find_max_recursive_left_max find_max_recursive_right_max) find_max_recursive_left_max find_max_recursive_right_max)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn test_find_max []
  (binding [test_find_max_arr nil] (do (set! test_find_max_arr [2.0 4.0 9.0 7.0 19.0 94.0 5.0]) (when (not= (find_max_iterative test_find_max_arr) 94.0) (throw (Exception. "find_max_iterative failed"))) (when (not= (find_max_recursive test_find_max_arr 0 (- (count test_find_max_arr) 1)) 94.0) (throw (Exception. "find_max_recursive failed"))) (when (not= (find_max_recursive test_find_max_arr (- (count test_find_max_arr)) (- 1)) 94.0) (throw (Exception. "negative index handling failed"))))))

(defn main []
  (binding [main_nums nil] (do (test_find_max) (set! main_nums [2.0 4.0 9.0 7.0 19.0 94.0 5.0]) (println (find_max_iterative main_nums)) (println (find_max_recursive main_nums 0 (- (count main_nums) 1))))))

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
