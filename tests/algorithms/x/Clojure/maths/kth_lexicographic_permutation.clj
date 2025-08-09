(ns main (:refer-clojure :exclude [remove_at kth_permutation list_equal list_to_string test_kth_permutation main]))

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

(declare remove_at kth_permutation list_equal list_to_string test_kth_permutation main)

(def ^:dynamic kth_permutation_e nil)

(def ^:dynamic kth_permutation_elements nil)

(def ^:dynamic kth_permutation_factorial nil)

(def ^:dynamic kth_permutation_factorials nil)

(def ^:dynamic kth_permutation_i nil)

(def ^:dynamic kth_permutation_idx nil)

(def ^:dynamic kth_permutation_k nil)

(def ^:dynamic kth_permutation_number nil)

(def ^:dynamic kth_permutation_permutation nil)

(def ^:dynamic kth_permutation_total nil)

(def ^:dynamic list_equal_i nil)

(def ^:dynamic list_to_string_i nil)

(def ^:dynamic list_to_string_s nil)

(def ^:dynamic main_res nil)

(def ^:dynamic remove_at_i nil)

(def ^:dynamic remove_at_res nil)

(def ^:dynamic test_kth_permutation_expected1 nil)

(def ^:dynamic test_kth_permutation_expected2 nil)

(def ^:dynamic test_kth_permutation_res1 nil)

(def ^:dynamic test_kth_permutation_res2 nil)

(defn remove_at [remove_at_xs remove_at_idx]
  (binding [remove_at_i nil remove_at_res nil] (try (do (set! remove_at_res []) (set! remove_at_i 0) (while (< remove_at_i (count remove_at_xs)) (do (when (not= remove_at_i remove_at_idx) (set! remove_at_res (conj remove_at_res (nth remove_at_xs remove_at_i)))) (set! remove_at_i (+ remove_at_i 1)))) (throw (ex-info "return" {:v remove_at_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn kth_permutation [kth_permutation_k_p kth_permutation_n]
  (binding [kth_permutation_e nil kth_permutation_elements nil kth_permutation_factorial nil kth_permutation_factorials nil kth_permutation_i nil kth_permutation_idx nil kth_permutation_k nil kth_permutation_number nil kth_permutation_permutation nil kth_permutation_total nil] (try (do (set! kth_permutation_k kth_permutation_k_p) (when (<= kth_permutation_n 0) (throw (Exception. "n must be positive"))) (set! kth_permutation_factorials [1]) (set! kth_permutation_i 2) (while (< kth_permutation_i kth_permutation_n) (do (set! kth_permutation_factorials (conj kth_permutation_factorials (* (nth kth_permutation_factorials (- (count kth_permutation_factorials) 1)) kth_permutation_i))) (set! kth_permutation_i (+ kth_permutation_i 1)))) (set! kth_permutation_total (* (nth kth_permutation_factorials (- (count kth_permutation_factorials) 1)) kth_permutation_n)) (when (or (< kth_permutation_k 0) (>= kth_permutation_k kth_permutation_total)) (throw (Exception. "k out of bounds"))) (set! kth_permutation_elements []) (set! kth_permutation_e 0) (while (< kth_permutation_e kth_permutation_n) (do (set! kth_permutation_elements (conj kth_permutation_elements kth_permutation_e)) (set! kth_permutation_e (+ kth_permutation_e 1)))) (set! kth_permutation_permutation []) (set! kth_permutation_idx (- (count kth_permutation_factorials) 1)) (while (>= kth_permutation_idx 0) (do (set! kth_permutation_factorial (nth kth_permutation_factorials kth_permutation_idx)) (set! kth_permutation_number (quot kth_permutation_k kth_permutation_factorial)) (set! kth_permutation_k (mod kth_permutation_k kth_permutation_factorial)) (set! kth_permutation_permutation (conj kth_permutation_permutation (nth kth_permutation_elements kth_permutation_number))) (set! kth_permutation_elements (remove_at kth_permutation_elements kth_permutation_number)) (set! kth_permutation_idx (- kth_permutation_idx 1)))) (set! kth_permutation_permutation (conj kth_permutation_permutation (nth kth_permutation_elements 0))) (throw (ex-info "return" {:v kth_permutation_permutation}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn list_equal [list_equal_a list_equal_b]
  (binding [list_equal_i nil] (try (do (when (not= (count list_equal_a) (count list_equal_b)) (throw (ex-info "return" {:v false}))) (set! list_equal_i 0) (while (< list_equal_i (count list_equal_a)) (do (when (not= (nth list_equal_a list_equal_i) (nth list_equal_b list_equal_i)) (throw (ex-info "return" {:v false}))) (set! list_equal_i (+ list_equal_i 1)))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn list_to_string [list_to_string_xs]
  (binding [list_to_string_i nil list_to_string_s nil] (try (do (when (= (count list_to_string_xs) 0) (throw (ex-info "return" {:v "[]"}))) (set! list_to_string_s (str "[" (str (nth list_to_string_xs 0)))) (set! list_to_string_i 1) (while (< list_to_string_i (count list_to_string_xs)) (do (set! list_to_string_s (str (str list_to_string_s ", ") (str (nth list_to_string_xs list_to_string_i)))) (set! list_to_string_i (+ list_to_string_i 1)))) (set! list_to_string_s (str list_to_string_s "]")) (throw (ex-info "return" {:v list_to_string_s}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn test_kth_permutation []
  (binding [test_kth_permutation_expected1 nil test_kth_permutation_expected2 nil test_kth_permutation_res1 nil test_kth_permutation_res2 nil] (do (set! test_kth_permutation_expected1 [0 1 2 3 4]) (set! test_kth_permutation_res1 (kth_permutation 0 5)) (when (not (list_equal test_kth_permutation_res1 test_kth_permutation_expected1)) (throw (Exception. "test case 1 failed"))) (set! test_kth_permutation_expected2 [1 3 0 2]) (set! test_kth_permutation_res2 (kth_permutation 10 4)) (when (not (list_equal test_kth_permutation_res2 test_kth_permutation_expected2)) (throw (Exception. "test case 2 failed"))))))

(defn main []
  (binding [main_res nil] (do (test_kth_permutation) (set! main_res (kth_permutation 10 4)) (println (list_to_string main_res)))))

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
