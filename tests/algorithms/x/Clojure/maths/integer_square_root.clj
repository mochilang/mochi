(ns main (:refer-clojure :exclude [integer_square_root test_integer_square_root main]))

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

(declare integer_square_root test_integer_square_root main)

(def ^:dynamic integer_square_root_left_bound nil)

(def ^:dynamic integer_square_root_mid nil)

(def ^:dynamic integer_square_root_mid_squared nil)

(def ^:dynamic integer_square_root_right_bound nil)

(def ^:dynamic test_integer_square_root_expected nil)

(def ^:dynamic test_integer_square_root_i nil)

(def ^:dynamic test_integer_square_root_result nil)

(defn integer_square_root [integer_square_root_num]
  (binding [integer_square_root_left_bound nil integer_square_root_mid nil integer_square_root_mid_squared nil integer_square_root_right_bound nil] (try (do (when (< integer_square_root_num 0) (throw (Exception. "num must be non-negative integer"))) (when (< integer_square_root_num 2) (throw (ex-info "return" {:v integer_square_root_num}))) (set! integer_square_root_left_bound 0) (set! integer_square_root_right_bound (quot integer_square_root_num 2)) (while (<= integer_square_root_left_bound integer_square_root_right_bound) (do (set! integer_square_root_mid (+ integer_square_root_left_bound (quot (- integer_square_root_right_bound integer_square_root_left_bound) 2))) (set! integer_square_root_mid_squared (* integer_square_root_mid integer_square_root_mid)) (when (= integer_square_root_mid_squared integer_square_root_num) (throw (ex-info "return" {:v integer_square_root_mid}))) (if (< integer_square_root_mid_squared integer_square_root_num) (set! integer_square_root_left_bound (+ integer_square_root_mid 1)) (set! integer_square_root_right_bound (- integer_square_root_mid 1))))) (throw (ex-info "return" {:v integer_square_root_right_bound}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn test_integer_square_root []
  (binding [test_integer_square_root_expected nil test_integer_square_root_i nil test_integer_square_root_result nil] (do (set! test_integer_square_root_expected [0 1 1 1 2 2 2 2 2 3 3 3 3 3 3 3 4 4]) (set! test_integer_square_root_i 0) (while (< test_integer_square_root_i (count test_integer_square_root_expected)) (do (set! test_integer_square_root_result (integer_square_root test_integer_square_root_i)) (when (not= test_integer_square_root_result (nth test_integer_square_root_expected test_integer_square_root_i)) (throw (Exception. (str "test failed at index " (str test_integer_square_root_i))))) (set! test_integer_square_root_i (+ test_integer_square_root_i 1)))) (when (not= (integer_square_root 625) 25) (throw (Exception. "sqrt of 625 incorrect"))) (when (not= (integer_square_root 2147483647) 46340) (throw (Exception. "sqrt of max int incorrect"))))))

(defn main []
  (do (test_integer_square_root) (println (str (integer_square_root 625)))))

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
