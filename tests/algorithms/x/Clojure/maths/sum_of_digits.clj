(ns main (:refer-clojure :exclude [abs_int sum_of_digits sum_of_digits_recursion sum_of_digits_compact test_sum_of_digits main]))

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

(declare abs_int sum_of_digits sum_of_digits_recursion sum_of_digits_compact test_sum_of_digits main)

(def ^:dynamic sum_of_digits_compact_i nil)

(def ^:dynamic sum_of_digits_compact_res nil)

(def ^:dynamic sum_of_digits_compact_s nil)

(def ^:dynamic sum_of_digits_m nil)

(def ^:dynamic sum_of_digits_recursion_m nil)

(def ^:dynamic sum_of_digits_res nil)

(defn abs_int [abs_int_n]
  (try (if (< abs_int_n 0) (- abs_int_n) abs_int_n) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn sum_of_digits [sum_of_digits_n]
  (binding [sum_of_digits_m nil sum_of_digits_res nil] (try (do (set! sum_of_digits_m (abs_int sum_of_digits_n)) (set! sum_of_digits_res 0) (while (> sum_of_digits_m 0) (do (set! sum_of_digits_res (+ sum_of_digits_res (mod sum_of_digits_m 10))) (set! sum_of_digits_m (quot sum_of_digits_m 10)))) (throw (ex-info "return" {:v sum_of_digits_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn sum_of_digits_recursion [sum_of_digits_recursion_n]
  (binding [sum_of_digits_recursion_m nil] (try (do (set! sum_of_digits_recursion_m (abs_int sum_of_digits_recursion_n)) (if (< sum_of_digits_recursion_m 10) sum_of_digits_recursion_m (+ (mod sum_of_digits_recursion_m 10) (sum_of_digits_recursion (quot sum_of_digits_recursion_m 10))))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn sum_of_digits_compact [sum_of_digits_compact_n]
  (binding [sum_of_digits_compact_i nil sum_of_digits_compact_res nil sum_of_digits_compact_s nil] (try (do (set! sum_of_digits_compact_s (str (abs_int sum_of_digits_compact_n))) (set! sum_of_digits_compact_res 0) (set! sum_of_digits_compact_i 0) (while (< sum_of_digits_compact_i (count sum_of_digits_compact_s)) (do (set! sum_of_digits_compact_res (+ sum_of_digits_compact_res (Long/parseLong (subs sum_of_digits_compact_s sum_of_digits_compact_i (+ sum_of_digits_compact_i 1))))) (set! sum_of_digits_compact_i (+ sum_of_digits_compact_i 1)))) (throw (ex-info "return" {:v sum_of_digits_compact_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn test_sum_of_digits []
  (do (when (not= (sum_of_digits 12345) 15) (throw (Exception. "sum_of_digits 12345 failed"))) (when (not= (sum_of_digits 123) 6) (throw (Exception. "sum_of_digits 123 failed"))) (when (not= (sum_of_digits (- 123)) 6) (throw (Exception. "sum_of_digits -123 failed"))) (when (not= (sum_of_digits 0) 0) (throw (Exception. "sum_of_digits 0 failed"))) (when (not= (sum_of_digits_recursion 12345) 15) (throw (Exception. "recursion 12345 failed"))) (when (not= (sum_of_digits_recursion 123) 6) (throw (Exception. "recursion 123 failed"))) (when (not= (sum_of_digits_recursion (- 123)) 6) (throw (Exception. "recursion -123 failed"))) (when (not= (sum_of_digits_recursion 0) 0) (throw (Exception. "recursion 0 failed"))) (when (not= (sum_of_digits_compact 12345) 15) (throw (Exception. "compact 12345 failed"))) (when (not= (sum_of_digits_compact 123) 6) (throw (Exception. "compact 123 failed"))) (when (not= (sum_of_digits_compact (- 123)) 6) (throw (Exception. "compact -123 failed"))) (when (not= (sum_of_digits_compact 0) 0) (throw (Exception. "compact 0 failed")))))

(defn main []
  (do (test_sum_of_digits) (println (str (sum_of_digits 12345)))))

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
