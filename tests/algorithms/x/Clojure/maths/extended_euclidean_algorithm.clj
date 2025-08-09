(ns main (:refer-clojure :exclude [abs_val extended_euclidean_algorithm test_extended_euclidean_algorithm main]))

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

(declare abs_val extended_euclidean_algorithm test_extended_euclidean_algorithm main)

(def ^:dynamic extended_euclidean_algorithm_coeff_a nil)

(def ^:dynamic extended_euclidean_algorithm_coeff_b nil)

(def ^:dynamic extended_euclidean_algorithm_old_coeff_a nil)

(def ^:dynamic extended_euclidean_algorithm_old_coeff_b nil)

(def ^:dynamic extended_euclidean_algorithm_old_remainder nil)

(def ^:dynamic extended_euclidean_algorithm_quotient nil)

(def ^:dynamic extended_euclidean_algorithm_remainder nil)

(def ^:dynamic extended_euclidean_algorithm_temp_a nil)

(def ^:dynamic extended_euclidean_algorithm_temp_b nil)

(def ^:dynamic extended_euclidean_algorithm_temp_remainder nil)

(def ^:dynamic main_res nil)

(def ^:dynamic test_extended_euclidean_algorithm_r1 nil)

(def ^:dynamic test_extended_euclidean_algorithm_r2 nil)

(def ^:dynamic test_extended_euclidean_algorithm_r3 nil)

(def ^:dynamic test_extended_euclidean_algorithm_r4 nil)

(def ^:dynamic test_extended_euclidean_algorithm_r5 nil)

(def ^:dynamic test_extended_euclidean_algorithm_r6 nil)

(def ^:dynamic test_extended_euclidean_algorithm_r7 nil)

(defn abs_val [abs_val_n]
  (try (if (< abs_val_n 0) (- abs_val_n) abs_val_n) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn extended_euclidean_algorithm [extended_euclidean_algorithm_a extended_euclidean_algorithm_b]
  (binding [extended_euclidean_algorithm_coeff_a nil extended_euclidean_algorithm_coeff_b nil extended_euclidean_algorithm_old_coeff_a nil extended_euclidean_algorithm_old_coeff_b nil extended_euclidean_algorithm_old_remainder nil extended_euclidean_algorithm_quotient nil extended_euclidean_algorithm_remainder nil extended_euclidean_algorithm_temp_a nil extended_euclidean_algorithm_temp_b nil extended_euclidean_algorithm_temp_remainder nil] (try (do (when (= (abs_val extended_euclidean_algorithm_a) 1) (throw (ex-info "return" {:v {:x extended_euclidean_algorithm_a :y 0}}))) (when (= (abs_val extended_euclidean_algorithm_b) 1) (throw (ex-info "return" {:v {:x 0 :y extended_euclidean_algorithm_b}}))) (set! extended_euclidean_algorithm_old_remainder extended_euclidean_algorithm_a) (set! extended_euclidean_algorithm_remainder extended_euclidean_algorithm_b) (set! extended_euclidean_algorithm_old_coeff_a 1) (set! extended_euclidean_algorithm_coeff_a 0) (set! extended_euclidean_algorithm_old_coeff_b 0) (set! extended_euclidean_algorithm_coeff_b 1) (while (not= extended_euclidean_algorithm_remainder 0) (do (set! extended_euclidean_algorithm_quotient (quot extended_euclidean_algorithm_old_remainder extended_euclidean_algorithm_remainder)) (set! extended_euclidean_algorithm_temp_remainder (- extended_euclidean_algorithm_old_remainder (* extended_euclidean_algorithm_quotient extended_euclidean_algorithm_remainder))) (set! extended_euclidean_algorithm_old_remainder extended_euclidean_algorithm_remainder) (set! extended_euclidean_algorithm_remainder extended_euclidean_algorithm_temp_remainder) (set! extended_euclidean_algorithm_temp_a (- extended_euclidean_algorithm_old_coeff_a (* extended_euclidean_algorithm_quotient extended_euclidean_algorithm_coeff_a))) (set! extended_euclidean_algorithm_old_coeff_a extended_euclidean_algorithm_coeff_a) (set! extended_euclidean_algorithm_coeff_a extended_euclidean_algorithm_temp_a) (set! extended_euclidean_algorithm_temp_b (- extended_euclidean_algorithm_old_coeff_b (* extended_euclidean_algorithm_quotient extended_euclidean_algorithm_coeff_b))) (set! extended_euclidean_algorithm_old_coeff_b extended_euclidean_algorithm_coeff_b) (set! extended_euclidean_algorithm_coeff_b extended_euclidean_algorithm_temp_b))) (when (< extended_euclidean_algorithm_a 0) (set! extended_euclidean_algorithm_old_coeff_a (- extended_euclidean_algorithm_old_coeff_a))) (when (< extended_euclidean_algorithm_b 0) (set! extended_euclidean_algorithm_old_coeff_b (- extended_euclidean_algorithm_old_coeff_b))) (throw (ex-info "return" {:v {:x extended_euclidean_algorithm_old_coeff_a :y extended_euclidean_algorithm_old_coeff_b}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn test_extended_euclidean_algorithm []
  (binding [test_extended_euclidean_algorithm_r1 nil test_extended_euclidean_algorithm_r2 nil test_extended_euclidean_algorithm_r3 nil test_extended_euclidean_algorithm_r4 nil test_extended_euclidean_algorithm_r5 nil test_extended_euclidean_algorithm_r6 nil test_extended_euclidean_algorithm_r7 nil] (do (set! test_extended_euclidean_algorithm_r1 (extended_euclidean_algorithm 1 24)) (when (or (not= (:x test_extended_euclidean_algorithm_r1) 1) (not= (:y test_extended_euclidean_algorithm_r1) 0)) (throw (Exception. "test1 failed"))) (set! test_extended_euclidean_algorithm_r2 (extended_euclidean_algorithm 8 14)) (when (or (not= (:x test_extended_euclidean_algorithm_r2) 2) (not= (:y test_extended_euclidean_algorithm_r2) (- 1))) (throw (Exception. "test2 failed"))) (set! test_extended_euclidean_algorithm_r3 (extended_euclidean_algorithm 240 46)) (when (or (not= (:x test_extended_euclidean_algorithm_r3) (- 9)) (not= (:y test_extended_euclidean_algorithm_r3) 47)) (throw (Exception. "test3 failed"))) (set! test_extended_euclidean_algorithm_r4 (extended_euclidean_algorithm 1 (- 4))) (when (or (not= (:x test_extended_euclidean_algorithm_r4) 1) (not= (:y test_extended_euclidean_algorithm_r4) 0)) (throw (Exception. "test4 failed"))) (set! test_extended_euclidean_algorithm_r5 (extended_euclidean_algorithm (- 2) (- 4))) (when (or (not= (:x test_extended_euclidean_algorithm_r5) (- 1)) (not= (:y test_extended_euclidean_algorithm_r5) 0)) (throw (Exception. "test5 failed"))) (set! test_extended_euclidean_algorithm_r6 (extended_euclidean_algorithm 0 (- 4))) (when (or (not= (:x test_extended_euclidean_algorithm_r6) 0) (not= (:y test_extended_euclidean_algorithm_r6) (- 1))) (throw (Exception. "test6 failed"))) (set! test_extended_euclidean_algorithm_r7 (extended_euclidean_algorithm 2 0)) (when (or (not= (:x test_extended_euclidean_algorithm_r7) 1) (not= (:y test_extended_euclidean_algorithm_r7) 0)) (throw (Exception. "test7 failed"))))))

(defn main []
  (binding [main_res nil] (do (test_extended_euclidean_algorithm) (set! main_res (extended_euclidean_algorithm 240 46)) (println (str (str (str (str "(" (str (:x main_res))) ", ") (str (:y main_res))) ")")))))

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
