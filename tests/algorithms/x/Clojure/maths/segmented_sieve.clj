(ns main (:refer-clojure :exclude [min_int int_sqrt sieve lists_equal test_sieve main]))

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

(declare min_int int_sqrt sieve lists_equal test_sieve main)

(def ^:dynamic int_sqrt_r nil)

(def ^:dynamic lists_equal_m nil)

(def ^:dynamic sieve_each nil)

(def ^:dynamic sieve_end nil)

(def ^:dynamic sieve_high nil)

(def ^:dynamic sieve_i nil)

(def ^:dynamic sieve_idx nil)

(def ^:dynamic sieve_in_prime nil)

(def ^:dynamic sieve_j nil)

(def ^:dynamic sieve_j2 nil)

(def ^:dynamic sieve_j3 nil)

(def ^:dynamic sieve_k nil)

(def ^:dynamic sieve_low nil)

(def ^:dynamic sieve_prime nil)

(def ^:dynamic sieve_size nil)

(def ^:dynamic sieve_start nil)

(def ^:dynamic sieve_t nil)

(def ^:dynamic sieve_temp nil)

(def ^:dynamic sieve_tempSeg nil)

(def ^:dynamic test_sieve_e1 nil)

(def ^:dynamic test_sieve_e2 nil)

(defn min_int [min_int_a min_int_b]
  (try (if (< min_int_a min_int_b) min_int_a min_int_b) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn int_sqrt [int_sqrt_n]
  (binding [int_sqrt_r nil] (try (do (set! int_sqrt_r 0) (while (<= (* (+ int_sqrt_r 1) (+ int_sqrt_r 1)) int_sqrt_n) (set! int_sqrt_r (+ int_sqrt_r 1))) (throw (ex-info "return" {:v int_sqrt_r}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn sieve [sieve_n]
  (binding [sieve_each nil sieve_end nil sieve_high nil sieve_i nil sieve_idx nil sieve_in_prime nil sieve_j nil sieve_j2 nil sieve_j3 nil sieve_k nil sieve_low nil sieve_prime nil sieve_size nil sieve_start nil sieve_t nil sieve_temp nil sieve_tempSeg nil] (try (do (when (<= sieve_n 0) (throw (Exception. "Number must instead be a positive integer"))) (set! sieve_in_prime []) (set! sieve_start 2) (set! sieve_end (int_sqrt sieve_n)) (set! sieve_temp []) (set! sieve_i 0) (while (< sieve_i (+ sieve_end 1)) (do (set! sieve_temp (conj sieve_temp 1)) (set! sieve_i (+ sieve_i 1)))) (set! sieve_prime []) (while (<= sieve_start sieve_end) (do (when (= (nth sieve_temp sieve_start) 1) (do (set! sieve_in_prime (conj sieve_in_prime sieve_start)) (set! sieve_j (* sieve_start sieve_start)) (while (<= sieve_j sieve_end) (do (set! sieve_temp (assoc sieve_temp sieve_j 0)) (set! sieve_j (+ sieve_j sieve_start)))))) (set! sieve_start (+ sieve_start 1)))) (set! sieve_i 0) (while (< sieve_i (count sieve_in_prime)) (do (set! sieve_prime (conj sieve_prime (nth sieve_in_prime sieve_i))) (set! sieve_i (+ sieve_i 1)))) (set! sieve_low (+ sieve_end 1)) (set! sieve_high (min_int (* 2 sieve_end) sieve_n)) (while (<= sieve_low sieve_n) (do (set! sieve_tempSeg []) (set! sieve_size (+ (- sieve_high sieve_low) 1)) (set! sieve_k 0) (while (< sieve_k sieve_size) (do (set! sieve_tempSeg (conj sieve_tempSeg 1)) (set! sieve_k (+ sieve_k 1)))) (set! sieve_idx 0) (while (< sieve_idx (count sieve_in_prime)) (do (set! sieve_each (nth sieve_in_prime sieve_idx)) (set! sieve_t (* (quot sieve_low sieve_each) sieve_each)) (when (< sieve_t sieve_low) (set! sieve_t (+ sieve_t sieve_each))) (set! sieve_j2 sieve_t) (while (<= sieve_j2 sieve_high) (do (set! sieve_tempSeg (assoc sieve_tempSeg (- sieve_j2 sieve_low) 0)) (set! sieve_j2 (+ sieve_j2 sieve_each)))) (set! sieve_idx (+ sieve_idx 1)))) (set! sieve_j3 0) (while (< sieve_j3 (count sieve_tempSeg)) (do (when (= (nth sieve_tempSeg sieve_j3) 1) (set! sieve_prime (conj sieve_prime (+ sieve_j3 sieve_low)))) (set! sieve_j3 (+ sieve_j3 1)))) (set! sieve_low (+ sieve_high 1)) (set! sieve_high (min_int (+ sieve_high sieve_end) sieve_n)))) (throw (ex-info "return" {:v sieve_prime}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn lists_equal [lists_equal_a lists_equal_b]
  (binding [lists_equal_m nil] (try (do (when (not= (count lists_equal_a) (count lists_equal_b)) (throw (ex-info "return" {:v false}))) (set! lists_equal_m 0) (while (< lists_equal_m (count lists_equal_a)) (do (when (not= (nth lists_equal_a lists_equal_m) (nth lists_equal_b lists_equal_m)) (throw (ex-info "return" {:v false}))) (set! lists_equal_m (+ lists_equal_m 1)))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn test_sieve []
  (binding [test_sieve_e1 nil test_sieve_e2 nil] (do (set! test_sieve_e1 (sieve 8)) (when (not (lists_equal test_sieve_e1 [2 3 5 7])) (throw (Exception. "sieve(8) failed"))) (set! test_sieve_e2 (sieve 27)) (when (not (lists_equal test_sieve_e2 [2 3 5 7 11 13 17 19 23])) (throw (Exception. "sieve(27) failed"))))))

(defn main []
  (do (test_sieve) (println (str (sieve 30)))))

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
