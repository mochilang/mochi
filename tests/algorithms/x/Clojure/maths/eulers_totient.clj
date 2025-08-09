(ns main (:refer-clojure :exclude [totient test_totient main]))

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

(declare totient test_totient main)

(def ^:dynamic main_i nil)

(def ^:dynamic main_n nil)

(def ^:dynamic main_res nil)

(def ^:dynamic test_totient_expected nil)

(def ^:dynamic test_totient_idx nil)

(def ^:dynamic test_totient_res nil)

(def ^:dynamic totient_i nil)

(def ^:dynamic totient_is_prime nil)

(def ^:dynamic totient_j nil)

(def ^:dynamic totient_p nil)

(def ^:dynamic totient_primes nil)

(def ^:dynamic totient_totients nil)

(defn totient [totient_n]
  (binding [totient_i nil totient_is_prime nil totient_j nil totient_p nil totient_primes nil totient_totients nil] (try (do (set! totient_is_prime []) (set! totient_totients []) (set! totient_primes []) (set! totient_i 0) (while (<= totient_i totient_n) (do (set! totient_is_prime (conj totient_is_prime true)) (set! totient_totients (conj totient_totients (- totient_i 1))) (set! totient_i (+ totient_i 1)))) (set! totient_i 2) (while (<= totient_i totient_n) (do (when (nth totient_is_prime totient_i) (set! totient_primes (conj totient_primes totient_i))) (set! totient_j 0) (loop [while_flag_1 true] (when (and while_flag_1 (< totient_j (count totient_primes))) (do (set! totient_p (nth totient_primes totient_j)) (cond (>= (* totient_i totient_p) totient_n) (recur false) (= (mod totient_i totient_p) 0) (do (set! totient_totients (assoc totient_totients (* totient_i totient_p) (* (nth totient_totients totient_i) totient_p))) (recur false)) :else (do (set! totient_is_prime (assoc totient_is_prime (* totient_i totient_p) false)) (set! totient_totients (assoc totient_totients (* totient_i totient_p) (* (nth totient_totients totient_i) (- totient_p 1)))) (set! totient_j (+ totient_j 1)) (recur while_flag_1)))))) (set! totient_i (+ totient_i 1)))) (throw (ex-info "return" {:v totient_totients}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn test_totient []
  (binding [test_totient_expected nil test_totient_idx nil test_totient_res nil] (do (set! test_totient_expected [(- 1) 0 1 2 2 4 2 6 4 6 9]) (set! test_totient_res (totient 10)) (set! test_totient_idx 0) (while (< test_totient_idx (count test_totient_expected)) (do (when (not= (nth test_totient_res test_totient_idx) (nth test_totient_expected test_totient_idx)) (throw (Exception. (str "totient mismatch at " (str test_totient_idx))))) (set! test_totient_idx (+ test_totient_idx 1)))))))

(defn main []
  (binding [main_i nil main_n nil main_res nil] (do (test_totient) (set! main_n 10) (set! main_res (totient main_n)) (set! main_i 1) (while (< main_i main_n) (do (println (str (str (str (str main_i) " has ") (str (nth main_res main_i))) " relative primes.")) (set! main_i (+ main_i 1)))))))

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
