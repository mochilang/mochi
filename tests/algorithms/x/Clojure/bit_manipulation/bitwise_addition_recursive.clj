(ns main (:refer-clojure :exclude [bitwise_xor bitwise_and bitwise_addition_recursive]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(defn indexOf [s sub]
  (let [idx (clojure.string/index-of s sub)] (if (nil? idx) -1 idx)))

(defn split [s sep]
  (clojure.string/split s (re-pattern sep)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare bitwise_xor bitwise_and bitwise_addition_recursive)

(def ^:dynamic bitwise_addition_recursive_bitwise_sum nil)

(def ^:dynamic bitwise_addition_recursive_carry nil)

(def ^:dynamic bitwise_and_bit nil)

(def ^:dynamic bitwise_and_result nil)

(def ^:dynamic bitwise_and_x nil)

(def ^:dynamic bitwise_and_y nil)

(def ^:dynamic bitwise_xor_ax nil)

(def ^:dynamic bitwise_xor_bit nil)

(def ^:dynamic bitwise_xor_by nil)

(def ^:dynamic bitwise_xor_result nil)

(def ^:dynamic bitwise_xor_x nil)

(def ^:dynamic bitwise_xor_y nil)

(defn bitwise_xor [bitwise_xor_a bitwise_xor_b]
  (binding [bitwise_xor_ax nil bitwise_xor_bit nil bitwise_xor_by nil bitwise_xor_result nil bitwise_xor_x nil bitwise_xor_y nil] (try (do (set! bitwise_xor_result 0) (set! bitwise_xor_bit 1) (set! bitwise_xor_x bitwise_xor_a) (set! bitwise_xor_y bitwise_xor_b) (while (or (> bitwise_xor_x 0) (> bitwise_xor_y 0)) (do (set! bitwise_xor_ax (mod bitwise_xor_x 2)) (set! bitwise_xor_by (mod bitwise_xor_y 2)) (when (= (mod (+ bitwise_xor_ax bitwise_xor_by) 2) 1) (set! bitwise_xor_result (+ bitwise_xor_result bitwise_xor_bit))) (set! bitwise_xor_x (quot bitwise_xor_x 2)) (set! bitwise_xor_y (quot bitwise_xor_y 2)) (set! bitwise_xor_bit (* bitwise_xor_bit 2)))) (throw (ex-info "return" {:v bitwise_xor_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn bitwise_and [bitwise_and_a bitwise_and_b]
  (binding [bitwise_and_bit nil bitwise_and_result nil bitwise_and_x nil bitwise_and_y nil] (try (do (set! bitwise_and_result 0) (set! bitwise_and_bit 1) (set! bitwise_and_x bitwise_and_a) (set! bitwise_and_y bitwise_and_b) (while (and (> bitwise_and_x 0) (> bitwise_and_y 0)) (do (when (and (= (mod bitwise_and_x 2) 1) (= (mod bitwise_and_y 2) 1)) (set! bitwise_and_result (+ bitwise_and_result bitwise_and_bit))) (set! bitwise_and_x (quot bitwise_and_x 2)) (set! bitwise_and_y (quot bitwise_and_y 2)) (set! bitwise_and_bit (* bitwise_and_bit 2)))) (throw (ex-info "return" {:v bitwise_and_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn bitwise_addition_recursive [bitwise_addition_recursive_number bitwise_addition_recursive_other_number]
  (binding [bitwise_addition_recursive_bitwise_sum nil bitwise_addition_recursive_carry nil] (try (do (when (or (< bitwise_addition_recursive_number 0) (< bitwise_addition_recursive_other_number 0)) (throw (Exception. "Both arguments MUST be non-negative!"))) (set! bitwise_addition_recursive_bitwise_sum (bitwise_xor bitwise_addition_recursive_number bitwise_addition_recursive_other_number)) (set! bitwise_addition_recursive_carry (bitwise_and bitwise_addition_recursive_number bitwise_addition_recursive_other_number)) (if (= bitwise_addition_recursive_carry 0) bitwise_addition_recursive_bitwise_sum (bitwise_addition_recursive bitwise_addition_recursive_bitwise_sum (* bitwise_addition_recursive_carry 2)))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (bitwise_addition_recursive 4 5)))
      (println (str (bitwise_addition_recursive 8 9)))
      (println (str (bitwise_addition_recursive 0 4)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
