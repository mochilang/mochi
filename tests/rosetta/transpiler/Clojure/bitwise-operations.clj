(ns main (:refer-clojure :exclude [toUnsigned16 bin16 bit_and bit_or bit_xor bit_not shl shr las ras rol ror bitwise]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare toUnsigned16 bin16 bit_and bit_or bit_xor bit_not shl shr las ras rol ror bitwise)

(declare bin16_bits bin16_mask bin16_u bit_and_bit bit_and_res bit_and_ua bit_and_ub bit_not_ua bit_or_bit bit_or_res bit_or_ua bit_or_ub bit_xor_abit bit_xor_bbit bit_xor_bit bit_xor_res bit_xor_ua bit_xor_ub ras_i ras_val rol_left rol_right rol_ua ror_left ror_right ror_ua shl_i shl_ua shr_i shr_ua toUnsigned16_u)

(defn toUnsigned16 [toUnsigned16_n]
  (try (do (def toUnsigned16_u toUnsigned16_n) (when (< toUnsigned16_u 0) (def toUnsigned16_u (+ toUnsigned16_u 65536))) (throw (ex-info "return" {:v (mod toUnsigned16_u 65536)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn bin16 [bin16_n]
  (try (do (def bin16_u (toUnsigned16 bin16_n)) (def bin16_bits "") (def bin16_mask 32768) (dotimes [i 16] (do (if (>= bin16_u bin16_mask) (do (def bin16_bits (str bin16_bits "1")) (def bin16_u (- bin16_u bin16_mask))) (def bin16_bits (str bin16_bits "0"))) (def bin16_mask (int (quot bin16_mask 2))))) (throw (ex-info "return" {:v bin16_bits}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn bit_and [bit_and_a bit_and_b]
  (try (do (def bit_and_ua (toUnsigned16 bit_and_a)) (def bit_and_ub (toUnsigned16 bit_and_b)) (def bit_and_res 0) (def bit_and_bit 1) (dotimes [i 16] (do (when (and (= (mod bit_and_ua 2) 1) (= (mod bit_and_ub 2) 1)) (def bit_and_res (+ bit_and_res bit_and_bit))) (def bit_and_ua (int (quot bit_and_ua 2))) (def bit_and_ub (int (quot bit_and_ub 2))) (def bit_and_bit (* bit_and_bit 2)))) (throw (ex-info "return" {:v bit_and_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn bit_or [bit_or_a bit_or_b]
  (try (do (def bit_or_ua (toUnsigned16 bit_or_a)) (def bit_or_ub (toUnsigned16 bit_or_b)) (def bit_or_res 0) (def bit_or_bit 1) (dotimes [i 16] (do (when (or (= (mod bit_or_ua 2) 1) (= (mod bit_or_ub 2) 1)) (def bit_or_res (+ bit_or_res bit_or_bit))) (def bit_or_ua (int (quot bit_or_ua 2))) (def bit_or_ub (int (quot bit_or_ub 2))) (def bit_or_bit (* bit_or_bit 2)))) (throw (ex-info "return" {:v bit_or_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn bit_xor [bit_xor_a bit_xor_b]
  (try (do (def bit_xor_ua (toUnsigned16 bit_xor_a)) (def bit_xor_ub (toUnsigned16 bit_xor_b)) (def bit_xor_res 0) (def bit_xor_bit 1) (dotimes [i 16] (do (def bit_xor_abit (mod bit_xor_ua 2)) (def bit_xor_bbit (mod bit_xor_ub 2)) (when (or (and (= bit_xor_abit 1) (= bit_xor_bbit 0)) (and (= bit_xor_abit 0) (= bit_xor_bbit 1))) (def bit_xor_res (+ bit_xor_res bit_xor_bit))) (def bit_xor_ua (int (quot bit_xor_ua 2))) (def bit_xor_ub (int (quot bit_xor_ub 2))) (def bit_xor_bit (* bit_xor_bit 2)))) (throw (ex-info "return" {:v bit_xor_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn bit_not [bit_not_a]
  (try (do (def bit_not_ua (toUnsigned16 bit_not_a)) (throw (ex-info "return" {:v (- 65535 bit_not_ua)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn shl [shl_a shl_b]
  (try (do (def shl_ua (toUnsigned16 shl_a)) (def shl_i 0) (while (< shl_i shl_b) (do (def shl_ua (mod (* shl_ua 2) 65536)) (def shl_i (+ shl_i 1)))) (throw (ex-info "return" {:v shl_ua}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn shr [shr_a shr_b]
  (try (do (def shr_ua (toUnsigned16 shr_a)) (def shr_i 0) (while (< shr_i shr_b) (do (def shr_ua (int (quot shr_ua 2))) (def shr_i (+ shr_i 1)))) (throw (ex-info "return" {:v shr_ua}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn las [las_a las_b]
  (try (throw (ex-info "return" {:v (shl las_a las_b)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn ras [ras_a ras_b]
  (try (do (def ras_val ras_a) (def ras_i 0) (while (< ras_i ras_b) (do (if (>= ras_val 0) (def ras_val (int (quot ras_val 2))) (def ras_val (int (quot (- ras_val 1) 2)))) (def ras_i (+ ras_i 1)))) (throw (ex-info "return" {:v (toUnsigned16 ras_val)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn rol [rol_a rol_b]
  (try (do (def rol_ua (toUnsigned16 rol_a)) (def rol_left (shl rol_ua rol_b)) (def rol_right (shr rol_ua (- 16 rol_b))) (throw (ex-info "return" {:v (toUnsigned16 (+ rol_left rol_right))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn ror [ror_a ror_b]
  (try (do (def ror_ua (toUnsigned16 ror_a)) (def ror_right (shr ror_ua ror_b)) (def ror_left (shl ror_ua (- 16 ror_b))) (throw (ex-info "return" {:v (toUnsigned16 (+ ror_left ror_right))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn bitwise [bitwise_a bitwise_b]
  (try (do (println (str "a:   " (bin16 bitwise_a))) (println (str "b:   " (bin16 bitwise_b))) (println (str "and: " (bin16 (bit_and bitwise_a bitwise_b)))) (println (str "or:  " (bin16 (bit_or bitwise_a bitwise_b)))) (println (str "xor: " (bin16 (bit_xor bitwise_a bitwise_b)))) (println (str "not: " (bin16 (bit_not bitwise_a)))) (when (< bitwise_b 0) (do (println "Right operand is negative, but all shifts require an unsigned right operand (shift distance).") (throw (ex-info "return" {:v nil})))) (println (str "shl: " (bin16 (shl bitwise_a bitwise_b)))) (println (str "shr: " (bin16 (shr bitwise_a bitwise_b)))) (println (str "las: " (bin16 (las bitwise_a bitwise_b)))) (println (str "ras: " (bin16 (ras bitwise_a bitwise_b)))) (println (str "rol: " (bin16 (rol bitwise_a bitwise_b)))) (println (str "ror: " (bin16 (ror bitwise_a bitwise_b))))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn -main []
  (bitwise (- 460) 6))

(-main)
