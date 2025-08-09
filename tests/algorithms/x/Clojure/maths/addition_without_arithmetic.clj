(ns main (:refer-clojure :exclude [to_unsigned from_unsigned bit_and bit_xor lshift1 add]))

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

(declare to_unsigned from_unsigned bit_and bit_xor lshift1 add)

(def ^:dynamic add_carry nil)

(def ^:dynamic add_result nil)

(def ^:dynamic add_second nil)

(def ^:dynamic bit_and_bit nil)

(def ^:dynamic bit_and_i nil)

(def ^:dynamic bit_and_res nil)

(def ^:dynamic bit_and_x nil)

(def ^:dynamic bit_and_y nil)

(def ^:dynamic bit_xor_abit nil)

(def ^:dynamic bit_xor_bbit nil)

(def ^:dynamic bit_xor_bit nil)

(def ^:dynamic bit_xor_i nil)

(def ^:dynamic bit_xor_res nil)

(def ^:dynamic bit_xor_x nil)

(def ^:dynamic bit_xor_y nil)

(def ^:dynamic first_v nil)

(def ^:dynamic main_MAX 4294967296)

(def ^:dynamic main_HALF 2147483648)

(defn to_unsigned [to_unsigned_n]
  (try (if (< to_unsigned_n 0) (+ main_MAX to_unsigned_n) to_unsigned_n) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn from_unsigned [from_unsigned_n]
  (try (if (>= from_unsigned_n main_HALF) (- from_unsigned_n main_MAX) from_unsigned_n) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn bit_and [bit_and_a bit_and_b]
  (binding [bit_and_bit nil bit_and_i nil bit_and_res nil bit_and_x nil bit_and_y nil] (try (do (set! bit_and_x bit_and_a) (set! bit_and_y bit_and_b) (set! bit_and_res 0) (set! bit_and_bit 1) (set! bit_and_i 0) (while (< bit_and_i 32) (do (when (and (= (mod bit_and_x 2) 1) (= (mod bit_and_y 2) 1)) (set! bit_and_res (+ bit_and_res bit_and_bit))) (set! bit_and_x (quot bit_and_x 2)) (set! bit_and_y (quot bit_and_y 2)) (set! bit_and_bit (* bit_and_bit 2)) (set! bit_and_i (+ bit_and_i 1)))) (throw (ex-info "return" {:v bit_and_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn bit_xor [bit_xor_a bit_xor_b]
  (binding [bit_xor_abit nil bit_xor_bbit nil bit_xor_bit nil bit_xor_i nil bit_xor_res nil bit_xor_x nil bit_xor_y nil] (try (do (set! bit_xor_x bit_xor_a) (set! bit_xor_y bit_xor_b) (set! bit_xor_res 0) (set! bit_xor_bit 1) (set! bit_xor_i 0) (while (< bit_xor_i 32) (do (set! bit_xor_abit (mod bit_xor_x 2)) (set! bit_xor_bbit (mod bit_xor_y 2)) (when (= (mod (+ bit_xor_abit bit_xor_bbit) 2) 1) (set! bit_xor_res (+ bit_xor_res bit_xor_bit))) (set! bit_xor_x (quot bit_xor_x 2)) (set! bit_xor_y (quot bit_xor_y 2)) (set! bit_xor_bit (* bit_xor_bit 2)) (set! bit_xor_i (+ bit_xor_i 1)))) (throw (ex-info "return" {:v bit_xor_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn lshift1 [lshift1_num]
  (try (throw (ex-info "return" {:v (mod (* lshift1_num 2) main_MAX)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn add [add_a add_b]
  (binding [add_carry nil add_result nil add_second nil first_v nil] (try (do (set! first_v (to_unsigned add_a)) (set! add_second (to_unsigned add_b)) (while (not= add_second 0) (do (set! add_carry (bit_and first_v add_second)) (set! first_v (bit_xor first_v add_second)) (set! add_second (lshift1 add_carry)))) (set! add_result (from_unsigned first_v)) (throw (ex-info "return" {:v add_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (add 3 5)))
      (println (str (add 13 5)))
      (println (str (add (- 7) 2)))
      (println (str (add 0 (- 7))))
      (println (str (add (- 321) 0)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
