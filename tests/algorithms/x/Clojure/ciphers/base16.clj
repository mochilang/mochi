(ns main (:refer-clojure :exclude [base16_encode base16_decode]))

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

(declare base16_encode base16_decode)

(def ^:dynamic base16_decode_digits nil)

(def ^:dynamic base16_decode_hi nil)

(def ^:dynamic base16_decode_hi_char nil)

(def ^:dynamic base16_decode_i nil)

(def ^:dynamic base16_decode_lo nil)

(def ^:dynamic base16_decode_lo_char nil)

(def ^:dynamic base16_decode_out nil)

(def ^:dynamic base16_encode_b nil)

(def ^:dynamic base16_encode_digits nil)

(def ^:dynamic base16_encode_hi nil)

(def ^:dynamic base16_encode_i nil)

(def ^:dynamic base16_encode_lo nil)

(def ^:dynamic base16_encode_res nil)

(def ^:dynamic hex_value_j nil)

(defn base16_encode [base16_encode_data]
  (binding [base16_encode_b nil base16_encode_digits nil base16_encode_hi nil base16_encode_i nil base16_encode_lo nil base16_encode_res nil] (try (do (set! base16_encode_digits "0123456789ABCDEF") (set! base16_encode_res "") (set! base16_encode_i 0) (while (< base16_encode_i (count base16_encode_data)) (do (set! base16_encode_b (nth base16_encode_data base16_encode_i)) (when (or (< base16_encode_b 0) (> base16_encode_b 255)) (throw (Exception. "byte out of range"))) (set! base16_encode_hi (quot base16_encode_b 16)) (set! base16_encode_lo (mod base16_encode_b 16)) (set! base16_encode_res (str (str base16_encode_res (subs base16_encode_digits base16_encode_hi (min (+ base16_encode_hi 1) (count base16_encode_digits)))) (subs base16_encode_digits base16_encode_lo (min (+ base16_encode_lo 1) (count base16_encode_digits))))) (set! base16_encode_i (+ base16_encode_i 1)))) (throw (ex-info "return" {:v base16_encode_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn hex_value [base16_decode_data hex_value_ch]
  (binding [hex_value_j nil] (try (do (set! hex_value_j 0) (while (< hex_value_j 16) (do (when (= (subvec base16_decode_digits hex_value_j (+ hex_value_j 1)) hex_value_ch) (throw (ex-info "return" {:v hex_value_j}))) (set! hex_value_j (+ hex_value_j 1)))) (throw (ex-info "return" {:v (- 1)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn base16_decode [base16_decode_data]
  (binding [base16_decode_digits nil base16_decode_hi nil base16_decode_hi_char nil base16_decode_i nil base16_decode_lo nil base16_decode_lo_char nil base16_decode_out nil] (try (do (set! base16_decode_digits "0123456789ABCDEF") (when (not= (mod (count base16_decode_data) 2) 0) (throw (Exception. "Base16 encoded data is invalid: Data does not have an even number of hex digits."))) (set! base16_decode_out []) (set! base16_decode_i 0) (while (< base16_decode_i (count base16_decode_data)) (do (set! base16_decode_hi_char (subvec base16_decode_data base16_decode_i (+ base16_decode_i 1))) (set! base16_decode_lo_char (subvec base16_decode_data (+ base16_decode_i 1) (+ base16_decode_i 2))) (set! base16_decode_hi (hex_value base16_decode_data base16_decode_hi_char)) (set! base16_decode_lo (hex_value base16_decode_data base16_decode_lo_char)) (when (or (< base16_decode_hi 0) (< base16_decode_lo 0)) (throw (Exception. "Base16 encoded data is invalid: Data is not uppercase hex or it contains invalid characters."))) (set! base16_decode_out (conj base16_decode_out (+ (* base16_decode_hi 16) base16_decode_lo))) (set! base16_decode_i (+ base16_decode_i 2)))) (throw (ex-info "return" {:v base16_decode_out}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_example1 [72 101 108 108 111 32 87 111 114 108 100 33])

(def ^:dynamic main_example2 [72 69 76 76 79 32 87 79 82 76 68 33])

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (base16_encode main_example1))
      (println (base16_encode main_example2))
      (println (base16_encode []))
      (println (str (base16_decode "48656C6C6F20576F726C6421")))
      (println (str (base16_decode "48454C4C4F20574F524C4421")))
      (println (str (base16_decode "")))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
