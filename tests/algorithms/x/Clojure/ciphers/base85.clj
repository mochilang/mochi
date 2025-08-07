(ns main (:refer-clojure :exclude [indexOf ord chr to_binary bin_to_int reverse base10_to_85 base85_to_10 ascii85_encode ascii85_decode]))

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

(declare indexOf ord chr to_binary bin_to_int reverse base10_to_85 base85_to_10 ascii85_encode ascii85_decode)

(def ^:dynamic ascii85_decode_binary_data nil)

(def ^:dynamic ascii85_decode_bits nil)

(def ^:dynamic ascii85_decode_byte_bits nil)

(def ^:dynamic ascii85_decode_c nil)

(def ^:dynamic ascii85_decode_chunk nil)

(def ^:dynamic ascii85_decode_i nil)

(def ^:dynamic ascii85_decode_j nil)

(def ^:dynamic ascii85_decode_null_values nil)

(def ^:dynamic ascii85_decode_result nil)

(def ^:dynamic ascii85_decode_trim nil)

(def ^:dynamic ascii85_decode_value nil)

(def ^:dynamic ascii85_encode_binary_data nil)

(def ^:dynamic ascii85_encode_chunk_bits nil)

(def ^:dynamic ascii85_encode_chunk_val nil)

(def ^:dynamic ascii85_encode_encoded nil)

(def ^:dynamic ascii85_encode_i nil)

(def ^:dynamic ascii85_encode_null_values nil)

(def ^:dynamic ascii85_encode_result nil)

(def ^:dynamic ascii85_encode_total_bits nil)

(def ^:dynamic base85_to_10_i nil)

(def ^:dynamic base85_to_10_value nil)

(def ^:dynamic bin_to_int_i nil)

(def ^:dynamic bin_to_int_n nil)

(def ^:dynamic indexOf_i nil)

(def ^:dynamic ord_idx nil)

(def ^:dynamic reverse_i nil)

(def ^:dynamic reverse_res nil)

(def ^:dynamic to_binary_b nil)

(def ^:dynamic to_binary_val nil)

(def ^:dynamic main_ascii85_chars "!\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstu")

(defn indexOf [indexOf_s indexOf_ch]
  (binding [indexOf_i nil] (try (do (set! indexOf_i 0) (while (< indexOf_i (count indexOf_s)) (do (when (= (nth indexOf_s indexOf_i) indexOf_ch) (throw (ex-info "return" {:v indexOf_i}))) (set! indexOf_i (+ indexOf_i 1)))) (throw (ex-info "return" {:v (- 1)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn ord [ord_ch]
  (binding [ord_idx nil] (try (do (set! ord_idx (indexOf main_ascii85_chars ord_ch)) (if (>= ord_idx 0) (+ 33 ord_idx) 0)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn chr [chr_n]
  (try (if (and (>= chr_n 33) (<= chr_n 117)) (subs main_ascii85_chars (- chr_n 33) (min (- chr_n 32) (count main_ascii85_chars))) "?") (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn to_binary [to_binary_n to_binary_bits]
  (binding [to_binary_b nil to_binary_val nil] (try (do (set! to_binary_b "") (set! to_binary_val to_binary_n) (while (> to_binary_val 0) (do (set! to_binary_b (str (str (mod to_binary_val 2)) to_binary_b)) (set! to_binary_val (quot to_binary_val 2)))) (while (< (count to_binary_b) to_binary_bits) (set! to_binary_b (str "0" to_binary_b))) (when (= (count to_binary_b) 0) (set! to_binary_b "0")) (throw (ex-info "return" {:v to_binary_b}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn bin_to_int [bin_to_int_bits]
  (binding [bin_to_int_i nil bin_to_int_n nil] (try (do (set! bin_to_int_n 0) (set! bin_to_int_i 0) (while (< bin_to_int_i (count bin_to_int_bits)) (do (if (= (nth bin_to_int_bits bin_to_int_i) "1") (set! bin_to_int_n (+ (* bin_to_int_n 2) 1)) (set! bin_to_int_n (* bin_to_int_n 2))) (set! bin_to_int_i (+ bin_to_int_i 1)))) (throw (ex-info "return" {:v bin_to_int_n}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn reverse [reverse_s]
  (binding [reverse_i nil reverse_res nil] (try (do (set! reverse_res "") (set! reverse_i (- (count reverse_s) 1)) (while (>= reverse_i 0) (do (set! reverse_res (str reverse_res (nth reverse_s reverse_i))) (set! reverse_i (- reverse_i 1)))) (throw (ex-info "return" {:v reverse_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn base10_to_85 [base10_to_85_d]
  (try (if (> base10_to_85_d 0) (str (chr (+ (mod base10_to_85_d 85) 33)) (base10_to_85 (quot base10_to_85_d 85))) "") (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn base85_to_10 [base85_to_10_digits]
  (binding [base85_to_10_i nil base85_to_10_value nil] (try (do (set! base85_to_10_value 0) (set! base85_to_10_i 0) (while (< base85_to_10_i (count base85_to_10_digits)) (do (set! base85_to_10_value (+ (* base85_to_10_value 85) (- (ord (nth base85_to_10_digits base85_to_10_i)) 33))) (set! base85_to_10_i (+ base85_to_10_i 1)))) (throw (ex-info "return" {:v base85_to_10_value}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn ascii85_encode [ascii85_encode_data]
  (binding [ascii85_encode_binary_data nil ascii85_encode_chunk_bits nil ascii85_encode_chunk_val nil ascii85_encode_encoded nil ascii85_encode_i nil ascii85_encode_null_values nil ascii85_encode_result nil ascii85_encode_total_bits nil] (try (do (set! ascii85_encode_binary_data "") (doseq [ch ascii85_encode_data] (set! ascii85_encode_binary_data (str ascii85_encode_binary_data (to_binary (ord ch) 8)))) (set! ascii85_encode_null_values (quot (- (* 32 (+ (quot (count ascii85_encode_binary_data) 32) 1)) (count ascii85_encode_binary_data)) 8)) (set! ascii85_encode_total_bits (* 32 (+ (quot (count ascii85_encode_binary_data) 32) 1))) (while (< (count ascii85_encode_binary_data) ascii85_encode_total_bits) (set! ascii85_encode_binary_data (str ascii85_encode_binary_data "0"))) (set! ascii85_encode_result "") (set! ascii85_encode_i 0) (while (< ascii85_encode_i (count ascii85_encode_binary_data)) (do (set! ascii85_encode_chunk_bits (subs ascii85_encode_binary_data ascii85_encode_i (min (+ ascii85_encode_i 32) (count ascii85_encode_binary_data)))) (set! ascii85_encode_chunk_val (bin_to_int ascii85_encode_chunk_bits)) (set! ascii85_encode_encoded (reverse (base10_to_85 ascii85_encode_chunk_val))) (set! ascii85_encode_result (str ascii85_encode_result ascii85_encode_encoded)) (set! ascii85_encode_i (+ ascii85_encode_i 32)))) (when (not= (mod ascii85_encode_null_values 4) 0) (set! ascii85_encode_result (subs ascii85_encode_result 0 (min (- (count ascii85_encode_result) ascii85_encode_null_values) (count ascii85_encode_result))))) (throw (ex-info "return" {:v ascii85_encode_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn ascii85_decode [ascii85_decode_data]
  (binding [ascii85_decode_binary_data nil ascii85_decode_bits nil ascii85_decode_byte_bits nil ascii85_decode_c nil ascii85_decode_chunk nil ascii85_decode_i nil ascii85_decode_j nil ascii85_decode_null_values nil ascii85_decode_result nil ascii85_decode_trim nil ascii85_decode_value nil] (try (do (set! ascii85_decode_null_values (- (* 5 (+ (quot (count ascii85_decode_data) 5) 1)) (count ascii85_decode_data))) (set! ascii85_decode_binary_data ascii85_decode_data) (set! ascii85_decode_i 0) (while (< ascii85_decode_i ascii85_decode_null_values) (do (set! ascii85_decode_binary_data (str ascii85_decode_binary_data "u")) (set! ascii85_decode_i (+ ascii85_decode_i 1)))) (set! ascii85_decode_result "") (set! ascii85_decode_i 0) (while (< ascii85_decode_i (count ascii85_decode_binary_data)) (do (set! ascii85_decode_chunk (subs ascii85_decode_binary_data ascii85_decode_i (min (+ ascii85_decode_i 5) (count ascii85_decode_binary_data)))) (set! ascii85_decode_value (base85_to_10 ascii85_decode_chunk)) (set! ascii85_decode_bits (to_binary ascii85_decode_value 32)) (set! ascii85_decode_j 0) (while (< ascii85_decode_j 32) (do (set! ascii85_decode_byte_bits (subs ascii85_decode_bits ascii85_decode_j (min (+ ascii85_decode_j 8) (count ascii85_decode_bits)))) (set! ascii85_decode_c (chr (bin_to_int ascii85_decode_byte_bits))) (set! ascii85_decode_result (str ascii85_decode_result ascii85_decode_c)) (set! ascii85_decode_j (+ ascii85_decode_j 8)))) (set! ascii85_decode_i (+ ascii85_decode_i 5)))) (set! ascii85_decode_trim ascii85_decode_null_values) (when (= (mod ascii85_decode_null_values 5) 0) (set! ascii85_decode_trim (- ascii85_decode_null_values 1))) (throw (ex-info "return" {:v (subs ascii85_decode_result 0 (min (- (count ascii85_decode_result) ascii85_decode_trim) (count ascii85_decode_result)))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (ascii85_encode ""))
      (println (ascii85_encode "12345"))
      (println (ascii85_encode "base 85"))
      (println (ascii85_decode ""))
      (println (ascii85_decode "0etOA2#"))
      (println (ascii85_decode "@UX=h+?24"))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
