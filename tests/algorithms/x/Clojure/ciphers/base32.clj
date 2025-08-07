(ns main (:refer-clojure :exclude [indexOfChar ord chr repeat to_binary binary_to_int base32_encode base32_decode]))

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

(declare indexOfChar ord chr repeat to_binary binary_to_int base32_encode base32_decode)

(def ^:dynamic base32_decode_binary_chunks nil)

(def ^:dynamic base32_decode_byte_bits nil)

(def ^:dynamic base32_decode_ch nil)

(def ^:dynamic base32_decode_clean nil)

(def ^:dynamic base32_decode_code nil)

(def ^:dynamic base32_decode_i nil)

(def ^:dynamic base32_decode_idx nil)

(def ^:dynamic base32_decode_j nil)

(def ^:dynamic base32_decode_result nil)

(def ^:dynamic base32_encode_b32_result nil)

(def ^:dynamic base32_encode_binary_data nil)

(def ^:dynamic base32_encode_chunk nil)

(def ^:dynamic base32_encode_i nil)

(def ^:dynamic base32_encode_index nil)

(def ^:dynamic base32_encode_j nil)

(def ^:dynamic base32_encode_rem nil)

(def ^:dynamic base32_encode_remainder nil)

(def ^:dynamic binary_to_int_i nil)

(def ^:dynamic binary_to_int_n nil)

(def ^:dynamic chr_digits nil)

(def ^:dynamic chr_idx nil)

(def ^:dynamic chr_lower nil)

(def ^:dynamic chr_upper nil)

(def ^:dynamic indexOfChar_i nil)

(def ^:dynamic ord_digits nil)

(def ^:dynamic ord_idx nil)

(def ^:dynamic ord_lower nil)

(def ^:dynamic ord_upper nil)

(def ^:dynamic repeat_i nil)

(def ^:dynamic repeat_out nil)

(def ^:dynamic to_binary_i nil)

(def ^:dynamic to_binary_out nil)

(def ^:dynamic to_binary_v nil)

(def ^:dynamic main_B32_CHARSET "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567")

(defn indexOfChar [indexOfChar_s indexOfChar_ch]
  (binding [indexOfChar_i nil] (try (do (set! indexOfChar_i 0) (while (< indexOfChar_i (count indexOfChar_s)) (do (when (= (nth indexOfChar_s indexOfChar_i) indexOfChar_ch) (throw (ex-info "return" {:v indexOfChar_i}))) (set! indexOfChar_i (+ indexOfChar_i 1)))) (throw (ex-info "return" {:v (- 1)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn ord [ord_ch]
  (binding [ord_digits nil ord_idx nil ord_lower nil ord_upper nil] (try (do (set! ord_upper "ABCDEFGHIJKLMNOPQRSTUVWXYZ") (set! ord_lower "abcdefghijklmnopqrstuvwxyz") (set! ord_digits "0123456789") (set! ord_idx (indexOfChar ord_upper ord_ch)) (when (>= ord_idx 0) (throw (ex-info "return" {:v (+ 65 ord_idx)}))) (set! ord_idx (indexOfChar ord_lower ord_ch)) (when (>= ord_idx 0) (throw (ex-info "return" {:v (+ 97 ord_idx)}))) (set! ord_idx (indexOfChar ord_digits ord_ch)) (when (>= ord_idx 0) (throw (ex-info "return" {:v (+ 48 ord_idx)}))) (when (= ord_ch " ") (throw (ex-info "return" {:v 32}))) (if (= ord_ch "!") 33 0)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn chr [chr_code]
  (binding [chr_digits nil chr_idx nil chr_lower nil chr_upper nil] (try (do (set! chr_upper "ABCDEFGHIJKLMNOPQRSTUVWXYZ") (set! chr_lower "abcdefghijklmnopqrstuvwxyz") (set! chr_digits "0123456789") (when (= chr_code 32) (throw (ex-info "return" {:v " "}))) (when (= chr_code 33) (throw (ex-info "return" {:v "!"}))) (set! chr_idx (- chr_code 65)) (when (and (>= chr_idx 0) (< chr_idx (count chr_upper))) (throw (ex-info "return" {:v (nth chr_upper chr_idx)}))) (set! chr_idx (- chr_code 97)) (when (and (>= chr_idx 0) (< chr_idx (count chr_lower))) (throw (ex-info "return" {:v (nth chr_lower chr_idx)}))) (set! chr_idx (- chr_code 48)) (if (and (>= chr_idx 0) (< chr_idx (count chr_digits))) (nth chr_digits chr_idx) "")) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn repeat [repeat_s repeat_n]
  (binding [repeat_i nil repeat_out nil] (try (do (set! repeat_out "") (set! repeat_i 0) (while (< repeat_i repeat_n) (do (set! repeat_out (str repeat_out repeat_s)) (set! repeat_i (+ repeat_i 1)))) (throw (ex-info "return" {:v repeat_out}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn to_binary [to_binary_n to_binary_bits]
  (binding [to_binary_i nil to_binary_out nil to_binary_v nil] (try (do (set! to_binary_v to_binary_n) (set! to_binary_out "") (set! to_binary_i 0) (while (< to_binary_i to_binary_bits) (do (set! to_binary_out (str (str (mod to_binary_v 2)) to_binary_out)) (set! to_binary_v (quot to_binary_v 2)) (set! to_binary_i (+ to_binary_i 1)))) (throw (ex-info "return" {:v to_binary_out}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn binary_to_int [binary_to_int_bits]
  (binding [binary_to_int_i nil binary_to_int_n nil] (try (do (set! binary_to_int_n 0) (set! binary_to_int_i 0) (while (< binary_to_int_i (count binary_to_int_bits)) (do (set! binary_to_int_n (* binary_to_int_n 2)) (when (= (nth binary_to_int_bits binary_to_int_i) "1") (set! binary_to_int_n (+ binary_to_int_n 1))) (set! binary_to_int_i (+ binary_to_int_i 1)))) (throw (ex-info "return" {:v binary_to_int_n}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn base32_encode [base32_encode_data]
  (binding [base32_encode_b32_result nil base32_encode_binary_data nil base32_encode_chunk nil base32_encode_i nil base32_encode_index nil base32_encode_j nil base32_encode_rem nil base32_encode_remainder nil] (try (do (set! base32_encode_binary_data "") (set! base32_encode_i 0) (while (< base32_encode_i (count base32_encode_data)) (do (set! base32_encode_binary_data (str base32_encode_binary_data (to_binary (ord (nth base32_encode_data base32_encode_i)) 8))) (set! base32_encode_i (+ base32_encode_i 1)))) (set! base32_encode_remainder (mod (count base32_encode_binary_data) 5)) (when (not= base32_encode_remainder 0) (set! base32_encode_binary_data (str base32_encode_binary_data (repeat "0" (- 5 base32_encode_remainder))))) (set! base32_encode_b32_result "") (set! base32_encode_j 0) (while (< base32_encode_j (count base32_encode_binary_data)) (do (set! base32_encode_chunk (subs base32_encode_binary_data base32_encode_j (min (+ base32_encode_j 5) (count base32_encode_binary_data)))) (set! base32_encode_index (binary_to_int base32_encode_chunk)) (set! base32_encode_b32_result (str base32_encode_b32_result (nth main_B32_CHARSET base32_encode_index))) (set! base32_encode_j (+ base32_encode_j 5)))) (set! base32_encode_rem (mod (count base32_encode_b32_result) 8)) (when (not= base32_encode_rem 0) (set! base32_encode_b32_result (str base32_encode_b32_result (repeat "=" (- 8 base32_encode_rem))))) (throw (ex-info "return" {:v base32_encode_b32_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn base32_decode [base32_decode_data]
  (binding [base32_decode_binary_chunks nil base32_decode_byte_bits nil base32_decode_ch nil base32_decode_clean nil base32_decode_code nil base32_decode_i nil base32_decode_idx nil base32_decode_j nil base32_decode_result nil] (try (do (set! base32_decode_clean "") (set! base32_decode_i 0) (while (< base32_decode_i (count base32_decode_data)) (do (set! base32_decode_ch (nth base32_decode_data base32_decode_i)) (when (not= base32_decode_ch "=") (set! base32_decode_clean (str base32_decode_clean base32_decode_ch))) (set! base32_decode_i (+ base32_decode_i 1)))) (set! base32_decode_binary_chunks "") (set! base32_decode_i 0) (while (< base32_decode_i (count base32_decode_clean)) (do (set! base32_decode_idx (indexOfChar main_B32_CHARSET (nth base32_decode_clean base32_decode_i))) (set! base32_decode_binary_chunks (str base32_decode_binary_chunks (to_binary base32_decode_idx 5))) (set! base32_decode_i (+ base32_decode_i 1)))) (set! base32_decode_result "") (set! base32_decode_j 0) (while (<= (+ base32_decode_j 8) (count base32_decode_binary_chunks)) (do (set! base32_decode_byte_bits (subs base32_decode_binary_chunks base32_decode_j (min (+ base32_decode_j 8) (count base32_decode_binary_chunks)))) (set! base32_decode_code (binary_to_int base32_decode_byte_bits)) (set! base32_decode_result (str base32_decode_result (chr base32_decode_code))) (set! base32_decode_j (+ base32_decode_j 8)))) (throw (ex-info "return" {:v base32_decode_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (base32_encode "Hello World!"))
      (println (base32_encode "123456"))
      (println (base32_encode "some long complex string"))
      (println (base32_decode "JBSWY3DPEBLW64TMMQQQ===="))
      (println (base32_decode "GEZDGNBVGY======"))
      (println (base32_decode "ONXW2ZJANRXW4ZZAMNXW24DMMV4CA43UOJUW4ZY="))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
