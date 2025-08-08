(ns main (:refer-clojure :exclude [index_of ord chr text_to_bits text_from_bits bool_to_string string_to_bitlist bitlist_to_string is_power_of_two list_eq pow2 has_bit hamming_encode hamming_decode main]))

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

(declare index_of ord chr text_to_bits text_from_bits bool_to_string string_to_bitlist bitlist_to_string is_power_of_two list_eq pow2 has_bit hamming_encode hamming_decode main)

(def ^:dynamic bitlist_to_string_i nil)

(def ^:dynamic bitlist_to_string_s nil)

(def ^:dynamic chr_lower nil)

(def ^:dynamic chr_upper nil)

(def ^:dynamic hamming_decode_ack nil)

(def ^:dynamic hamming_decode_data_output nil)

(def ^:dynamic hamming_decode_i nil)

(def ^:dynamic hamming_decode_idx nil)

(def ^:dynamic hamming_decode_j nil)

(def ^:dynamic hamming_decode_parity_calc nil)

(def ^:dynamic hamming_decode_parity_received nil)

(def ^:dynamic hamming_decode_recomputed nil)

(def ^:dynamic hamming_encode_bit nil)

(def ^:dynamic hamming_encode_bp nil)

(def ^:dynamic hamming_encode_cont_bo nil)

(def ^:dynamic hamming_encode_cont_bp nil)

(def ^:dynamic hamming_encode_cont_data nil)

(def ^:dynamic hamming_encode_data_ord nil)

(def ^:dynamic hamming_encode_i nil)

(def ^:dynamic hamming_encode_j nil)

(def ^:dynamic hamming_encode_parity nil)

(def ^:dynamic hamming_encode_pos nil)

(def ^:dynamic hamming_encode_result nil)

(def ^:dynamic hamming_encode_total nil)

(def ^:dynamic hamming_encode_x nil)

(def ^:dynamic has_bit_p nil)

(def ^:dynamic index_of_i nil)

(def ^:dynamic is_power_of_two_p nil)

(def ^:dynamic list_eq_i nil)

(def ^:dynamic main_be nil)

(def ^:dynamic main_binary nil)

(def ^:dynamic main_corrupted nil)

(def ^:dynamic main_data_bits nil)

(def ^:dynamic main_decoded nil)

(def ^:dynamic main_decoded_err nil)

(def ^:dynamic main_encoded nil)

(def ^:dynamic main_i nil)

(def ^:dynamic main_pos nil)

(def ^:dynamic main_sizePari nil)

(def ^:dynamic main_text nil)

(def ^:dynamic ord_idx nil)

(def ^:dynamic ord_lower nil)

(def ^:dynamic ord_upper nil)

(def ^:dynamic pow2_i nil)

(def ^:dynamic pow2_res nil)

(def ^:dynamic string_to_bitlist_i nil)

(def ^:dynamic string_to_bitlist_res nil)

(def ^:dynamic text_from_bits_code nil)

(def ^:dynamic text_from_bits_i nil)

(def ^:dynamic text_from_bits_j nil)

(def ^:dynamic text_from_bits_text nil)

(def ^:dynamic text_to_bits_bits nil)

(def ^:dynamic text_to_bits_code nil)

(def ^:dynamic text_to_bits_i nil)

(def ^:dynamic text_to_bits_j nil)

(def ^:dynamic text_to_bits_p nil)

(defn index_of [index_of_s index_of_ch]
  (binding [index_of_i nil] (try (do (set! index_of_i 0) (while (< index_of_i (count index_of_s)) (do (when (= (nth index_of_s index_of_i) index_of_ch) (throw (ex-info "return" {:v index_of_i}))) (set! index_of_i (+ index_of_i 1)))) (throw (ex-info "return" {:v (- 1)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn ord [ord_ch]
  (binding [ord_idx nil ord_lower nil ord_upper nil] (try (do (set! ord_upper "ABCDEFGHIJKLMNOPQRSTUVWXYZ") (set! ord_lower "abcdefghijklmnopqrstuvwxyz") (set! ord_idx (index_of ord_upper ord_ch)) (when (>= ord_idx 0) (throw (ex-info "return" {:v (+ 65 ord_idx)}))) (set! ord_idx (index_of ord_lower ord_ch)) (if (>= ord_idx 0) (+ 97 ord_idx) 0)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn chr [chr_n]
  (binding [chr_lower nil chr_upper nil] (try (do (set! chr_upper "ABCDEFGHIJKLMNOPQRSTUVWXYZ") (set! chr_lower "abcdefghijklmnopqrstuvwxyz") (when (and (>= chr_n 65) (< chr_n 91)) (throw (ex-info "return" {:v (subs chr_upper (- chr_n 65) (min (- chr_n 64) (count chr_upper)))}))) (if (and (>= chr_n 97) (< chr_n 123)) (subs chr_lower (- chr_n 97) (min (- chr_n 96) (count chr_lower))) "?")) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn text_to_bits [text_to_bits_text]
  (binding [text_to_bits_bits nil text_to_bits_code nil text_to_bits_i nil text_to_bits_j nil text_to_bits_p nil] (try (do (set! text_to_bits_bits "") (set! text_to_bits_i 0) (while (< text_to_bits_i (count text_to_bits_text)) (do (set! text_to_bits_code (ord (nth text_to_bits_text text_to_bits_i))) (set! text_to_bits_j 7) (while (>= text_to_bits_j 0) (do (set! text_to_bits_p (pow2 text_to_bits_j)) (if (= (mod (quot text_to_bits_code text_to_bits_p) 2) 1) (set! text_to_bits_bits (str text_to_bits_bits "1")) (set! text_to_bits_bits (str text_to_bits_bits "0"))) (set! text_to_bits_j (- text_to_bits_j 1)))) (set! text_to_bits_i (+ text_to_bits_i 1)))) (throw (ex-info "return" {:v text_to_bits_bits}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn text_from_bits [text_from_bits_bits]
  (binding [text_from_bits_code nil text_from_bits_i nil text_from_bits_j nil text_from_bits_text nil] (try (do (set! text_from_bits_text "") (set! text_from_bits_i 0) (while (< text_from_bits_i (count text_from_bits_bits)) (do (set! text_from_bits_code 0) (set! text_from_bits_j 0) (while (and (< text_from_bits_j 8) (< (+ text_from_bits_i text_from_bits_j) (count text_from_bits_bits))) (do (set! text_from_bits_code (* text_from_bits_code 2)) (when (= (nth text_from_bits_bits (+ text_from_bits_i text_from_bits_j)) "1") (set! text_from_bits_code (+ text_from_bits_code 1))) (set! text_from_bits_j (+ text_from_bits_j 1)))) (set! text_from_bits_text (str text_from_bits_text (chr text_from_bits_code))) (set! text_from_bits_i (+ text_from_bits_i 8)))) (throw (ex-info "return" {:v text_from_bits_text}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn bool_to_string [bool_to_string_b]
  (try (if bool_to_string_b "True" "False") (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn string_to_bitlist [string_to_bitlist_s]
  (binding [string_to_bitlist_i nil string_to_bitlist_res nil] (try (do (set! string_to_bitlist_res []) (set! string_to_bitlist_i 0) (while (< string_to_bitlist_i (count string_to_bitlist_s)) (do (if (= (nth string_to_bitlist_s string_to_bitlist_i) "1") (set! string_to_bitlist_res (conj string_to_bitlist_res 1)) (set! string_to_bitlist_res (conj string_to_bitlist_res 0))) (set! string_to_bitlist_i (+ string_to_bitlist_i 1)))) (throw (ex-info "return" {:v string_to_bitlist_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn bitlist_to_string [bitlist_to_string_bits]
  (binding [bitlist_to_string_i nil bitlist_to_string_s nil] (try (do (set! bitlist_to_string_s "") (set! bitlist_to_string_i 0) (while (< bitlist_to_string_i (count bitlist_to_string_bits)) (do (if (= (nth bitlist_to_string_bits bitlist_to_string_i) 1) (set! bitlist_to_string_s (str bitlist_to_string_s "1")) (set! bitlist_to_string_s (str bitlist_to_string_s "0"))) (set! bitlist_to_string_i (+ bitlist_to_string_i 1)))) (throw (ex-info "return" {:v bitlist_to_string_s}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn is_power_of_two [is_power_of_two_x]
  (binding [is_power_of_two_p nil] (try (do (when (< is_power_of_two_x 1) (throw (ex-info "return" {:v false}))) (set! is_power_of_two_p 1) (while (< is_power_of_two_p is_power_of_two_x) (set! is_power_of_two_p (* is_power_of_two_p 2))) (throw (ex-info "return" {:v (= is_power_of_two_p is_power_of_two_x)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn list_eq [list_eq_a list_eq_b]
  (binding [list_eq_i nil] (try (do (when (not= (count list_eq_a) (count list_eq_b)) (throw (ex-info "return" {:v false}))) (set! list_eq_i 0) (while (< list_eq_i (count list_eq_a)) (do (when (not= (nth list_eq_a list_eq_i) (nth list_eq_b list_eq_i)) (throw (ex-info "return" {:v false}))) (set! list_eq_i (+ list_eq_i 1)))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn pow2 [pow2_e]
  (binding [pow2_i nil pow2_res nil] (try (do (set! pow2_res 1) (set! pow2_i 0) (while (< pow2_i pow2_e) (do (set! pow2_res (* pow2_res 2)) (set! pow2_i (+ pow2_i 1)))) (throw (ex-info "return" {:v pow2_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn has_bit [has_bit_n has_bit_b]
  (binding [has_bit_p nil] (try (do (set! has_bit_p (pow2 has_bit_b)) (if (= (mod (quot has_bit_n has_bit_p) 2) 1) true false)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn hamming_encode [hamming_encode_r hamming_encode_data_bits]
  (binding [hamming_encode_bit nil hamming_encode_bp nil hamming_encode_cont_bo nil hamming_encode_cont_bp nil hamming_encode_cont_data nil hamming_encode_data_ord nil hamming_encode_i nil hamming_encode_j nil hamming_encode_parity nil hamming_encode_pos nil hamming_encode_result nil hamming_encode_total nil hamming_encode_x nil] (try (do (set! hamming_encode_total (+ hamming_encode_r (count hamming_encode_data_bits))) (set! hamming_encode_data_ord []) (set! hamming_encode_cont_data 0) (set! hamming_encode_x 1) (while (<= hamming_encode_x hamming_encode_total) (do (if (is_power_of_two hamming_encode_x) (set! hamming_encode_data_ord (conj hamming_encode_data_ord (- 1))) (do (set! hamming_encode_data_ord (conj hamming_encode_data_ord (nth hamming_encode_data_bits hamming_encode_cont_data))) (set! hamming_encode_cont_data (+ hamming_encode_cont_data 1)))) (set! hamming_encode_x (+ hamming_encode_x 1)))) (set! hamming_encode_parity []) (set! hamming_encode_bp 0) (while (< hamming_encode_bp hamming_encode_r) (do (set! hamming_encode_cont_bo 0) (set! hamming_encode_j 0) (while (< hamming_encode_j (count hamming_encode_data_ord)) (do (set! hamming_encode_bit (nth hamming_encode_data_ord hamming_encode_j)) (when (>= hamming_encode_bit 0) (do (set! hamming_encode_pos (+ hamming_encode_j 1)) (when (and (has_bit hamming_encode_pos hamming_encode_bp) (= hamming_encode_bit 1)) (set! hamming_encode_cont_bo (+ hamming_encode_cont_bo 1))))) (set! hamming_encode_j (+ hamming_encode_j 1)))) (set! hamming_encode_parity (conj hamming_encode_parity (mod hamming_encode_cont_bo 2))) (set! hamming_encode_bp (+ hamming_encode_bp 1)))) (set! hamming_encode_result []) (set! hamming_encode_cont_bp 0) (set! hamming_encode_i 0) (while (< hamming_encode_i (count hamming_encode_data_ord)) (do (if (< (nth hamming_encode_data_ord hamming_encode_i) 0) (do (set! hamming_encode_result (conj hamming_encode_result (nth hamming_encode_parity hamming_encode_cont_bp))) (set! hamming_encode_cont_bp (+ hamming_encode_cont_bp 1))) (set! hamming_encode_result (conj hamming_encode_result (nth hamming_encode_data_ord hamming_encode_i)))) (set! hamming_encode_i (+ hamming_encode_i 1)))) (throw (ex-info "return" {:v hamming_encode_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn hamming_decode [hamming_decode_r hamming_decode_code]
  (binding [hamming_decode_ack nil hamming_decode_data_output nil hamming_decode_i nil hamming_decode_idx nil hamming_decode_j nil hamming_decode_parity_calc nil hamming_decode_parity_received nil hamming_decode_recomputed nil] (try (do (set! hamming_decode_data_output []) (set! hamming_decode_parity_received []) (set! hamming_decode_i 1) (set! hamming_decode_idx 0) (while (<= hamming_decode_i (count hamming_decode_code)) (do (if (is_power_of_two hamming_decode_i) (set! hamming_decode_parity_received (conj hamming_decode_parity_received (nth hamming_decode_code hamming_decode_idx))) (set! hamming_decode_data_output (conj hamming_decode_data_output (nth hamming_decode_code hamming_decode_idx)))) (set! hamming_decode_idx (+ hamming_decode_idx 1)) (set! hamming_decode_i (+ hamming_decode_i 1)))) (set! hamming_decode_recomputed (hamming_encode hamming_decode_r hamming_decode_data_output)) (set! hamming_decode_parity_calc []) (set! hamming_decode_j 0) (while (< hamming_decode_j (count hamming_decode_recomputed)) (do (when (is_power_of_two (+ hamming_decode_j 1)) (set! hamming_decode_parity_calc (conj hamming_decode_parity_calc (nth hamming_decode_recomputed hamming_decode_j)))) (set! hamming_decode_j (+ hamming_decode_j 1)))) (set! hamming_decode_ack (list_eq hamming_decode_parity_received hamming_decode_parity_calc)) (throw (ex-info "return" {:v {:ack hamming_decode_ack :data hamming_decode_data_output}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_be nil main_binary nil main_corrupted nil main_data_bits nil main_decoded nil main_decoded_err nil main_encoded nil main_i nil main_pos nil main_sizePari nil main_text nil] (do (set! main_sizePari 4) (set! main_be 2) (set! main_text "Message01") (set! main_binary (text_to_bits main_text)) (println (str (str "Text input in binary is '" main_binary) "'")) (set! main_data_bits (string_to_bitlist main_binary)) (set! main_encoded (hamming_encode main_sizePari main_data_bits)) (println (str "Data converted ----------> " (bitlist_to_string main_encoded))) (set! main_decoded (hamming_decode main_sizePari main_encoded)) (println (str (str (str "Data receive ------------> " (bitlist_to_string (:data main_decoded))) " -- Data integrity: ") (bool_to_string (:ack main_decoded)))) (set! main_corrupted []) (set! main_i 0) (while (< main_i (count main_encoded)) (do (set! main_corrupted (conj main_corrupted (nth main_encoded main_i))) (set! main_i (+ main_i 1)))) (set! main_pos (- main_be 1)) (if (= (nth main_corrupted main_pos) 0) (set! main_corrupted (assoc main_corrupted main_pos 1)) (set! main_corrupted (assoc main_corrupted main_pos 0))) (set! main_decoded_err (hamming_decode main_sizePari main_corrupted)) (println (str (str (str "Data receive (error) ----> " (bitlist_to_string (:data main_decoded_err))) " -- Data integrity: ") (bool_to_string (:ack main_decoded_err)))))))

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
