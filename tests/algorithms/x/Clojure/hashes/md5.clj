(ns main (:refer-clojure :exclude [ord to_little_endian int_to_bits bits_to_int to_hex reformat_hex preprocess get_block_words bit_and bit_or bit_xor not_32 sum_32 lshift rshift left_rotate_32 md5_me]))

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

(declare ord to_little_endian int_to_bits bits_to_int to_hex reformat_hex preprocess get_block_words bit_and bit_or bit_xor not_32 sum_32 lshift rshift left_rotate_32 md5_me)

(def ^:dynamic bit_and_bit nil)

(def ^:dynamic bit_and_i nil)

(def ^:dynamic bit_and_res nil)

(def ^:dynamic bit_and_x nil)

(def ^:dynamic bit_and_y nil)

(def ^:dynamic bit_or_abit nil)

(def ^:dynamic bit_or_bbit nil)

(def ^:dynamic bit_or_bit nil)

(def ^:dynamic bit_or_i nil)

(def ^:dynamic bit_or_res nil)

(def ^:dynamic bit_or_x nil)

(def ^:dynamic bit_or_y nil)

(def ^:dynamic bit_xor_abit nil)

(def ^:dynamic bit_xor_bbit nil)

(def ^:dynamic bit_xor_bit nil)

(def ^:dynamic bit_xor_i nil)

(def ^:dynamic bit_xor_res nil)

(def ^:dynamic bit_xor_x nil)

(def ^:dynamic bit_xor_y nil)

(def ^:dynamic bits_to_int_i nil)

(def ^:dynamic bits_to_int_num nil)

(def ^:dynamic get_block_words_block nil)

(def ^:dynamic get_block_words_blocks nil)

(def ^:dynamic get_block_words_i nil)

(def ^:dynamic get_block_words_part nil)

(def ^:dynamic get_block_words_pos nil)

(def ^:dynamic get_block_words_word nil)

(def ^:dynamic int_to_bits_bits nil)

(def ^:dynamic int_to_bits_num nil)

(def ^:dynamic left_rotate_32_left nil)

(def ^:dynamic left_rotate_32_right nil)

(def ^:dynamic lshift_i nil)

(def ^:dynamic lshift_result nil)

(def ^:dynamic md5_me_a nil)

(def ^:dynamic md5_me_a0 nil)

(def ^:dynamic md5_me_added_consts nil)

(def ^:dynamic md5_me_b nil)

(def ^:dynamic md5_me_b0 nil)

(def ^:dynamic md5_me_bi nil)

(def ^:dynamic md5_me_bit_string nil)

(def ^:dynamic md5_me_block nil)

(def ^:dynamic md5_me_blocks nil)

(def ^:dynamic md5_me_c nil)

(def ^:dynamic md5_me_c0 nil)

(def ^:dynamic md5_me_d nil)

(def ^:dynamic md5_me_d0 nil)

(def ^:dynamic md5_me_digest nil)

(def ^:dynamic md5_me_f nil)

(def ^:dynamic md5_me_g nil)

(def ^:dynamic md5_me_i nil)

(def ^:dynamic md5_me_new_b nil)

(def ^:dynamic md5_me_rotated nil)

(def ^:dynamic md5_me_shift_amounts nil)

(def ^:dynamic ord_i nil)

(def ^:dynamic preprocess_bit_string nil)

(def ^:dynamic preprocess_ch nil)

(def ^:dynamic preprocess_i nil)

(def ^:dynamic preprocess_start_len nil)

(def ^:dynamic reformat_hex_hex nil)

(def ^:dynamic reformat_hex_j nil)

(def ^:dynamic reformat_hex_le nil)

(def ^:dynamic rshift_i nil)

(def ^:dynamic rshift_result nil)

(def ^:dynamic to_hex_d nil)

(def ^:dynamic to_hex_digits nil)

(def ^:dynamic to_hex_num nil)

(def ^:dynamic to_hex_s nil)

(def ^:dynamic main_MOD 4294967296)

(def ^:dynamic main_ASCII " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~")

(defn ord [ord_ch]
  (binding [ord_i nil] (try (do (set! ord_i 0) (while (< ord_i (count main_ASCII)) (do (when (= (subs main_ASCII ord_i (min (+ ord_i 1) (count main_ASCII))) ord_ch) (throw (ex-info "return" {:v (+ 32 ord_i)}))) (set! ord_i (+ ord_i 1)))) (throw (ex-info "return" {:v 0}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn to_little_endian [to_little_endian_s]
  (try (do (when (not= (count to_little_endian_s) 32) (throw (Exception. "Input must be of length 32"))) (throw (ex-info "return" {:v (str (str (str (subs to_little_endian_s 24 (min 32 (count to_little_endian_s))) (subs to_little_endian_s 16 (min 24 (count to_little_endian_s)))) (subs to_little_endian_s 8 (min 16 (count to_little_endian_s)))) (subs to_little_endian_s 0 (min 8 (count to_little_endian_s))))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn int_to_bits [int_to_bits_n int_to_bits_width]
  (binding [int_to_bits_bits nil int_to_bits_num nil] (try (do (set! int_to_bits_bits "") (set! int_to_bits_num int_to_bits_n) (while (> int_to_bits_num 0) (do (set! int_to_bits_bits (str (str (mod int_to_bits_num 2)) int_to_bits_bits)) (set! int_to_bits_num (quot int_to_bits_num 2)))) (while (< (count int_to_bits_bits) int_to_bits_width) (set! int_to_bits_bits (str "0" int_to_bits_bits))) (when (> (count int_to_bits_bits) int_to_bits_width) (set! int_to_bits_bits (subs int_to_bits_bits (- (count int_to_bits_bits) int_to_bits_width) (min (count int_to_bits_bits) (count int_to_bits_bits))))) (throw (ex-info "return" {:v int_to_bits_bits}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn bits_to_int [bits_to_int_bits]
  (binding [bits_to_int_i nil bits_to_int_num nil] (try (do (set! bits_to_int_num 0) (set! bits_to_int_i 0) (while (< bits_to_int_i (count bits_to_int_bits)) (do (if (= (subs bits_to_int_bits bits_to_int_i (min (+ bits_to_int_i 1) (count bits_to_int_bits))) "1") (set! bits_to_int_num (+ (* bits_to_int_num 2) 1)) (set! bits_to_int_num (* bits_to_int_num 2))) (set! bits_to_int_i (+ bits_to_int_i 1)))) (throw (ex-info "return" {:v bits_to_int_num}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn to_hex [to_hex_n]
  (binding [to_hex_d nil to_hex_digits nil to_hex_num nil to_hex_s nil] (try (do (set! to_hex_digits "0123456789abcdef") (when (= to_hex_n 0) (throw (ex-info "return" {:v "0"}))) (set! to_hex_num to_hex_n) (set! to_hex_s "") (while (> to_hex_num 0) (do (set! to_hex_d (mod to_hex_num 16)) (set! to_hex_s (str (subs to_hex_digits to_hex_d (min (+ to_hex_d 1) (count to_hex_digits))) to_hex_s)) (set! to_hex_num (quot to_hex_num 16)))) (throw (ex-info "return" {:v to_hex_s}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn reformat_hex [reformat_hex_i]
  (binding [reformat_hex_hex nil reformat_hex_j nil reformat_hex_le nil] (try (do (when (< reformat_hex_i 0) (throw (Exception. "Input must be non-negative"))) (set! reformat_hex_hex (to_hex reformat_hex_i)) (while (< (count reformat_hex_hex) 8) (set! reformat_hex_hex (str "0" reformat_hex_hex))) (when (> (count reformat_hex_hex) 8) (set! reformat_hex_hex (subs reformat_hex_hex (- (count reformat_hex_hex) 8) (min (count reformat_hex_hex) (count reformat_hex_hex))))) (set! reformat_hex_le "") (set! reformat_hex_j (- (count reformat_hex_hex) 2)) (while (>= reformat_hex_j 0) (do (set! reformat_hex_le (str reformat_hex_le (subs reformat_hex_hex reformat_hex_j (min (+ reformat_hex_j 2) (count reformat_hex_hex))))) (set! reformat_hex_j (- reformat_hex_j 2)))) (throw (ex-info "return" {:v reformat_hex_le}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn preprocess [preprocess_message]
  (binding [preprocess_bit_string nil preprocess_ch nil preprocess_i nil preprocess_start_len nil] (try (do (set! preprocess_bit_string "") (set! preprocess_i 0) (while (< preprocess_i (count preprocess_message)) (do (set! preprocess_ch (subs preprocess_message preprocess_i (min (+ preprocess_i 1) (count preprocess_message)))) (set! preprocess_bit_string (str preprocess_bit_string (int_to_bits (ord preprocess_ch) 8))) (set! preprocess_i (+ preprocess_i 1)))) (set! preprocess_start_len (int_to_bits (count preprocess_bit_string) 64)) (set! preprocess_bit_string (str preprocess_bit_string "1")) (while (not= (mod (count preprocess_bit_string) 512) 448) (set! preprocess_bit_string (str preprocess_bit_string "0"))) (set! preprocess_bit_string (str (str preprocess_bit_string (to_little_endian (subs preprocess_start_len 32 (min 64 (count preprocess_start_len))))) (to_little_endian (subs preprocess_start_len 0 (min 32 (count preprocess_start_len)))))) (throw (ex-info "return" {:v preprocess_bit_string}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn get_block_words [get_block_words_bit_string]
  (binding [get_block_words_block nil get_block_words_blocks nil get_block_words_i nil get_block_words_part nil get_block_words_pos nil get_block_words_word nil] (try (do (when (not= (mod (count get_block_words_bit_string) 512) 0) (throw (Exception. "Input must have length that's a multiple of 512"))) (set! get_block_words_blocks []) (set! get_block_words_pos 0) (while (< get_block_words_pos (count get_block_words_bit_string)) (do (set! get_block_words_block []) (set! get_block_words_i 0) (while (< get_block_words_i 512) (do (set! get_block_words_part (subs get_block_words_bit_string (+ get_block_words_pos get_block_words_i) (min (+ (+ get_block_words_pos get_block_words_i) 32) (count get_block_words_bit_string)))) (set! get_block_words_word (bits_to_int (to_little_endian get_block_words_part))) (set! get_block_words_block (conj get_block_words_block get_block_words_word)) (set! get_block_words_i (+ get_block_words_i 32)))) (set! get_block_words_blocks (conj get_block_words_blocks get_block_words_block)) (set! get_block_words_pos (+ get_block_words_pos 512)))) (throw (ex-info "return" {:v get_block_words_blocks}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn bit_and [bit_and_a bit_and_b]
  (binding [bit_and_bit nil bit_and_i nil bit_and_res nil bit_and_x nil bit_and_y nil] (try (do (set! bit_and_x bit_and_a) (set! bit_and_y bit_and_b) (set! bit_and_res 0) (set! bit_and_bit 1) (set! bit_and_i 0) (while (< bit_and_i 32) (do (when (and (= (mod bit_and_x 2) 1) (= (mod bit_and_y 2) 1)) (set! bit_and_res (+ bit_and_res bit_and_bit))) (set! bit_and_x (quot bit_and_x 2)) (set! bit_and_y (quot bit_and_y 2)) (set! bit_and_bit (* bit_and_bit 2)) (set! bit_and_i (+ bit_and_i 1)))) (throw (ex-info "return" {:v bit_and_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn bit_or [bit_or_a bit_or_b]
  (binding [bit_or_abit nil bit_or_bbit nil bit_or_bit nil bit_or_i nil bit_or_res nil bit_or_x nil bit_or_y nil] (try (do (set! bit_or_x bit_or_a) (set! bit_or_y bit_or_b) (set! bit_or_res 0) (set! bit_or_bit 1) (set! bit_or_i 0) (while (< bit_or_i 32) (do (set! bit_or_abit (mod bit_or_x 2)) (set! bit_or_bbit (mod bit_or_y 2)) (when (or (= bit_or_abit 1) (= bit_or_bbit 1)) (set! bit_or_res (+ bit_or_res bit_or_bit))) (set! bit_or_x (quot bit_or_x 2)) (set! bit_or_y (quot bit_or_y 2)) (set! bit_or_bit (* bit_or_bit 2)) (set! bit_or_i (+ bit_or_i 1)))) (throw (ex-info "return" {:v bit_or_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn bit_xor [bit_xor_a bit_xor_b]
  (binding [bit_xor_abit nil bit_xor_bbit nil bit_xor_bit nil bit_xor_i nil bit_xor_res nil bit_xor_x nil bit_xor_y nil] (try (do (set! bit_xor_x bit_xor_a) (set! bit_xor_y bit_xor_b) (set! bit_xor_res 0) (set! bit_xor_bit 1) (set! bit_xor_i 0) (while (< bit_xor_i 32) (do (set! bit_xor_abit (mod bit_xor_x 2)) (set! bit_xor_bbit (mod bit_xor_y 2)) (when (= (mod (+ bit_xor_abit bit_xor_bbit) 2) 1) (set! bit_xor_res (+ bit_xor_res bit_xor_bit))) (set! bit_xor_x (quot bit_xor_x 2)) (set! bit_xor_y (quot bit_xor_y 2)) (set! bit_xor_bit (* bit_xor_bit 2)) (set! bit_xor_i (+ bit_xor_i 1)))) (throw (ex-info "return" {:v bit_xor_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn not_32 [not_32_i]
  (try (do (when (< not_32_i 0) (throw (Exception. "Input must be non-negative"))) (throw (ex-info "return" {:v (- 4294967295 not_32_i)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn sum_32 [sum_32_a sum_32_b]
  (try (throw (ex-info "return" {:v (mod (+ sum_32_a sum_32_b) main_MOD)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn lshift [lshift_num lshift_k]
  (binding [lshift_i nil lshift_result nil] (try (do (set! lshift_result (mod lshift_num main_MOD)) (set! lshift_i 0) (while (< lshift_i lshift_k) (do (set! lshift_result (mod (* lshift_result 2) main_MOD)) (set! lshift_i (+ lshift_i 1)))) (throw (ex-info "return" {:v lshift_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn rshift [rshift_num rshift_k]
  (binding [rshift_i nil rshift_result nil] (try (do (set! rshift_result rshift_num) (set! rshift_i 0) (while (< rshift_i rshift_k) (do (set! rshift_result (quot rshift_result 2)) (set! rshift_i (+ rshift_i 1)))) (throw (ex-info "return" {:v rshift_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn left_rotate_32 [left_rotate_32_i left_rotate_32_shift]
  (binding [left_rotate_32_left nil left_rotate_32_right nil] (try (do (when (< left_rotate_32_i 0) (throw (Exception. "Input must be non-negative"))) (when (< left_rotate_32_shift 0) (throw (Exception. "Shift must be non-negative"))) (set! left_rotate_32_left (lshift left_rotate_32_i left_rotate_32_shift)) (set! left_rotate_32_right (rshift left_rotate_32_i (- 32 left_rotate_32_shift))) (throw (ex-info "return" {:v (mod (+ left_rotate_32_left left_rotate_32_right) main_MOD)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn md5_me [md5_me_message]
  (binding [md5_me_a nil md5_me_a0 nil md5_me_added_consts nil md5_me_b nil md5_me_b0 nil md5_me_bi nil md5_me_bit_string nil md5_me_block nil md5_me_blocks nil md5_me_c nil md5_me_c0 nil md5_me_d nil md5_me_d0 nil md5_me_digest nil md5_me_f nil md5_me_g nil md5_me_i nil md5_me_new_b nil md5_me_rotated nil md5_me_shift_amounts nil] (try (do (set! md5_me_bit_string (preprocess md5_me_message)) (set! md5_me_added_consts [3614090360 3905402710 606105819 3250441966 4118548399 1200080426 2821735955 4249261313 1770035416 2336552879 4294925233 2304563134 1804603682 4254626195 2792965006 1236535329 4129170786 3225465664 643717713 3921069994 3593408605 38016083 3634488961 3889429448 568446438 3275163606 4107603335 1163531501 2850285829 4243563512 1735328473 2368359562 4294588738 2272392833 1839030562 4259657740 2763975236 1272893353 4139469664 3200236656 681279174 3936430074 3572445317 76029189 3654602809 3873151461 530742520 3299628645 4096336452 1126891415 2878612391 4237533241 1700485571 2399980690 4293915773 2240044497 1873313359 4264355552 2734768916 1309151649 4149444226 3174756917 718787259 3951481745]) (set! md5_me_shift_amounts [7 12 17 22 7 12 17 22 7 12 17 22 7 12 17 22 5 9 14 20 5 9 14 20 5 9 14 20 5 9 14 20 4 11 16 23 4 11 16 23 4 11 16 23 4 11 16 23 6 10 15 21 6 10 15 21 6 10 15 21 6 10 15 21]) (set! md5_me_a0 1732584193) (set! md5_me_b0 4023233417) (set! md5_me_c0 2562383102) (set! md5_me_d0 271733878) (set! md5_me_blocks (get_block_words md5_me_bit_string)) (set! md5_me_bi 0) (while (< md5_me_bi (count md5_me_blocks)) (do (set! md5_me_block (nth md5_me_blocks md5_me_bi)) (set! md5_me_a md5_me_a0) (set! md5_me_b md5_me_b0) (set! md5_me_c md5_me_c0) (set! md5_me_d md5_me_d0) (set! md5_me_i 0) (while (< md5_me_i 64) (do (set! md5_me_f 0) (set! md5_me_g 0) (if (<= md5_me_i 15) (do (set! md5_me_f (bit_xor md5_me_d (bit_and md5_me_b (bit_xor md5_me_c md5_me_d)))) (set! md5_me_g md5_me_i)) (if (<= md5_me_i 31) (do (set! md5_me_f (bit_xor md5_me_c (bit_and md5_me_d (bit_xor md5_me_b md5_me_c)))) (set! md5_me_g (mod (+ (* 5 md5_me_i) 1) 16))) (if (<= md5_me_i 47) (do (set! md5_me_f (bit_xor (bit_xor md5_me_b md5_me_c) md5_me_d)) (set! md5_me_g (mod (+ (* 3 md5_me_i) 5) 16))) (do (set! md5_me_f (bit_xor md5_me_c (bit_or md5_me_b (not_32 md5_me_d)))) (set! md5_me_g (mod (* 7 md5_me_i) 16)))))) (set! md5_me_f (sum_32 md5_me_f md5_me_a)) (set! md5_me_f (sum_32 md5_me_f (nth md5_me_added_consts md5_me_i))) (set! md5_me_f (sum_32 md5_me_f (nth md5_me_block md5_me_g))) (set! md5_me_rotated (left_rotate_32 md5_me_f (nth md5_me_shift_amounts md5_me_i))) (set! md5_me_new_b (sum_32 md5_me_b md5_me_rotated)) (set! md5_me_a md5_me_d) (set! md5_me_d md5_me_c) (set! md5_me_c md5_me_b) (set! md5_me_b md5_me_new_b) (set! md5_me_i (+ md5_me_i 1)))) (set! md5_me_a0 (sum_32 md5_me_a0 md5_me_a)) (set! md5_me_b0 (sum_32 md5_me_b0 md5_me_b)) (set! md5_me_c0 (sum_32 md5_me_c0 md5_me_c)) (set! md5_me_d0 (sum_32 md5_me_d0 md5_me_d)) (set! md5_me_bi (+ md5_me_bi 1)))) (set! md5_me_digest (str (str (str (reformat_hex md5_me_a0) (reformat_hex md5_me_b0)) (reformat_hex md5_me_c0)) (reformat_hex md5_me_d0))) (throw (ex-info "return" {:v md5_me_digest}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]

      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
