(ns main (:refer-clojure :exclude [mod36 gcd replace_letters replace_digits to_upper process_text matrix_minor determinant cofactor_matrix transpose matrix_mod scalar_matrix_mult adjugate multiply_matrix_vector inverse_key hill_encrypt hill_decrypt]))

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

(declare mod36 gcd replace_letters replace_digits to_upper process_text matrix_minor determinant cofactor_matrix transpose matrix_mod scalar_matrix_mult adjugate multiply_matrix_vector inverse_key hill_encrypt hill_decrypt)

(def ^:dynamic adjugate_cof nil)

(def ^:dynamic adjugate_i nil)

(def ^:dynamic adjugate_j nil)

(def ^:dynamic adjugate_n nil)

(def ^:dynamic adjugate_res nil)

(def ^:dynamic adjugate_row nil)

(def ^:dynamic cofactor_matrix_det_minor nil)

(def ^:dynamic cofactor_matrix_i nil)

(def ^:dynamic cofactor_matrix_j nil)

(def ^:dynamic cofactor_matrix_minor_mat nil)

(def ^:dynamic cofactor_matrix_n nil)

(def ^:dynamic cofactor_matrix_res nil)

(def ^:dynamic cofactor_matrix_row nil)

(def ^:dynamic cofactor_matrix_sign nil)

(def ^:dynamic determinant_col nil)

(def ^:dynamic determinant_det nil)

(def ^:dynamic determinant_minor_mat nil)

(def ^:dynamic determinant_n nil)

(def ^:dynamic determinant_sign nil)

(def ^:dynamic gcd_t nil)

(def ^:dynamic gcd_x nil)

(def ^:dynamic gcd_y nil)

(def ^:dynamic hill_decrypt_break_key nil)

(def ^:dynamic hill_decrypt_dec_vec nil)

(def ^:dynamic hill_decrypt_decrypt_key nil)

(def ^:dynamic hill_decrypt_decrypted nil)

(def ^:dynamic hill_decrypt_i nil)

(def ^:dynamic hill_decrypt_j nil)

(def ^:dynamic hill_decrypt_k nil)

(def ^:dynamic hill_decrypt_processed nil)

(def ^:dynamic hill_decrypt_vec nil)

(def ^:dynamic hill_encrypt_break_key nil)

(def ^:dynamic hill_encrypt_enc_vec nil)

(def ^:dynamic hill_encrypt_encrypted nil)

(def ^:dynamic hill_encrypt_i nil)

(def ^:dynamic hill_encrypt_j nil)

(def ^:dynamic hill_encrypt_k nil)

(def ^:dynamic hill_encrypt_processed nil)

(def ^:dynamic hill_encrypt_vec nil)

(def ^:dynamic inverse_key_adj nil)

(def ^:dynamic inverse_key_det_inv nil)

(def ^:dynamic inverse_key_det_mod nil)

(def ^:dynamic inverse_key_det_val nil)

(def ^:dynamic inverse_key_i nil)

(def ^:dynamic inverse_key_res nil)

(def ^:dynamic inverse_key_tmp nil)

(def ^:dynamic matrix_minor_i nil)

(def ^:dynamic matrix_minor_j nil)

(def ^:dynamic matrix_minor_r nil)

(def ^:dynamic matrix_minor_res nil)

(def ^:dynamic matrix_mod_i nil)

(def ^:dynamic matrix_mod_j nil)

(def ^:dynamic matrix_mod_res nil)

(def ^:dynamic matrix_mod_row nil)

(def ^:dynamic mod36_r nil)

(def ^:dynamic multiply_matrix_vector_i nil)

(def ^:dynamic multiply_matrix_vector_j nil)

(def ^:dynamic multiply_matrix_vector_n nil)

(def ^:dynamic multiply_matrix_vector_res nil)

(def ^:dynamic multiply_matrix_vector_sum nil)

(def ^:dynamic process_text_c nil)

(def ^:dynamic process_text_chars nil)

(def ^:dynamic process_text_i nil)

(def ^:dynamic process_text_j nil)

(def ^:dynamic process_text_k nil)

(def ^:dynamic process_text_last nil)

(def ^:dynamic process_text_ok nil)

(def ^:dynamic process_text_res nil)

(def ^:dynamic replace_digits_idx nil)

(def ^:dynamic replace_letters_i nil)

(def ^:dynamic scalar_matrix_mult_i nil)

(def ^:dynamic scalar_matrix_mult_j nil)

(def ^:dynamic scalar_matrix_mult_res nil)

(def ^:dynamic scalar_matrix_mult_row nil)

(def ^:dynamic to_upper_i nil)

(def ^:dynamic to_upper_lower nil)

(def ^:dynamic to_upper_upper nil)

(def ^:dynamic transpose_cols nil)

(def ^:dynamic transpose_i nil)

(def ^:dynamic transpose_j nil)

(def ^:dynamic transpose_res nil)

(def ^:dynamic transpose_row nil)

(def ^:dynamic transpose_rows nil)

(def ^:dynamic main_KEY_STRING "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")

(defn mod36 [mod36_n]
  (binding [mod36_r nil] (try (do (set! mod36_r (mod mod36_n 36)) (when (< mod36_r 0) (set! mod36_r (+ mod36_r 36))) (throw (ex-info "return" {:v mod36_r}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn gcd [gcd_a gcd_b]
  (binding [gcd_t nil gcd_x nil gcd_y nil] (try (do (set! gcd_x gcd_a) (set! gcd_y gcd_b) (while (not= gcd_y 0) (do (set! gcd_t gcd_y) (set! gcd_y (mod gcd_x gcd_y)) (set! gcd_x gcd_t))) (when (< gcd_x 0) (set! gcd_x (- gcd_x))) (throw (ex-info "return" {:v gcd_x}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn replace_letters [replace_letters_letter]
  (binding [replace_letters_i nil] (try (do (set! replace_letters_i 0) (while (< replace_letters_i (count main_KEY_STRING)) (do (when (= (nth main_KEY_STRING replace_letters_i) replace_letters_letter) (throw (ex-info "return" {:v replace_letters_i}))) (set! replace_letters_i (+ replace_letters_i 1)))) (throw (ex-info "return" {:v 0}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn replace_digits [replace_digits_num]
  (binding [replace_digits_idx nil] (try (do (set! replace_digits_idx (mod36 replace_digits_num)) (throw (ex-info "return" {:v (nth main_KEY_STRING replace_digits_idx)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn to_upper [to_upper_c]
  (binding [to_upper_i nil to_upper_lower nil to_upper_upper nil] (try (do (set! to_upper_lower "abcdefghijklmnopqrstuvwxyz") (set! to_upper_upper "ABCDEFGHIJKLMNOPQRSTUVWXYZ") (set! to_upper_i 0) (while (< to_upper_i (count to_upper_lower)) (do (when (= to_upper_c (nth to_upper_lower to_upper_i)) (throw (ex-info "return" {:v (nth to_upper_upper to_upper_i)}))) (set! to_upper_i (+ to_upper_i 1)))) (throw (ex-info "return" {:v to_upper_c}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn process_text [process_text_text process_text_break_key]
  (binding [process_text_c nil process_text_chars nil process_text_i nil process_text_j nil process_text_k nil process_text_last nil process_text_ok nil process_text_res nil] (try (do (set! process_text_chars []) (set! process_text_i 0) (while (< process_text_i (count process_text_text)) (do (set! process_text_c (to_upper (nth process_text_text process_text_i))) (set! process_text_j 0) (set! process_text_ok false) (loop [while_flag_1 true] (when (and while_flag_1 (< process_text_j (count main_KEY_STRING))) (cond (= (nth main_KEY_STRING process_text_j) process_text_c) (do (set! process_text_ok true) (recur false)) :else (do (set! process_text_j (+ process_text_j 1)) (recur while_flag_1))))) (when process_text_ok (set! process_text_chars (conj process_text_chars process_text_c))) (set! process_text_i (+ process_text_i 1)))) (when (= (count process_text_chars) 0) (throw (ex-info "return" {:v ""}))) (set! process_text_last (nth process_text_chars (- (count process_text_chars) 1))) (while (not= (mod (count process_text_chars) process_text_break_key) 0) (set! process_text_chars (conj process_text_chars process_text_last))) (set! process_text_res "") (set! process_text_k 0) (while (< process_text_k (count process_text_chars)) (do (set! process_text_res (str process_text_res (nth process_text_chars process_text_k))) (set! process_text_k (+ process_text_k 1)))) (throw (ex-info "return" {:v process_text_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn matrix_minor [matrix_minor_m matrix_minor_row matrix_minor_col]
  (binding [matrix_minor_i nil matrix_minor_j nil matrix_minor_r nil matrix_minor_res nil] (try (do (set! matrix_minor_res []) (set! matrix_minor_i 0) (while (< matrix_minor_i (count matrix_minor_m)) (do (when (not= matrix_minor_i matrix_minor_row) (do (set! matrix_minor_r []) (set! matrix_minor_j 0) (while (< matrix_minor_j (count (nth matrix_minor_m matrix_minor_i))) (do (when (not= matrix_minor_j matrix_minor_col) (set! matrix_minor_r (conj matrix_minor_r (nth (nth matrix_minor_m matrix_minor_i) matrix_minor_j)))) (set! matrix_minor_j (+ matrix_minor_j 1)))) (set! matrix_minor_res (conj matrix_minor_res matrix_minor_r)))) (set! matrix_minor_i (+ matrix_minor_i 1)))) (throw (ex-info "return" {:v matrix_minor_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn determinant [determinant_m]
  (binding [determinant_col nil determinant_det nil determinant_minor_mat nil determinant_n nil determinant_sign nil] (try (do (set! determinant_n (count determinant_m)) (when (= determinant_n 1) (throw (ex-info "return" {:v (nth (nth determinant_m 0) 0)}))) (when (= determinant_n 2) (throw (ex-info "return" {:v (- (* (nth (nth determinant_m 0) 0) (nth (nth determinant_m 1) 1)) (* (nth (nth determinant_m 0) 1) (nth (nth determinant_m 1) 0)))}))) (set! determinant_det 0) (set! determinant_col 0) (while (< determinant_col determinant_n) (do (set! determinant_minor_mat (matrix_minor determinant_m 0 determinant_col)) (set! determinant_sign 1) (when (= (mod determinant_col 2) 1) (set! determinant_sign (- 1))) (set! determinant_det (+ determinant_det (* (* determinant_sign (nth (nth determinant_m 0) determinant_col)) (determinant determinant_minor_mat)))) (set! determinant_col (+ determinant_col 1)))) (throw (ex-info "return" {:v determinant_det}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn cofactor_matrix [cofactor_matrix_m]
  (binding [cofactor_matrix_det_minor nil cofactor_matrix_i nil cofactor_matrix_j nil cofactor_matrix_minor_mat nil cofactor_matrix_n nil cofactor_matrix_res nil cofactor_matrix_row nil cofactor_matrix_sign nil] (try (do (set! cofactor_matrix_n (count cofactor_matrix_m)) (set! cofactor_matrix_res []) (set! cofactor_matrix_i 0) (while (< cofactor_matrix_i cofactor_matrix_n) (do (set! cofactor_matrix_row []) (set! cofactor_matrix_j 0) (while (< cofactor_matrix_j cofactor_matrix_n) (do (set! cofactor_matrix_minor_mat (matrix_minor cofactor_matrix_m cofactor_matrix_i cofactor_matrix_j)) (set! cofactor_matrix_det_minor (determinant cofactor_matrix_minor_mat)) (set! cofactor_matrix_sign 1) (when (= (mod (+ cofactor_matrix_i cofactor_matrix_j) 2) 1) (set! cofactor_matrix_sign (- 1))) (set! cofactor_matrix_row (conj cofactor_matrix_row (* cofactor_matrix_sign cofactor_matrix_det_minor))) (set! cofactor_matrix_j (+ cofactor_matrix_j 1)))) (set! cofactor_matrix_res (conj cofactor_matrix_res cofactor_matrix_row)) (set! cofactor_matrix_i (+ cofactor_matrix_i 1)))) (throw (ex-info "return" {:v cofactor_matrix_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn transpose [transpose_m]
  (binding [transpose_cols nil transpose_i nil transpose_j nil transpose_res nil transpose_row nil transpose_rows nil] (try (do (set! transpose_rows (count transpose_m)) (set! transpose_cols (count (nth transpose_m 0))) (set! transpose_res []) (set! transpose_j 0) (while (< transpose_j transpose_cols) (do (set! transpose_row []) (set! transpose_i 0) (while (< transpose_i transpose_rows) (do (set! transpose_row (conj transpose_row (nth (nth transpose_m transpose_i) transpose_j))) (set! transpose_i (+ transpose_i 1)))) (set! transpose_res (conj transpose_res transpose_row)) (set! transpose_j (+ transpose_j 1)))) (throw (ex-info "return" {:v transpose_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn matrix_mod [matrix_mod_m]
  (binding [matrix_mod_i nil matrix_mod_j nil matrix_mod_res nil matrix_mod_row nil] (try (do (set! matrix_mod_res []) (set! matrix_mod_i 0) (while (< matrix_mod_i (count matrix_mod_m)) (do (set! matrix_mod_row []) (set! matrix_mod_j 0) (while (< matrix_mod_j (count (nth matrix_mod_m matrix_mod_i))) (do (set! matrix_mod_row (conj matrix_mod_row (mod36 (nth (nth matrix_mod_m matrix_mod_i) matrix_mod_j)))) (set! matrix_mod_j (+ matrix_mod_j 1)))) (set! matrix_mod_res (conj matrix_mod_res matrix_mod_row)) (set! matrix_mod_i (+ matrix_mod_i 1)))) (throw (ex-info "return" {:v matrix_mod_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn scalar_matrix_mult [scalar_matrix_mult_s scalar_matrix_mult_m]
  (binding [scalar_matrix_mult_i nil scalar_matrix_mult_j nil scalar_matrix_mult_res nil scalar_matrix_mult_row nil] (try (do (set! scalar_matrix_mult_res []) (set! scalar_matrix_mult_i 0) (while (< scalar_matrix_mult_i (count scalar_matrix_mult_m)) (do (set! scalar_matrix_mult_row []) (set! scalar_matrix_mult_j 0) (while (< scalar_matrix_mult_j (count (nth scalar_matrix_mult_m scalar_matrix_mult_i))) (do (set! scalar_matrix_mult_row (conj scalar_matrix_mult_row (mod36 (* scalar_matrix_mult_s (nth (nth scalar_matrix_mult_m scalar_matrix_mult_i) scalar_matrix_mult_j))))) (set! scalar_matrix_mult_j (+ scalar_matrix_mult_j 1)))) (set! scalar_matrix_mult_res (conj scalar_matrix_mult_res scalar_matrix_mult_row)) (set! scalar_matrix_mult_i (+ scalar_matrix_mult_i 1)))) (throw (ex-info "return" {:v scalar_matrix_mult_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn adjugate [adjugate_m]
  (binding [adjugate_cof nil adjugate_i nil adjugate_j nil adjugate_n nil adjugate_res nil adjugate_row nil] (try (do (set! adjugate_cof (cofactor_matrix adjugate_m)) (set! adjugate_n (count adjugate_cof)) (set! adjugate_res []) (set! adjugate_i 0) (while (< adjugate_i adjugate_n) (do (set! adjugate_row []) (set! adjugate_j 0) (while (< adjugate_j adjugate_n) (do (set! adjugate_row (conj adjugate_row (nth (nth adjugate_cof adjugate_j) adjugate_i))) (set! adjugate_j (+ adjugate_j 1)))) (set! adjugate_res (conj adjugate_res adjugate_row)) (set! adjugate_i (+ adjugate_i 1)))) (throw (ex-info "return" {:v adjugate_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn multiply_matrix_vector [multiply_matrix_vector_m multiply_matrix_vector_v]
  (binding [multiply_matrix_vector_i nil multiply_matrix_vector_j nil multiply_matrix_vector_n nil multiply_matrix_vector_res nil multiply_matrix_vector_sum nil] (try (do (set! multiply_matrix_vector_n (count multiply_matrix_vector_m)) (set! multiply_matrix_vector_res []) (set! multiply_matrix_vector_i 0) (while (< multiply_matrix_vector_i multiply_matrix_vector_n) (do (set! multiply_matrix_vector_sum 0) (set! multiply_matrix_vector_j 0) (while (< multiply_matrix_vector_j multiply_matrix_vector_n) (do (set! multiply_matrix_vector_sum (+ multiply_matrix_vector_sum (* (nth (nth multiply_matrix_vector_m multiply_matrix_vector_i) multiply_matrix_vector_j) (nth multiply_matrix_vector_v multiply_matrix_vector_j)))) (set! multiply_matrix_vector_j (+ multiply_matrix_vector_j 1)))) (set! multiply_matrix_vector_res (conj multiply_matrix_vector_res (mod36 multiply_matrix_vector_sum))) (set! multiply_matrix_vector_i (+ multiply_matrix_vector_i 1)))) (throw (ex-info "return" {:v multiply_matrix_vector_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn inverse_key [inverse_key_key]
  (binding [inverse_key_adj nil inverse_key_det_inv nil inverse_key_det_mod nil inverse_key_det_val nil inverse_key_i nil inverse_key_res nil inverse_key_tmp nil] (try (do (set! inverse_key_det_val (determinant inverse_key_key)) (set! inverse_key_det_mod (mod36 inverse_key_det_val)) (set! inverse_key_det_inv 0) (set! inverse_key_i 0) (loop [while_flag_2 true] (when (and while_flag_2 (< inverse_key_i 36)) (cond (= (mod (* inverse_key_det_mod inverse_key_i) 36) 1) (do (set! inverse_key_det_inv inverse_key_i) (recur false)) :else (do (set! inverse_key_i (+ inverse_key_i 1)) (recur while_flag_2))))) (set! inverse_key_adj (adjugate inverse_key_key)) (set! inverse_key_tmp (scalar_matrix_mult inverse_key_det_inv inverse_key_adj)) (set! inverse_key_res (matrix_mod inverse_key_tmp)) (throw (ex-info "return" {:v inverse_key_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn hill_encrypt [hill_encrypt_key hill_encrypt_text]
  (binding [hill_encrypt_break_key nil hill_encrypt_enc_vec nil hill_encrypt_encrypted nil hill_encrypt_i nil hill_encrypt_j nil hill_encrypt_k nil hill_encrypt_processed nil hill_encrypt_vec nil] (try (do (set! hill_encrypt_break_key (count hill_encrypt_key)) (set! hill_encrypt_processed (process_text hill_encrypt_text hill_encrypt_break_key)) (set! hill_encrypt_encrypted "") (set! hill_encrypt_i 0) (while (< hill_encrypt_i (count hill_encrypt_processed)) (do (set! hill_encrypt_vec []) (set! hill_encrypt_j 0) (while (< hill_encrypt_j hill_encrypt_break_key) (do (set! hill_encrypt_vec (conj hill_encrypt_vec (replace_letters (nth hill_encrypt_processed (+ hill_encrypt_i hill_encrypt_j))))) (set! hill_encrypt_j (+ hill_encrypt_j 1)))) (set! hill_encrypt_enc_vec (multiply_matrix_vector hill_encrypt_key hill_encrypt_vec)) (set! hill_encrypt_k 0) (while (< hill_encrypt_k hill_encrypt_break_key) (do (set! hill_encrypt_encrypted (str hill_encrypt_encrypted (replace_digits (nth hill_encrypt_enc_vec hill_encrypt_k)))) (set! hill_encrypt_k (+ hill_encrypt_k 1)))) (set! hill_encrypt_i (+ hill_encrypt_i hill_encrypt_break_key)))) (throw (ex-info "return" {:v hill_encrypt_encrypted}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn hill_decrypt [hill_decrypt_key hill_decrypt_text]
  (binding [hill_decrypt_break_key nil hill_decrypt_dec_vec nil hill_decrypt_decrypt_key nil hill_decrypt_decrypted nil hill_decrypt_i nil hill_decrypt_j nil hill_decrypt_k nil hill_decrypt_processed nil hill_decrypt_vec nil] (try (do (set! hill_decrypt_break_key (count hill_decrypt_key)) (set! hill_decrypt_decrypt_key (inverse_key hill_decrypt_key)) (set! hill_decrypt_processed (process_text hill_decrypt_text hill_decrypt_break_key)) (set! hill_decrypt_decrypted "") (set! hill_decrypt_i 0) (while (< hill_decrypt_i (count hill_decrypt_processed)) (do (set! hill_decrypt_vec []) (set! hill_decrypt_j 0) (while (< hill_decrypt_j hill_decrypt_break_key) (do (set! hill_decrypt_vec (conj hill_decrypt_vec (replace_letters (nth hill_decrypt_processed (+ hill_decrypt_i hill_decrypt_j))))) (set! hill_decrypt_j (+ hill_decrypt_j 1)))) (set! hill_decrypt_dec_vec (multiply_matrix_vector hill_decrypt_decrypt_key hill_decrypt_vec)) (set! hill_decrypt_k 0) (while (< hill_decrypt_k hill_decrypt_break_key) (do (set! hill_decrypt_decrypted (str hill_decrypt_decrypted (replace_digits (nth hill_decrypt_dec_vec hill_decrypt_k)))) (set! hill_decrypt_k (+ hill_decrypt_k 1)))) (set! hill_decrypt_i (+ hill_decrypt_i hill_decrypt_break_key)))) (throw (ex-info "return" {:v hill_decrypt_decrypted}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_key [[2 5] [1 6]])

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (hill_encrypt main_key "testing hill cipher"))
      (println (hill_encrypt main_key "hello"))
      (println (hill_decrypt main_key "WHXYJOLM9C6XT085LL"))
      (println (hill_decrypt main_key "85FF00"))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
