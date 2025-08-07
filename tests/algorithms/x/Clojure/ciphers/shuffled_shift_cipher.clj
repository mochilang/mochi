(ns main (:refer-clojure :exclude [ord neg_pos passcode_creator unique_sorted make_key_list make_shift_key new_cipher index_of encrypt decrypt test_end_to_end]))

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

(declare ord neg_pos passcode_creator unique_sorted make_key_list make_shift_key new_cipher index_of encrypt decrypt test_end_to_end)

(def ^:dynamic decrypt_ch nil)

(def ^:dynamic decrypt_decoded nil)

(def ^:dynamic decrypt_i nil)

(def ^:dynamic decrypt_n nil)

(def ^:dynamic decrypt_new_pos nil)

(def ^:dynamic decrypt_position nil)

(def ^:dynamic encrypt_ch nil)

(def ^:dynamic encrypt_encoded nil)

(def ^:dynamic encrypt_i nil)

(def ^:dynamic encrypt_n nil)

(def ^:dynamic encrypt_new_pos nil)

(def ^:dynamic encrypt_position nil)

(def ^:dynamic index_of_i nil)

(def ^:dynamic make_key_list_breakpoints nil)

(def ^:dynamic make_key_list_ch nil)

(def ^:dynamic make_key_list_i nil)

(def ^:dynamic make_key_list_k nil)

(def ^:dynamic make_key_list_key_list_options nil)

(def ^:dynamic make_key_list_keys_l nil)

(def ^:dynamic make_key_list_temp_list nil)

(def ^:dynamic make_shift_key_codes nil)

(def ^:dynamic make_shift_key_i nil)

(def ^:dynamic make_shift_key_total nil)

(def ^:dynamic neg_pos_i nil)

(def ^:dynamic neg_pos_iterlist nil)

(def ^:dynamic new_cipher_i nil)

(def ^:dynamic new_cipher_key_list nil)

(def ^:dynamic new_cipher_passcode nil)

(def ^:dynamic new_cipher_shift_key nil)

(def ^:dynamic ord_digits nil)

(def ^:dynamic ord_i nil)

(def ^:dynamic ord_lower nil)

(def ^:dynamic ord_upper nil)

(def ^:dynamic passcode_creator_choices nil)

(def ^:dynamic passcode_creator_i nil)

(def ^:dynamic passcode_creator_idx nil)

(def ^:dynamic passcode_creator_length nil)

(def ^:dynamic passcode_creator_password nil)

(def ^:dynamic passcode_creator_seed nil)

(def ^:dynamic test_end_to_end_cip nil)

(def ^:dynamic test_end_to_end_msg nil)

(def ^:dynamic unique_sorted_ch nil)

(def ^:dynamic unique_sorted_i nil)

(def ^:dynamic unique_sorted_j nil)

(def ^:dynamic unique_sorted_k nil)

(def ^:dynamic unique_sorted_min_idx nil)

(def ^:dynamic unique_sorted_tmp nil)

(def ^:dynamic unique_sorted_uniq nil)

(defn ord [ord_ch]
  (binding [ord_digits nil ord_i nil ord_lower nil ord_upper nil] (try (do (set! ord_digits "0123456789") (set! ord_i 0) (while (< ord_i (count ord_digits)) (do (when (= (subs ord_digits ord_i (min (+ ord_i 1) (count ord_digits))) ord_ch) (throw (ex-info "return" {:v (+ 48 ord_i)}))) (set! ord_i (+ ord_i 1)))) (set! ord_upper "ABCDEFGHIJKLMNOPQRSTUVWXYZ") (set! ord_i 0) (while (< ord_i (count ord_upper)) (do (when (= (subs ord_upper ord_i (min (+ ord_i 1) (count ord_upper))) ord_ch) (throw (ex-info "return" {:v (+ 65 ord_i)}))) (set! ord_i (+ ord_i 1)))) (set! ord_lower "abcdefghijklmnopqrstuvwxyz") (set! ord_i 0) (while (< ord_i (count ord_lower)) (do (when (= (subs ord_lower ord_i (min (+ ord_i 1) (count ord_lower))) ord_ch) (throw (ex-info "return" {:v (+ 97 ord_i)}))) (set! ord_i (+ ord_i 1)))) (throw (ex-info "return" {:v 0}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn neg_pos [neg_pos_iterlist_p]
  (binding [neg_pos_i nil neg_pos_iterlist nil] (try (do (set! neg_pos_iterlist neg_pos_iterlist_p) (set! neg_pos_i 1) (while (< neg_pos_i (count neg_pos_iterlist)) (do (set! neg_pos_iterlist (assoc neg_pos_iterlist neg_pos_i (- (nth neg_pos_iterlist neg_pos_i)))) (set! neg_pos_i (+ neg_pos_i 2)))) (throw (ex-info "return" {:v neg_pos_iterlist}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn passcode_creator []
  (binding [passcode_creator_choices nil passcode_creator_i nil passcode_creator_idx nil passcode_creator_length nil passcode_creator_password nil passcode_creator_seed nil] (try (do (set! passcode_creator_choices "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789") (set! passcode_creator_seed (swap! nowSeed (fn [s] (mod (+ (* s 1664525) 1013904223) 2147483647)))) (set! passcode_creator_length (+ 10 (mod passcode_creator_seed 11))) (set! passcode_creator_password []) (set! passcode_creator_i 0) (while (< passcode_creator_i passcode_creator_length) (do (set! passcode_creator_seed (mod (+ (* passcode_creator_seed 1103515245) 12345) 2147483647)) (set! passcode_creator_idx (mod passcode_creator_seed (count passcode_creator_choices))) (set! passcode_creator_password (conj passcode_creator_password (subs passcode_creator_choices passcode_creator_idx (min (+ passcode_creator_idx 1) (count passcode_creator_choices))))) (set! passcode_creator_i (+ passcode_creator_i 1)))) (throw (ex-info "return" {:v passcode_creator_password}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn unique_sorted [unique_sorted_chars]
  (binding [unique_sorted_ch nil unique_sorted_i nil unique_sorted_j nil unique_sorted_k nil unique_sorted_min_idx nil unique_sorted_tmp nil unique_sorted_uniq nil] (try (do (set! unique_sorted_uniq []) (set! unique_sorted_i 0) (while (< unique_sorted_i (count unique_sorted_chars)) (do (set! unique_sorted_ch (nth unique_sorted_chars unique_sorted_i)) (when (not (in unique_sorted_ch unique_sorted_uniq)) (set! unique_sorted_uniq (conj unique_sorted_uniq unique_sorted_ch))) (set! unique_sorted_i (+ unique_sorted_i 1)))) (set! unique_sorted_j 0) (while (< unique_sorted_j (count unique_sorted_uniq)) (do (set! unique_sorted_k (+ unique_sorted_j 1)) (set! unique_sorted_min_idx unique_sorted_j) (while (< unique_sorted_k (count unique_sorted_uniq)) (do (when (< (nth unique_sorted_uniq unique_sorted_k) (nth unique_sorted_uniq unique_sorted_min_idx)) (set! unique_sorted_min_idx unique_sorted_k)) (set! unique_sorted_k (+ unique_sorted_k 1)))) (when (not= unique_sorted_min_idx unique_sorted_j) (do (set! unique_sorted_tmp (nth unique_sorted_uniq unique_sorted_j)) (set! unique_sorted_uniq (assoc unique_sorted_uniq unique_sorted_j (nth unique_sorted_uniq unique_sorted_min_idx))) (set! unique_sorted_uniq (assoc unique_sorted_uniq unique_sorted_min_idx unique_sorted_tmp)))) (set! unique_sorted_j (+ unique_sorted_j 1)))) (throw (ex-info "return" {:v unique_sorted_uniq}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn make_key_list [make_key_list_passcode]
  (binding [make_key_list_breakpoints nil make_key_list_ch nil make_key_list_i nil make_key_list_k nil make_key_list_key_list_options nil make_key_list_keys_l nil make_key_list_temp_list nil] (try (do (set! make_key_list_key_list_options "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~ \t\n") (set! make_key_list_breakpoints (unique_sorted make_key_list_passcode)) (set! make_key_list_keys_l []) (set! make_key_list_temp_list []) (set! make_key_list_i 0) (while (< make_key_list_i (count make_key_list_key_list_options)) (do (set! make_key_list_ch (subs make_key_list_key_list_options make_key_list_i (min (+ make_key_list_i 1) (count make_key_list_key_list_options)))) (set! make_key_list_temp_list (conj make_key_list_temp_list make_key_list_ch)) (when (or (in make_key_list_ch make_key_list_breakpoints) (= make_key_list_i (- (count make_key_list_key_list_options) 1))) (do (set! make_key_list_k (- (count make_key_list_temp_list) 1)) (while (>= make_key_list_k 0) (do (set! make_key_list_keys_l (conj make_key_list_keys_l (nth make_key_list_temp_list make_key_list_k))) (set! make_key_list_k (- make_key_list_k 1)))) (set! make_key_list_temp_list []))) (set! make_key_list_i (+ make_key_list_i 1)))) (throw (ex-info "return" {:v make_key_list_keys_l}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn make_shift_key [make_shift_key_passcode]
  (binding [make_shift_key_codes nil make_shift_key_i nil make_shift_key_total nil] (try (do (set! make_shift_key_codes []) (set! make_shift_key_i 0) (while (< make_shift_key_i (count make_shift_key_passcode)) (do (set! make_shift_key_codes (conj make_shift_key_codes (ord (nth make_shift_key_passcode make_shift_key_i)))) (set! make_shift_key_i (+ make_shift_key_i 1)))) (set! make_shift_key_codes (neg_pos make_shift_key_codes)) (set! make_shift_key_total 0) (set! make_shift_key_i 0) (while (< make_shift_key_i (count make_shift_key_codes)) (do (set! make_shift_key_total (+ make_shift_key_total (nth make_shift_key_codes make_shift_key_i))) (set! make_shift_key_i (+ make_shift_key_i 1)))) (if (> make_shift_key_total 0) make_shift_key_total (count make_shift_key_passcode))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn new_cipher [new_cipher_passcode_str]
  (binding [new_cipher_i nil new_cipher_key_list nil new_cipher_passcode nil new_cipher_shift_key nil] (try (do (set! new_cipher_passcode []) (if (= (count new_cipher_passcode_str) 0) (set! new_cipher_passcode (passcode_creator)) (do (set! new_cipher_i 0) (while (< new_cipher_i (count new_cipher_passcode_str)) (do (set! new_cipher_passcode (conj new_cipher_passcode (subs new_cipher_passcode_str new_cipher_i (min (+ new_cipher_i 1) (count new_cipher_passcode_str))))) (set! new_cipher_i (+ new_cipher_i 1)))))) (set! new_cipher_key_list (make_key_list new_cipher_passcode)) (set! new_cipher_shift_key (make_shift_key new_cipher_passcode)) (throw (ex-info "return" {:v {:passcode new_cipher_passcode :key_list new_cipher_key_list :shift_key new_cipher_shift_key}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn index_of [index_of_lst index_of_ch]
  (binding [index_of_i nil] (try (do (set! index_of_i 0) (while (< index_of_i (count index_of_lst)) (do (when (= (nth index_of_lst index_of_i) index_of_ch) (throw (ex-info "return" {:v index_of_i}))) (set! index_of_i (+ index_of_i 1)))) (throw (ex-info "return" {:v (- 1)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn encrypt [encrypt_c encrypt_plaintext]
  (binding [encrypt_ch nil encrypt_encoded nil encrypt_i nil encrypt_n nil encrypt_new_pos nil encrypt_position nil] (try (do (set! encrypt_encoded "") (set! encrypt_i 0) (set! encrypt_n (count (:key_list encrypt_c))) (while (< encrypt_i (count encrypt_plaintext)) (do (set! encrypt_ch (subs encrypt_plaintext encrypt_i (min (+ encrypt_i 1) (count encrypt_plaintext)))) (set! encrypt_position (index_of (:key_list encrypt_c) encrypt_ch)) (set! encrypt_new_pos (mod (+ encrypt_position (:shift_key encrypt_c)) encrypt_n)) (set! encrypt_encoded (str encrypt_encoded (get (:key_list encrypt_c) encrypt_new_pos))) (set! encrypt_i (+ encrypt_i 1)))) (throw (ex-info "return" {:v encrypt_encoded}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn decrypt [decrypt_c decrypt_encoded_message]
  (binding [decrypt_ch nil decrypt_decoded nil decrypt_i nil decrypt_n nil decrypt_new_pos nil decrypt_position nil] (try (do (set! decrypt_decoded "") (set! decrypt_i 0) (set! decrypt_n (count (:key_list decrypt_c))) (while (< decrypt_i (count decrypt_encoded_message)) (do (set! decrypt_ch (subs decrypt_encoded_message decrypt_i (min (+ decrypt_i 1) (count decrypt_encoded_message)))) (set! decrypt_position (index_of (:key_list decrypt_c) decrypt_ch)) (set! decrypt_new_pos (mod (- decrypt_position (:shift_key decrypt_c)) decrypt_n)) (when (< decrypt_new_pos 0) (set! decrypt_new_pos (+ decrypt_new_pos decrypt_n))) (set! decrypt_decoded (str decrypt_decoded (get (:key_list decrypt_c) decrypt_new_pos))) (set! decrypt_i (+ decrypt_i 1)))) (throw (ex-info "return" {:v decrypt_decoded}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn test_end_to_end []
  (binding [test_end_to_end_cip nil test_end_to_end_msg nil] (try (do (set! test_end_to_end_msg "Hello, this is a modified Caesar cipher") (set! test_end_to_end_cip (new_cipher "")) (throw (ex-info "return" {:v (decrypt test_end_to_end_cip (encrypt test_end_to_end_cip test_end_to_end_msg))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_ssc (new_cipher "4PYIXyqeQZr44"))

(def ^:dynamic main_encoded (encrypt main_ssc "Hello, this is a modified Caesar cipher"))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println main_encoded)
      (println (decrypt main_ssc main_encoded))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
