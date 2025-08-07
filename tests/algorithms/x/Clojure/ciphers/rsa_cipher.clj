(ns main (:refer-clojure :exclude [pow_int mod_pow ord chr get_blocks_from_text get_text_from_blocks encrypt_message decrypt_message main]))

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

(declare pow_int mod_pow ord chr get_blocks_from_text get_text_from_blocks encrypt_message decrypt_message main)

(def ^:dynamic decrypt_message_decrypted_blocks nil)

(def ^:dynamic decrypt_message_message nil)

(def ^:dynamic encrypt_message_blocks nil)

(def ^:dynamic encrypt_message_encrypted nil)

(def ^:dynamic get_blocks_from_text_block_int nil)

(def ^:dynamic get_blocks_from_text_block_ints nil)

(def ^:dynamic get_blocks_from_text_block_start nil)

(def ^:dynamic get_blocks_from_text_i nil)

(def ^:dynamic get_text_from_blocks_ascii_number nil)

(def ^:dynamic get_text_from_blocks_block nil)

(def ^:dynamic get_text_from_blocks_block_message nil)

(def ^:dynamic get_text_from_blocks_i nil)

(def ^:dynamic get_text_from_blocks_message nil)

(def ^:dynamic main_block_size nil)

(def ^:dynamic main_d nil)

(def ^:dynamic main_decrypted nil)

(def ^:dynamic main_e nil)

(def ^:dynamic main_encrypted nil)

(def ^:dynamic main_message nil)

(def ^:dynamic main_n nil)

(def ^:dynamic mod_pow_b nil)

(def ^:dynamic mod_pow_e nil)

(def ^:dynamic mod_pow_result nil)

(def ^:dynamic pow_int_i nil)

(def ^:dynamic pow_int_result nil)

(def ^:dynamic main_BYTE_SIZE 256)

(defn pow_int [pow_int_base pow_int_exp]
  (binding [pow_int_i nil pow_int_result nil] (try (do (set! pow_int_result 1) (set! pow_int_i 0) (while (< pow_int_i pow_int_exp) (do (set! pow_int_result (* pow_int_result pow_int_base)) (set! pow_int_i (+ pow_int_i 1)))) (throw (ex-info "return" {:v pow_int_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn mod_pow [mod_pow_base mod_pow_exponent mod_pow_modulus]
  (binding [mod_pow_b nil mod_pow_e nil mod_pow_result nil] (try (do (set! mod_pow_result 1) (set! mod_pow_b (mod mod_pow_base mod_pow_modulus)) (set! mod_pow_e mod_pow_exponent) (while (> mod_pow_e 0) (do (when (= (mod mod_pow_e 2) 1) (set! mod_pow_result (mod (* mod_pow_result mod_pow_b) mod_pow_modulus))) (set! mod_pow_e (quot mod_pow_e 2)) (set! mod_pow_b (mod (* mod_pow_b mod_pow_b) mod_pow_modulus)))) (throw (ex-info "return" {:v mod_pow_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn ord [ord_ch]
  (try (do (when (= ord_ch " ") (throw (ex-info "return" {:v 32}))) (when (= ord_ch "a") (throw (ex-info "return" {:v 97}))) (when (= ord_ch "b") (throw (ex-info "return" {:v 98}))) (when (= ord_ch "c") (throw (ex-info "return" {:v 99}))) (when (= ord_ch "d") (throw (ex-info "return" {:v 100}))) (when (= ord_ch "e") (throw (ex-info "return" {:v 101}))) (when (= ord_ch "f") (throw (ex-info "return" {:v 102}))) (when (= ord_ch "g") (throw (ex-info "return" {:v 103}))) (when (= ord_ch "h") (throw (ex-info "return" {:v 104}))) (when (= ord_ch "i") (throw (ex-info "return" {:v 105}))) (when (= ord_ch "j") (throw (ex-info "return" {:v 106}))) (when (= ord_ch "k") (throw (ex-info "return" {:v 107}))) (when (= ord_ch "l") (throw (ex-info "return" {:v 108}))) (when (= ord_ch "m") (throw (ex-info "return" {:v 109}))) (when (= ord_ch "n") (throw (ex-info "return" {:v 110}))) (when (= ord_ch "o") (throw (ex-info "return" {:v 111}))) (when (= ord_ch "p") (throw (ex-info "return" {:v 112}))) (when (= ord_ch "q") (throw (ex-info "return" {:v 113}))) (when (= ord_ch "r") (throw (ex-info "return" {:v 114}))) (when (= ord_ch "s") (throw (ex-info "return" {:v 115}))) (when (= ord_ch "t") (throw (ex-info "return" {:v 116}))) (when (= ord_ch "u") (throw (ex-info "return" {:v 117}))) (when (= ord_ch "v") (throw (ex-info "return" {:v 118}))) (when (= ord_ch "w") (throw (ex-info "return" {:v 119}))) (when (= ord_ch "x") (throw (ex-info "return" {:v 120}))) (when (= ord_ch "y") (throw (ex-info "return" {:v 121}))) (if (= ord_ch "z") 122 0)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn chr [chr_code]
  (try (do (when (= chr_code 32) (throw (ex-info "return" {:v " "}))) (when (= chr_code 97) (throw (ex-info "return" {:v "a"}))) (when (= chr_code 98) (throw (ex-info "return" {:v "b"}))) (when (= chr_code 99) (throw (ex-info "return" {:v "c"}))) (when (= chr_code 100) (throw (ex-info "return" {:v "d"}))) (when (= chr_code 101) (throw (ex-info "return" {:v "e"}))) (when (= chr_code 102) (throw (ex-info "return" {:v "f"}))) (when (= chr_code 103) (throw (ex-info "return" {:v "g"}))) (when (= chr_code 104) (throw (ex-info "return" {:v "h"}))) (when (= chr_code 105) (throw (ex-info "return" {:v "i"}))) (when (= chr_code 106) (throw (ex-info "return" {:v "j"}))) (when (= chr_code 107) (throw (ex-info "return" {:v "k"}))) (when (= chr_code 108) (throw (ex-info "return" {:v "l"}))) (when (= chr_code 109) (throw (ex-info "return" {:v "m"}))) (when (= chr_code 110) (throw (ex-info "return" {:v "n"}))) (when (= chr_code 111) (throw (ex-info "return" {:v "o"}))) (when (= chr_code 112) (throw (ex-info "return" {:v "p"}))) (when (= chr_code 113) (throw (ex-info "return" {:v "q"}))) (when (= chr_code 114) (throw (ex-info "return" {:v "r"}))) (when (= chr_code 115) (throw (ex-info "return" {:v "s"}))) (when (= chr_code 116) (throw (ex-info "return" {:v "t"}))) (when (= chr_code 117) (throw (ex-info "return" {:v "u"}))) (when (= chr_code 118) (throw (ex-info "return" {:v "v"}))) (when (= chr_code 119) (throw (ex-info "return" {:v "w"}))) (when (= chr_code 120) (throw (ex-info "return" {:v "x"}))) (when (= chr_code 121) (throw (ex-info "return" {:v "y"}))) (if (= chr_code 122) "z" "")) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn get_blocks_from_text [get_blocks_from_text_message get_blocks_from_text_block_size]
  (binding [get_blocks_from_text_block_int nil get_blocks_from_text_block_ints nil get_blocks_from_text_block_start nil get_blocks_from_text_i nil] (try (do (set! get_blocks_from_text_block_ints []) (set! get_blocks_from_text_block_start 0) (while (< get_blocks_from_text_block_start (count get_blocks_from_text_message)) (do (set! get_blocks_from_text_block_int 0) (set! get_blocks_from_text_i get_blocks_from_text_block_start) (while (and (< get_blocks_from_text_i (+ get_blocks_from_text_block_start get_blocks_from_text_block_size)) (< get_blocks_from_text_i (count get_blocks_from_text_message))) (do (set! get_blocks_from_text_block_int (+ get_blocks_from_text_block_int (* (ord (nth get_blocks_from_text_message get_blocks_from_text_i)) (pow_int main_BYTE_SIZE (- get_blocks_from_text_i get_blocks_from_text_block_start))))) (set! get_blocks_from_text_i (+ get_blocks_from_text_i 1)))) (set! get_blocks_from_text_block_ints (conj get_blocks_from_text_block_ints get_blocks_from_text_block_int)) (set! get_blocks_from_text_block_start (+ get_blocks_from_text_block_start get_blocks_from_text_block_size)))) (throw (ex-info "return" {:v get_blocks_from_text_block_ints}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn get_text_from_blocks [get_text_from_blocks_block_ints get_text_from_blocks_message_length get_text_from_blocks_block_size]
  (binding [get_text_from_blocks_ascii_number nil get_text_from_blocks_block nil get_text_from_blocks_block_message nil get_text_from_blocks_i nil get_text_from_blocks_message nil] (try (do (set! get_text_from_blocks_message "") (doseq [block_int get_text_from_blocks_block_ints] (do (set! get_text_from_blocks_block block_int) (set! get_text_from_blocks_i (- get_text_from_blocks_block_size 1)) (set! get_text_from_blocks_block_message "") (while (>= get_text_from_blocks_i 0) (do (when (< (+ (count get_text_from_blocks_message) get_text_from_blocks_i) get_text_from_blocks_message_length) (do (set! get_text_from_blocks_ascii_number (/ get_text_from_blocks_block (pow_int main_BYTE_SIZE get_text_from_blocks_i))) (set! get_text_from_blocks_block (mod get_text_from_blocks_block (pow_int main_BYTE_SIZE get_text_from_blocks_i))) (set! get_text_from_blocks_block_message (str (chr get_text_from_blocks_ascii_number) get_text_from_blocks_block_message)))) (set! get_text_from_blocks_i (- get_text_from_blocks_i 1)))) (set! get_text_from_blocks_message (str get_text_from_blocks_message get_text_from_blocks_block_message)))) (throw (ex-info "return" {:v get_text_from_blocks_message}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn encrypt_message [encrypt_message_message encrypt_message_n encrypt_message_e encrypt_message_block_size]
  (binding [encrypt_message_blocks nil encrypt_message_encrypted nil] (try (do (set! encrypt_message_encrypted []) (set! encrypt_message_blocks (get_blocks_from_text encrypt_message_message encrypt_message_block_size)) (doseq [block encrypt_message_blocks] (set! encrypt_message_encrypted (conj encrypt_message_encrypted (mod_pow block encrypt_message_e encrypt_message_n)))) (throw (ex-info "return" {:v encrypt_message_encrypted}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn decrypt_message [decrypt_message_blocks decrypt_message_message_length decrypt_message_n decrypt_message_d decrypt_message_block_size]
  (binding [decrypt_message_decrypted_blocks nil decrypt_message_message nil] (try (do (set! decrypt_message_decrypted_blocks []) (doseq [block decrypt_message_blocks] (set! decrypt_message_decrypted_blocks (conj decrypt_message_decrypted_blocks (mod_pow block decrypt_message_d decrypt_message_n)))) (set! decrypt_message_message "") (doseq [num decrypt_message_decrypted_blocks] (set! decrypt_message_message (str decrypt_message_message (chr num)))) (throw (ex-info "return" {:v decrypt_message_message}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_block_size nil main_d nil main_decrypted nil main_e nil main_encrypted nil main_message nil main_n nil] (do (set! main_message "hello world") (set! main_n 3233) (set! main_e 17) (set! main_d 2753) (set! main_block_size 1) (set! main_encrypted (encrypt_message main_message main_n main_e main_block_size)) (println (str main_encrypted)) (set! main_decrypted (decrypt_message main_encrypted (count main_message) main_n main_d main_block_size)) (println main_decrypted))))

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
