(ns main (:refer-clojure :exclude [gcd mod_inverse find_symbol check_keys encrypt_message decrypt_message main]))

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

(declare gcd mod_inverse find_symbol check_keys encrypt_message decrypt_message main)

(def ^:dynamic check_keys_m nil)

(def ^:dynamic decrypt_message_ch nil)

(def ^:dynamic decrypt_message_final nil)

(def ^:dynamic decrypt_message_i nil)

(def ^:dynamic decrypt_message_index nil)

(def ^:dynamic decrypt_message_inv nil)

(def ^:dynamic decrypt_message_key_a nil)

(def ^:dynamic decrypt_message_key_b nil)

(def ^:dynamic decrypt_message_m nil)

(def ^:dynamic decrypt_message_n nil)

(def ^:dynamic decrypt_message_plain_text nil)

(def ^:dynamic decrypt_message_pos nil)

(def ^:dynamic encrypt_message_ch nil)

(def ^:dynamic encrypt_message_cipher_text nil)

(def ^:dynamic encrypt_message_i nil)

(def ^:dynamic encrypt_message_index nil)

(def ^:dynamic encrypt_message_key_a nil)

(def ^:dynamic encrypt_message_key_b nil)

(def ^:dynamic encrypt_message_m nil)

(def ^:dynamic find_symbol_i nil)

(def ^:dynamic gcd_temp nil)

(def ^:dynamic gcd_x nil)

(def ^:dynamic gcd_y nil)

(def ^:dynamic main_enc nil)

(def ^:dynamic main_key nil)

(def ^:dynamic main_msg nil)

(def ^:dynamic mod_inverse_q nil)

(def ^:dynamic mod_inverse_res nil)

(def ^:dynamic mod_inverse_t1 nil)

(def ^:dynamic mod_inverse_t2 nil)

(def ^:dynamic mod_inverse_t3 nil)

(def ^:dynamic mod_inverse_u1 nil)

(def ^:dynamic mod_inverse_u2 nil)

(def ^:dynamic mod_inverse_u3 nil)

(def ^:dynamic mod_inverse_v1 nil)

(def ^:dynamic mod_inverse_v2 nil)

(def ^:dynamic mod_inverse_v3 nil)

(def ^:dynamic main_SYMBOLS " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~")

(defn gcd [gcd_a gcd_b]
  (binding [gcd_temp nil gcd_x nil gcd_y nil] (try (do (set! gcd_x gcd_a) (set! gcd_y gcd_b) (while (not= gcd_y 0) (do (set! gcd_temp (mod gcd_x gcd_y)) (set! gcd_x gcd_y) (set! gcd_y gcd_temp))) (throw (ex-info "return" {:v gcd_x}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn mod_inverse [mod_inverse_a mod_inverse_m]
  (binding [mod_inverse_q nil mod_inverse_res nil mod_inverse_t1 nil mod_inverse_t2 nil mod_inverse_t3 nil mod_inverse_u1 nil mod_inverse_u2 nil mod_inverse_u3 nil mod_inverse_v1 nil mod_inverse_v2 nil mod_inverse_v3 nil] (try (do (when (not= (gcd mod_inverse_a mod_inverse_m) 1) (throw (Exception. (str (str (str (str "mod inverse of " (str mod_inverse_a)) " and ") (str mod_inverse_m)) " does not exist")))) (set! mod_inverse_u1 1) (set! mod_inverse_u2 0) (set! mod_inverse_u3 mod_inverse_a) (set! mod_inverse_v1 0) (set! mod_inverse_v2 1) (set! mod_inverse_v3 mod_inverse_m) (while (not= mod_inverse_v3 0) (do (set! mod_inverse_q (/ mod_inverse_u3 mod_inverse_v3)) (set! mod_inverse_t1 (- mod_inverse_u1 (* mod_inverse_q mod_inverse_v1))) (set! mod_inverse_t2 (- mod_inverse_u2 (* mod_inverse_q mod_inverse_v2))) (set! mod_inverse_t3 (- mod_inverse_u3 (* mod_inverse_q mod_inverse_v3))) (set! mod_inverse_u1 mod_inverse_v1) (set! mod_inverse_u2 mod_inverse_v2) (set! mod_inverse_u3 mod_inverse_v3) (set! mod_inverse_v1 mod_inverse_t1) (set! mod_inverse_v2 mod_inverse_t2) (set! mod_inverse_v3 mod_inverse_t3))) (set! mod_inverse_res (mod mod_inverse_u1 mod_inverse_m)) (if (< mod_inverse_res 0) (+ mod_inverse_res mod_inverse_m) mod_inverse_res)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn find_symbol [find_symbol_ch]
  (binding [find_symbol_i nil] (try (do (set! find_symbol_i 0) (while (< find_symbol_i (count main_SYMBOLS)) (do (when (= (nth main_SYMBOLS find_symbol_i) find_symbol_ch) (throw (ex-info "return" {:v find_symbol_i}))) (set! find_symbol_i (+ find_symbol_i 1)))) (throw (ex-info "return" {:v (- 1)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn check_keys [check_keys_key_a check_keys_key_b check_keys_mode]
  (binding [check_keys_m nil] (do (set! check_keys_m (count main_SYMBOLS)) (when (= check_keys_mode "encrypt") (do (when (= check_keys_key_a 1) (throw (Exception. "The affine cipher becomes weak when key A is set to 1. Choose different key"))) (when (= check_keys_key_b 0) (throw (Exception. "The affine cipher becomes weak when key B is set to 0. Choose different key"))))) (when (or (or (< check_keys_key_a 0) (< check_keys_key_b 0)) (> check_keys_key_b (- check_keys_m 1))) (throw (Exception. (str "Key A must be greater than 0 and key B must be between 0 and " (str (- check_keys_m 1)))))) (when (not= (gcd check_keys_key_a check_keys_m) 1) (throw (Exception. (str (str (str (str "Key A " (str check_keys_key_a)) " and the symbol set size ") (str check_keys_m)) " are not relatively prime. Choose a different key.")))))))

(defn encrypt_message [encrypt_message_key encrypt_message_message]
  (binding [encrypt_message_ch nil encrypt_message_cipher_text nil encrypt_message_i nil encrypt_message_index nil encrypt_message_key_a nil encrypt_message_key_b nil encrypt_message_m nil] (try (do (set! encrypt_message_m (count main_SYMBOLS)) (set! encrypt_message_key_a (/ encrypt_message_key encrypt_message_m)) (set! encrypt_message_key_b (mod encrypt_message_key encrypt_message_m)) (check_keys encrypt_message_key_a encrypt_message_key_b "encrypt") (set! encrypt_message_cipher_text "") (set! encrypt_message_i 0) (while (< encrypt_message_i (count encrypt_message_message)) (do (set! encrypt_message_ch (nth encrypt_message_message encrypt_message_i)) (set! encrypt_message_index (find_symbol encrypt_message_ch)) (if (>= encrypt_message_index 0) (set! encrypt_message_cipher_text (str encrypt_message_cipher_text (nth main_SYMBOLS (mod (+ (* encrypt_message_index encrypt_message_key_a) encrypt_message_key_b) encrypt_message_m)))) (set! encrypt_message_cipher_text (str encrypt_message_cipher_text encrypt_message_ch))) (set! encrypt_message_i (+ encrypt_message_i 1)))) (throw (ex-info "return" {:v encrypt_message_cipher_text}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn decrypt_message [decrypt_message_key decrypt_message_message]
  (binding [decrypt_message_ch nil decrypt_message_final nil decrypt_message_i nil decrypt_message_index nil decrypt_message_inv nil decrypt_message_key_a nil decrypt_message_key_b nil decrypt_message_m nil decrypt_message_n nil decrypt_message_plain_text nil decrypt_message_pos nil] (try (do (set! decrypt_message_m (count main_SYMBOLS)) (set! decrypt_message_key_a (/ decrypt_message_key decrypt_message_m)) (set! decrypt_message_key_b (mod decrypt_message_key decrypt_message_m)) (check_keys decrypt_message_key_a decrypt_message_key_b "decrypt") (set! decrypt_message_inv (mod_inverse decrypt_message_key_a decrypt_message_m)) (set! decrypt_message_plain_text "") (set! decrypt_message_i 0) (while (< decrypt_message_i (count decrypt_message_message)) (do (set! decrypt_message_ch (nth decrypt_message_message decrypt_message_i)) (set! decrypt_message_index (find_symbol decrypt_message_ch)) (if (>= decrypt_message_index 0) (do (set! decrypt_message_n (* (- decrypt_message_index decrypt_message_key_b) decrypt_message_inv)) (set! decrypt_message_pos (mod decrypt_message_n decrypt_message_m)) (set! decrypt_message_final (if (< decrypt_message_pos 0) (+ decrypt_message_pos decrypt_message_m) decrypt_message_pos)) (set! decrypt_message_plain_text (str decrypt_message_plain_text (nth main_SYMBOLS decrypt_message_final)))) (set! decrypt_message_plain_text (str decrypt_message_plain_text decrypt_message_ch))) (set! decrypt_message_i (+ decrypt_message_i 1)))) (throw (ex-info "return" {:v decrypt_message_plain_text}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_enc nil main_key nil main_msg nil] (do (set! main_key 4545) (set! main_msg "The affine cipher is a type of monoalphabetic substitution cipher.") (set! main_enc (encrypt_message main_key main_msg)) (println main_enc) (println (decrypt_message main_key main_enc)))))

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
