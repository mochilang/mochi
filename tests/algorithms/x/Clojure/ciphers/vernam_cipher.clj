(ns main (:refer-clojure :exclude [indexOf ord chr vernam_encrypt vernam_decrypt]))

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

(declare indexOf ord chr vernam_encrypt vernam_decrypt)

(def ^:dynamic chr_upper nil)

(def ^:dynamic indexOf_i nil)

(def ^:dynamic ord_idx nil)

(def ^:dynamic ord_upper nil)

(def ^:dynamic vernam_decrypt_c nil)

(def ^:dynamic vernam_decrypt_decrypted nil)

(def ^:dynamic vernam_decrypt_i nil)

(def ^:dynamic vernam_decrypt_k nil)

(def ^:dynamic vernam_decrypt_val nil)

(def ^:dynamic vernam_encrypt_ciphertext nil)

(def ^:dynamic vernam_encrypt_ct nil)

(def ^:dynamic vernam_encrypt_i nil)

(def ^:dynamic vernam_encrypt_k nil)

(def ^:dynamic vernam_encrypt_p nil)

(defn indexOf [indexOf_s indexOf_ch]
  (binding [indexOf_i nil] (try (do (set! indexOf_i 0) (while (< indexOf_i (count indexOf_s)) (do (when (= (subs indexOf_s indexOf_i (min (+ indexOf_i 1) (count indexOf_s))) indexOf_ch) (throw (ex-info "return" {:v indexOf_i}))) (set! indexOf_i (+ indexOf_i 1)))) (throw (ex-info "return" {:v (- 1)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn ord [ord_ch]
  (binding [ord_idx nil ord_upper nil] (try (do (set! ord_upper "ABCDEFGHIJKLMNOPQRSTUVWXYZ") (set! ord_idx (indexOf ord_upper ord_ch)) (if (>= ord_idx 0) (+ 65 ord_idx) 0)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn chr [chr_n]
  (binding [chr_upper nil] (try (do (set! chr_upper "ABCDEFGHIJKLMNOPQRSTUVWXYZ") (if (and (>= chr_n 65) (< chr_n 91)) (subs chr_upper (- chr_n 65) (min (- chr_n 64) (count chr_upper))) "?")) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn vernam_encrypt [vernam_encrypt_plaintext vernam_encrypt_key]
  (binding [vernam_encrypt_ciphertext nil vernam_encrypt_ct nil vernam_encrypt_i nil vernam_encrypt_k nil vernam_encrypt_p nil] (try (do (set! vernam_encrypt_ciphertext "") (set! vernam_encrypt_i 0) (while (< vernam_encrypt_i (count vernam_encrypt_plaintext)) (do (set! vernam_encrypt_p (- (ord (subs vernam_encrypt_plaintext vernam_encrypt_i (min (+ vernam_encrypt_i 1) (count vernam_encrypt_plaintext)))) 65)) (set! vernam_encrypt_k (- (ord (subs vernam_encrypt_key (mod vernam_encrypt_i (count vernam_encrypt_key)) (min (+ (mod vernam_encrypt_i (count vernam_encrypt_key)) 1) (count vernam_encrypt_key)))) 65)) (set! vernam_encrypt_ct (+ vernam_encrypt_p vernam_encrypt_k)) (while (> vernam_encrypt_ct 25) (set! vernam_encrypt_ct (- vernam_encrypt_ct 26))) (set! vernam_encrypt_ciphertext (str vernam_encrypt_ciphertext (chr (+ vernam_encrypt_ct 65)))) (set! vernam_encrypt_i (+ vernam_encrypt_i 1)))) (throw (ex-info "return" {:v vernam_encrypt_ciphertext}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn vernam_decrypt [vernam_decrypt_ciphertext vernam_decrypt_key]
  (binding [vernam_decrypt_c nil vernam_decrypt_decrypted nil vernam_decrypt_i nil vernam_decrypt_k nil vernam_decrypt_val nil] (try (do (set! vernam_decrypt_decrypted "") (set! vernam_decrypt_i 0) (while (< vernam_decrypt_i (count vernam_decrypt_ciphertext)) (do (set! vernam_decrypt_c (ord (subs vernam_decrypt_ciphertext vernam_decrypt_i (min (+ vernam_decrypt_i 1) (count vernam_decrypt_ciphertext))))) (set! vernam_decrypt_k (ord (subs vernam_decrypt_key (mod vernam_decrypt_i (count vernam_decrypt_key)) (min (+ (mod vernam_decrypt_i (count vernam_decrypt_key)) 1) (count vernam_decrypt_key))))) (set! vernam_decrypt_val (- vernam_decrypt_c vernam_decrypt_k)) (while (< vernam_decrypt_val 0) (set! vernam_decrypt_val (+ vernam_decrypt_val 26))) (set! vernam_decrypt_decrypted (str vernam_decrypt_decrypted (chr (+ vernam_decrypt_val 65)))) (set! vernam_decrypt_i (+ vernam_decrypt_i 1)))) (throw (ex-info "return" {:v vernam_decrypt_decrypted}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_plaintext "HELLO")

(def ^:dynamic main_key "KEY")

(def ^:dynamic main_encrypted (vernam_encrypt main_plaintext main_key))

(def ^:dynamic main_decrypted (vernam_decrypt main_encrypted main_key))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str "Plaintext: " main_plaintext))
      (println (str "Encrypted: " main_encrypted))
      (println (str "Decrypted: " main_decrypted))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
