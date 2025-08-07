(ns main (:refer-clojure :exclude [indexOf ord chr clean_text running_key_encrypt running_key_decrypt]))

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

(declare indexOf ord chr clean_text running_key_encrypt running_key_decrypt)

(def ^:dynamic chr_lower nil)

(def ^:dynamic chr_upper nil)

(def ^:dynamic clean_text_ch nil)

(def ^:dynamic clean_text_i nil)

(def ^:dynamic clean_text_out nil)

(def ^:dynamic indexOf_i nil)

(def ^:dynamic ord_idx nil)

(def ^:dynamic ord_lower nil)

(def ^:dynamic ord_upper nil)

(def ^:dynamic running_key_decrypt_c nil)

(def ^:dynamic running_key_decrypt_ct nil)

(def ^:dynamic running_key_decrypt_i nil)

(def ^:dynamic running_key_decrypt_k nil)

(def ^:dynamic running_key_decrypt_key_len nil)

(def ^:dynamic running_key_decrypt_kv nil)

(def ^:dynamic running_key_decrypt_ord_a nil)

(def ^:dynamic running_key_decrypt_p nil)

(def ^:dynamic running_key_decrypt_res nil)

(def ^:dynamic running_key_encrypt_c nil)

(def ^:dynamic running_key_encrypt_i nil)

(def ^:dynamic running_key_encrypt_k nil)

(def ^:dynamic running_key_encrypt_key_len nil)

(def ^:dynamic running_key_encrypt_kv nil)

(def ^:dynamic running_key_encrypt_ord_a nil)

(def ^:dynamic running_key_encrypt_p nil)

(def ^:dynamic running_key_encrypt_pt nil)

(def ^:dynamic running_key_encrypt_res nil)

(defn indexOf [indexOf_s indexOf_ch]
  (binding [indexOf_i nil] (try (do (set! indexOf_i 0) (while (< indexOf_i (count indexOf_s)) (do (when (= (nth indexOf_s indexOf_i) indexOf_ch) (throw (ex-info "return" {:v indexOf_i}))) (set! indexOf_i (+ indexOf_i 1)))) (throw (ex-info "return" {:v (- 1)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn ord [ord_ch]
  (binding [ord_idx nil ord_lower nil ord_upper nil] (try (do (set! ord_upper "ABCDEFGHIJKLMNOPQRSTUVWXYZ") (set! ord_lower "abcdefghijklmnopqrstuvwxyz") (set! ord_idx (indexOf ord_upper ord_ch)) (when (>= ord_idx 0) (throw (ex-info "return" {:v (+ 65 ord_idx)}))) (set! ord_idx (indexOf ord_lower ord_ch)) (if (>= ord_idx 0) (+ 97 ord_idx) 0)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn chr [chr_n]
  (binding [chr_lower nil chr_upper nil] (try (do (set! chr_upper "ABCDEFGHIJKLMNOPQRSTUVWXYZ") (set! chr_lower "abcdefghijklmnopqrstuvwxyz") (when (and (>= chr_n 65) (< chr_n 91)) (throw (ex-info "return" {:v (subs chr_upper (- chr_n 65) (min (- chr_n 64) (count chr_upper)))}))) (if (and (>= chr_n 97) (< chr_n 123)) (subs chr_lower (- chr_n 97) (min (- chr_n 96) (count chr_lower))) "?")) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn clean_text [clean_text_s]
  (binding [clean_text_ch nil clean_text_i nil clean_text_out nil] (try (do (set! clean_text_out "") (set! clean_text_i 0) (while (< clean_text_i (count clean_text_s)) (do (set! clean_text_ch (nth clean_text_s clean_text_i)) (if (and (>= (compare clean_text_ch "A") 0) (<= (compare clean_text_ch "Z") 0)) (set! clean_text_out (str clean_text_out clean_text_ch)) (when (and (>= (compare clean_text_ch "a") 0) (<= (compare clean_text_ch "z") 0)) (set! clean_text_out (str clean_text_out (chr (- (ord clean_text_ch) 32)))))) (set! clean_text_i (+ clean_text_i 1)))) (throw (ex-info "return" {:v clean_text_out}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn running_key_encrypt [running_key_encrypt_key running_key_encrypt_plaintext]
  (binding [running_key_encrypt_c nil running_key_encrypt_i nil running_key_encrypt_k nil running_key_encrypt_key_len nil running_key_encrypt_kv nil running_key_encrypt_ord_a nil running_key_encrypt_p nil running_key_encrypt_pt nil running_key_encrypt_res nil] (try (do (set! running_key_encrypt_pt (clean_text running_key_encrypt_plaintext)) (set! running_key_encrypt_k (clean_text running_key_encrypt_key)) (set! running_key_encrypt_key_len (count running_key_encrypt_k)) (set! running_key_encrypt_res "") (set! running_key_encrypt_ord_a (ord "A")) (set! running_key_encrypt_i 0) (while (< running_key_encrypt_i (count running_key_encrypt_pt)) (do (set! running_key_encrypt_p (- (ord (nth running_key_encrypt_pt running_key_encrypt_i)) running_key_encrypt_ord_a)) (set! running_key_encrypt_kv (- (ord (nth running_key_encrypt_k (mod running_key_encrypt_i running_key_encrypt_key_len))) running_key_encrypt_ord_a)) (set! running_key_encrypt_c (mod (+ running_key_encrypt_p running_key_encrypt_kv) 26)) (set! running_key_encrypt_res (str running_key_encrypt_res (chr (+ running_key_encrypt_c running_key_encrypt_ord_a)))) (set! running_key_encrypt_i (+ running_key_encrypt_i 1)))) (throw (ex-info "return" {:v running_key_encrypt_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn running_key_decrypt [running_key_decrypt_key running_key_decrypt_ciphertext]
  (binding [running_key_decrypt_c nil running_key_decrypt_ct nil running_key_decrypt_i nil running_key_decrypt_k nil running_key_decrypt_key_len nil running_key_decrypt_kv nil running_key_decrypt_ord_a nil running_key_decrypt_p nil running_key_decrypt_res nil] (try (do (set! running_key_decrypt_ct (clean_text running_key_decrypt_ciphertext)) (set! running_key_decrypt_k (clean_text running_key_decrypt_key)) (set! running_key_decrypt_key_len (count running_key_decrypt_k)) (set! running_key_decrypt_res "") (set! running_key_decrypt_ord_a (ord "A")) (set! running_key_decrypt_i 0) (while (< running_key_decrypt_i (count running_key_decrypt_ct)) (do (set! running_key_decrypt_c (- (ord (nth running_key_decrypt_ct running_key_decrypt_i)) running_key_decrypt_ord_a)) (set! running_key_decrypt_kv (- (ord (nth running_key_decrypt_k (mod running_key_decrypt_i running_key_decrypt_key_len))) running_key_decrypt_ord_a)) (set! running_key_decrypt_p (mod (+ (- running_key_decrypt_c running_key_decrypt_kv) 26) 26)) (set! running_key_decrypt_res (str running_key_decrypt_res (chr (+ running_key_decrypt_p running_key_decrypt_ord_a)))) (set! running_key_decrypt_i (+ running_key_decrypt_i 1)))) (throw (ex-info "return" {:v running_key_decrypt_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_key "How does the duck know that? said Victor")

(def ^:dynamic main_plaintext "DEFEND THIS")

(def ^:dynamic main_ciphertext (running_key_encrypt main_key main_plaintext))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println main_ciphertext)
      (println (running_key_decrypt main_key main_ciphertext))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
