(ns main (:refer-clojure :exclude [to_lowercase char_index index_char encrypt decrypt]))

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

(declare to_lowercase char_index index_char encrypt decrypt)

(def ^:dynamic char_index_i nil)

(def ^:dynamic decrypt_c_char nil)

(def ^:dynamic decrypt_c_i nil)

(def ^:dynamic decrypt_c_idx nil)

(def ^:dynamic decrypt_current_key nil)

(def ^:dynamic decrypt_k_char nil)

(def ^:dynamic decrypt_k_i nil)

(def ^:dynamic decrypt_k_idx nil)

(def ^:dynamic decrypt_p_char nil)

(def ^:dynamic decrypt_p_idx nil)

(def ^:dynamic decrypt_plaintext nil)

(def ^:dynamic encrypt_c_idx nil)

(def ^:dynamic encrypt_ciphertext nil)

(def ^:dynamic encrypt_full_key nil)

(def ^:dynamic encrypt_k_char nil)

(def ^:dynamic encrypt_k_i nil)

(def ^:dynamic encrypt_k_idx nil)

(def ^:dynamic encrypt_p_char nil)

(def ^:dynamic encrypt_p_i nil)

(def ^:dynamic encrypt_p_idx nil)

(def ^:dynamic encrypt_plaintext nil)

(def ^:dynamic to_lowercase_c nil)

(def ^:dynamic to_lowercase_found nil)

(def ^:dynamic to_lowercase_i nil)

(def ^:dynamic to_lowercase_j nil)

(def ^:dynamic to_lowercase_res nil)

(def ^:dynamic main_LOWER "abcdefghijklmnopqrstuvwxyz")

(def ^:dynamic main_UPPER "ABCDEFGHIJKLMNOPQRSTUVWXYZ")

(defn to_lowercase [to_lowercase_s]
  (binding [to_lowercase_c nil to_lowercase_found nil to_lowercase_i nil to_lowercase_j nil to_lowercase_res nil] (try (do (set! to_lowercase_res "") (set! to_lowercase_i 0) (while (< to_lowercase_i (count to_lowercase_s)) (do (set! to_lowercase_c (nth to_lowercase_s to_lowercase_i)) (set! to_lowercase_j 0) (set! to_lowercase_found false) (loop [while_flag_1 true] (when (and while_flag_1 (< to_lowercase_j 26)) (cond (= to_lowercase_c (nth main_UPPER to_lowercase_j)) (do (set! to_lowercase_res (str to_lowercase_res (nth main_LOWER to_lowercase_j))) (set! to_lowercase_found true) (recur false)) :else (do (set! to_lowercase_j (+ to_lowercase_j 1)) (recur while_flag_1))))) (when (not to_lowercase_found) (set! to_lowercase_res (str to_lowercase_res to_lowercase_c))) (set! to_lowercase_i (+ to_lowercase_i 1)))) (throw (ex-info "return" {:v to_lowercase_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn char_index [char_index_c]
  (binding [char_index_i nil] (try (do (set! char_index_i 0) (while (< char_index_i 26) (do (when (= char_index_c (nth main_LOWER char_index_i)) (throw (ex-info "return" {:v char_index_i}))) (set! char_index_i (+ char_index_i 1)))) (throw (ex-info "return" {:v (- 1)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn index_char [index_char_i]
  (try (throw (ex-info "return" {:v (nth main_LOWER index_char_i)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn encrypt [encrypt_plaintext_p encrypt_key]
  (binding [encrypt_c_idx nil encrypt_ciphertext nil encrypt_full_key nil encrypt_k_char nil encrypt_k_i nil encrypt_k_idx nil encrypt_p_char nil encrypt_p_i nil encrypt_p_idx nil encrypt_plaintext nil] (try (do (set! encrypt_plaintext encrypt_plaintext_p) (when (= (count encrypt_plaintext) 0) (throw (Exception. "plaintext is empty"))) (when (= (count encrypt_key) 0) (throw (Exception. "key is empty"))) (set! encrypt_full_key (str encrypt_key encrypt_plaintext)) (set! encrypt_plaintext (to_lowercase encrypt_plaintext)) (set! encrypt_full_key (to_lowercase encrypt_full_key)) (set! encrypt_p_i 0) (set! encrypt_k_i 0) (set! encrypt_ciphertext "") (while (< encrypt_p_i (count encrypt_plaintext)) (do (set! encrypt_p_char (nth encrypt_plaintext encrypt_p_i)) (set! encrypt_p_idx (char_index encrypt_p_char)) (if (< encrypt_p_idx 0) (do (set! encrypt_ciphertext (str encrypt_ciphertext encrypt_p_char)) (set! encrypt_p_i (+ encrypt_p_i 1))) (do (set! encrypt_k_char (nth encrypt_full_key encrypt_k_i)) (set! encrypt_k_idx (char_index encrypt_k_char)) (if (< encrypt_k_idx 0) (set! encrypt_k_i (+ encrypt_k_i 1)) (do (set! encrypt_c_idx (mod (+ encrypt_p_idx encrypt_k_idx) 26)) (set! encrypt_ciphertext (str encrypt_ciphertext (index_char encrypt_c_idx))) (set! encrypt_k_i (+ encrypt_k_i 1)) (set! encrypt_p_i (+ encrypt_p_i 1)))))))) (throw (ex-info "return" {:v encrypt_ciphertext}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn decrypt [decrypt_ciphertext decrypt_key]
  (binding [decrypt_c_char nil decrypt_c_i nil decrypt_c_idx nil decrypt_current_key nil decrypt_k_char nil decrypt_k_i nil decrypt_k_idx nil decrypt_p_char nil decrypt_p_idx nil decrypt_plaintext nil] (try (do (when (= (count decrypt_ciphertext) 0) (throw (Exception. "ciphertext is empty"))) (when (= (count decrypt_key) 0) (throw (Exception. "key is empty"))) (set! decrypt_current_key (to_lowercase decrypt_key)) (set! decrypt_c_i 0) (set! decrypt_k_i 0) (set! decrypt_plaintext "") (while (< decrypt_c_i (count decrypt_ciphertext)) (do (set! decrypt_c_char (nth decrypt_ciphertext decrypt_c_i)) (set! decrypt_c_idx (char_index decrypt_c_char)) (if (< decrypt_c_idx 0) (set! decrypt_plaintext (str decrypt_plaintext decrypt_c_char)) (do (set! decrypt_k_char (nth decrypt_current_key decrypt_k_i)) (set! decrypt_k_idx (char_index decrypt_k_char)) (set! decrypt_p_idx (mod (+ (- decrypt_c_idx decrypt_k_idx) 26) 26)) (set! decrypt_p_char (index_char decrypt_p_idx)) (set! decrypt_plaintext (str decrypt_plaintext decrypt_p_char)) (set! decrypt_current_key (str decrypt_current_key decrypt_p_char)) (set! decrypt_k_i (+ decrypt_k_i 1)))) (set! decrypt_c_i (+ decrypt_c_i 1)))) (throw (ex-info "return" {:v decrypt_plaintext}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (encrypt "hello world" "coffee"))
      (println (decrypt "jsqqs avvwo" "coffee"))
      (println (encrypt "coffee is good as python" "TheAlgorithms"))
      (println (decrypt "vvjfpk wj ohvp su ddylsv" "TheAlgorithms"))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
