(ns main (:refer-clojure :exclude [rand generate_valid_block_size generate_permutation_key encrypt repeat_string decrypt]))

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

(declare rand generate_valid_block_size generate_permutation_key encrypt repeat_string decrypt)

(def ^:dynamic decrypt_block nil)

(def ^:dynamic decrypt_decrypted nil)

(def ^:dynamic decrypt_i nil)

(def ^:dynamic decrypt_j nil)

(def ^:dynamic decrypt_klen nil)

(def ^:dynamic decrypt_original nil)

(def ^:dynamic encrypt_block nil)

(def ^:dynamic encrypt_encrypted nil)

(def ^:dynamic encrypt_i nil)

(def ^:dynamic encrypt_j nil)

(def ^:dynamic generate_permutation_key_digits nil)

(def ^:dynamic generate_permutation_key_i nil)

(def ^:dynamic generate_permutation_key_j nil)

(def ^:dynamic generate_permutation_key_k nil)

(def ^:dynamic generate_permutation_key_temp nil)

(def ^:dynamic generate_valid_block_size_factors nil)

(def ^:dynamic generate_valid_block_size_i nil)

(def ^:dynamic generate_valid_block_size_idx nil)

(def ^:dynamic repeat_string_i nil)

(def ^:dynamic repeat_string_res nil)

(def ^:dynamic main_seed 1)

(defn rand [rand_max]
  (try (do (alter-var-root (var main_seed) (fn [_] (mod (+ (* main_seed 1103515245) 12345) 2147483647))) (throw (ex-info "return" {:v (mod main_seed rand_max)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn generate_valid_block_size [generate_valid_block_size_message_length]
  (binding [generate_valid_block_size_factors nil generate_valid_block_size_i nil generate_valid_block_size_idx nil] (try (do (set! generate_valid_block_size_factors []) (set! generate_valid_block_size_i 2) (while (<= generate_valid_block_size_i generate_valid_block_size_message_length) (do (when (= (mod generate_valid_block_size_message_length generate_valid_block_size_i) 0) (set! generate_valid_block_size_factors (conj generate_valid_block_size_factors generate_valid_block_size_i))) (set! generate_valid_block_size_i (+ generate_valid_block_size_i 1)))) (set! generate_valid_block_size_idx (rand (count generate_valid_block_size_factors))) (throw (ex-info "return" {:v (nth generate_valid_block_size_factors generate_valid_block_size_idx)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn generate_permutation_key [generate_permutation_key_block_size]
  (binding [generate_permutation_key_digits nil generate_permutation_key_i nil generate_permutation_key_j nil generate_permutation_key_k nil generate_permutation_key_temp nil] (try (do (set! generate_permutation_key_digits []) (set! generate_permutation_key_i 0) (while (< generate_permutation_key_i generate_permutation_key_block_size) (do (set! generate_permutation_key_digits (conj generate_permutation_key_digits generate_permutation_key_i)) (set! generate_permutation_key_i (+ generate_permutation_key_i 1)))) (set! generate_permutation_key_j (- generate_permutation_key_block_size 1)) (while (> generate_permutation_key_j 0) (do (set! generate_permutation_key_k (rand (+ generate_permutation_key_j 1))) (set! generate_permutation_key_temp (nth generate_permutation_key_digits generate_permutation_key_j)) (set! generate_permutation_key_digits (assoc generate_permutation_key_digits generate_permutation_key_j (nth generate_permutation_key_digits generate_permutation_key_k))) (set! generate_permutation_key_digits (assoc generate_permutation_key_digits generate_permutation_key_k generate_permutation_key_temp)) (set! generate_permutation_key_j (- generate_permutation_key_j 1)))) (throw (ex-info "return" {:v generate_permutation_key_digits}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn encrypt [encrypt_message encrypt_key encrypt_block_size]
  (binding [encrypt_block nil encrypt_encrypted nil encrypt_i nil encrypt_j nil] (try (do (set! encrypt_encrypted "") (set! encrypt_i 0) (while (< encrypt_i (count encrypt_message)) (do (set! encrypt_block (subs encrypt_message encrypt_i (min (+ encrypt_i encrypt_block_size) (count encrypt_message)))) (set! encrypt_j 0) (while (< encrypt_j encrypt_block_size) (do (set! encrypt_encrypted (str encrypt_encrypted (subs encrypt_block (nth encrypt_key encrypt_j) (min (+ (nth encrypt_key encrypt_j) 1) (count encrypt_block))))) (set! encrypt_j (+ encrypt_j 1)))) (set! encrypt_i (+ encrypt_i encrypt_block_size)))) (throw (ex-info "return" {:v encrypt_encrypted}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn repeat_string [repeat_string_times]
  (binding [repeat_string_i nil repeat_string_res nil] (try (do (set! repeat_string_res []) (set! repeat_string_i 0) (while (< repeat_string_i repeat_string_times) (do (set! repeat_string_res (conj repeat_string_res "")) (set! repeat_string_i (+ repeat_string_i 1)))) (throw (ex-info "return" {:v repeat_string_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn decrypt [decrypt_encrypted decrypt_key]
  (binding [decrypt_block nil decrypt_decrypted nil decrypt_i nil decrypt_j nil decrypt_klen nil decrypt_original nil] (try (do (set! decrypt_klen (count decrypt_key)) (set! decrypt_decrypted "") (set! decrypt_i 0) (while (< decrypt_i (count decrypt_encrypted)) (do (set! decrypt_block (subs decrypt_encrypted decrypt_i (min (+ decrypt_i decrypt_klen) (count decrypt_encrypted)))) (set! decrypt_original (repeat_string decrypt_klen)) (set! decrypt_j 0) (while (< decrypt_j decrypt_klen) (do (set! decrypt_original (assoc decrypt_original (nth decrypt_key decrypt_j) (subs decrypt_block decrypt_j (min (+ decrypt_j 1) (count decrypt_block))))) (set! decrypt_j (+ decrypt_j 1)))) (set! decrypt_j 0) (while (< decrypt_j decrypt_klen) (do (set! decrypt_decrypted (str decrypt_decrypted (nth decrypt_original decrypt_j))) (set! decrypt_j (+ decrypt_j 1)))) (set! decrypt_i (+ decrypt_i decrypt_klen)))) (throw (ex-info "return" {:v decrypt_decrypted}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_message "HELLO WORLD")

(def ^:dynamic main_block_size (generate_valid_block_size (count main_message)))

(def ^:dynamic main_key (generate_permutation_key main_block_size))

(def ^:dynamic main_encrypted (encrypt main_message main_key main_block_size))

(def ^:dynamic main_decrypted (decrypt main_encrypted main_key))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str "Block size: " (str main_block_size)))
      (println (str "Key: " (str main_key)))
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
