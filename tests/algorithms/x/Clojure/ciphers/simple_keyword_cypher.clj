(ns main (:refer-clojure :exclude [index_in_string contains_char is_alpha to_upper remove_duplicates create_cipher_map index_in_list encipher decipher]))

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

(declare index_in_string contains_char is_alpha to_upper remove_duplicates create_cipher_map index_in_list encipher decipher)

(def ^:dynamic create_cipher_map_alphabet nil)

(def ^:dynamic create_cipher_map_char nil)

(def ^:dynamic create_cipher_map_cipher nil)

(def ^:dynamic create_cipher_map_cleaned nil)

(def ^:dynamic create_cipher_map_i nil)

(def ^:dynamic create_cipher_map_j nil)

(def ^:dynamic create_cipher_map_offset nil)

(def ^:dynamic decipher_alphabet nil)

(def ^:dynamic decipher_ch nil)

(def ^:dynamic decipher_i nil)

(def ^:dynamic decipher_idx nil)

(def ^:dynamic decipher_msg nil)

(def ^:dynamic decipher_res nil)

(def ^:dynamic encipher_alphabet nil)

(def ^:dynamic encipher_ch nil)

(def ^:dynamic encipher_i nil)

(def ^:dynamic encipher_idx nil)

(def ^:dynamic encipher_msg nil)

(def ^:dynamic encipher_res nil)

(def ^:dynamic index_in_list_i nil)

(def ^:dynamic index_in_string_i nil)

(def ^:dynamic is_alpha_lower nil)

(def ^:dynamic is_alpha_upper nil)

(def ^:dynamic remove_duplicates_ch nil)

(def ^:dynamic remove_duplicates_i nil)

(def ^:dynamic remove_duplicates_res nil)

(def ^:dynamic to_upper_ch nil)

(def ^:dynamic to_upper_i nil)

(def ^:dynamic to_upper_idx nil)

(def ^:dynamic to_upper_lower nil)

(def ^:dynamic to_upper_res nil)

(def ^:dynamic to_upper_upper nil)

(defn index_in_string [index_in_string_s index_in_string_ch]
  (binding [index_in_string_i nil] (try (do (set! index_in_string_i 0) (while (< index_in_string_i (count index_in_string_s)) (do (when (= (nth index_in_string_s index_in_string_i) index_in_string_ch) (throw (ex-info "return" {:v index_in_string_i}))) (set! index_in_string_i (+ index_in_string_i 1)))) (throw (ex-info "return" {:v (- 1)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn contains_char [contains_char_s contains_char_ch]
  (try (throw (ex-info "return" {:v (>= (index_in_string contains_char_s contains_char_ch) 0)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn is_alpha [is_alpha_ch]
  (binding [is_alpha_lower nil is_alpha_upper nil] (try (do (set! is_alpha_lower "abcdefghijklmnopqrstuvwxyz") (set! is_alpha_upper "ABCDEFGHIJKLMNOPQRSTUVWXYZ") (throw (ex-info "return" {:v (or (contains_char is_alpha_lower is_alpha_ch) (contains_char is_alpha_upper is_alpha_ch))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn to_upper [to_upper_s]
  (binding [to_upper_ch nil to_upper_i nil to_upper_idx nil to_upper_lower nil to_upper_res nil to_upper_upper nil] (try (do (set! to_upper_lower "abcdefghijklmnopqrstuvwxyz") (set! to_upper_upper "ABCDEFGHIJKLMNOPQRSTUVWXYZ") (set! to_upper_res "") (set! to_upper_i 0) (while (< to_upper_i (count to_upper_s)) (do (set! to_upper_ch (nth to_upper_s to_upper_i)) (set! to_upper_idx (index_in_string to_upper_lower to_upper_ch)) (if (>= to_upper_idx 0) (set! to_upper_res (str to_upper_res (nth to_upper_upper to_upper_idx))) (set! to_upper_res (str to_upper_res to_upper_ch))) (set! to_upper_i (+ to_upper_i 1)))) (throw (ex-info "return" {:v to_upper_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn remove_duplicates [remove_duplicates_key]
  (binding [remove_duplicates_ch nil remove_duplicates_i nil remove_duplicates_res nil] (try (do (set! remove_duplicates_res "") (set! remove_duplicates_i 0) (while (< remove_duplicates_i (count remove_duplicates_key)) (do (set! remove_duplicates_ch (nth remove_duplicates_key remove_duplicates_i)) (when (or (= remove_duplicates_ch " ") (and (is_alpha remove_duplicates_ch) (= (contains_char remove_duplicates_res remove_duplicates_ch) false))) (set! remove_duplicates_res (str remove_duplicates_res remove_duplicates_ch))) (set! remove_duplicates_i (+ remove_duplicates_i 1)))) (throw (ex-info "return" {:v remove_duplicates_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn create_cipher_map [create_cipher_map_key]
  (binding [create_cipher_map_alphabet nil create_cipher_map_char nil create_cipher_map_cipher nil create_cipher_map_cleaned nil create_cipher_map_i nil create_cipher_map_j nil create_cipher_map_offset nil] (try (do (set! create_cipher_map_alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZ") (set! create_cipher_map_cleaned (remove_duplicates (to_upper create_cipher_map_key))) (set! create_cipher_map_cipher []) (set! create_cipher_map_i 0) (while (< create_cipher_map_i (count create_cipher_map_cleaned)) (do (set! create_cipher_map_cipher (conj create_cipher_map_cipher (nth create_cipher_map_cleaned create_cipher_map_i))) (set! create_cipher_map_i (+ create_cipher_map_i 1)))) (set! create_cipher_map_offset (count create_cipher_map_cleaned)) (set! create_cipher_map_j (count create_cipher_map_cipher)) (while (< create_cipher_map_j 26) (do (set! create_cipher_map_char (nth create_cipher_map_alphabet (- create_cipher_map_j create_cipher_map_offset))) (while (contains_char create_cipher_map_cleaned create_cipher_map_char) (do (set! create_cipher_map_offset (- create_cipher_map_offset 1)) (set! create_cipher_map_char (nth create_cipher_map_alphabet (- create_cipher_map_j create_cipher_map_offset))))) (set! create_cipher_map_cipher (conj create_cipher_map_cipher create_cipher_map_char)) (set! create_cipher_map_j (+ create_cipher_map_j 1)))) (throw (ex-info "return" {:v create_cipher_map_cipher}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn index_in_list [index_in_list_lst index_in_list_ch]
  (binding [index_in_list_i nil] (try (do (set! index_in_list_i 0) (while (< index_in_list_i (count index_in_list_lst)) (do (when (= (nth index_in_list_lst index_in_list_i) index_in_list_ch) (throw (ex-info "return" {:v index_in_list_i}))) (set! index_in_list_i (+ index_in_list_i 1)))) (throw (ex-info "return" {:v (- 1)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn encipher [encipher_message encipher_cipher]
  (binding [encipher_alphabet nil encipher_ch nil encipher_i nil encipher_idx nil encipher_msg nil encipher_res nil] (try (do (set! encipher_alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZ") (set! encipher_msg (to_upper encipher_message)) (set! encipher_res "") (set! encipher_i 0) (while (< encipher_i (count encipher_msg)) (do (set! encipher_ch (nth encipher_msg encipher_i)) (set! encipher_idx (index_in_string encipher_alphabet encipher_ch)) (if (>= encipher_idx 0) (set! encipher_res (str encipher_res (nth encipher_cipher encipher_idx))) (set! encipher_res (str encipher_res encipher_ch))) (set! encipher_i (+ encipher_i 1)))) (throw (ex-info "return" {:v encipher_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn decipher [decipher_message decipher_cipher]
  (binding [decipher_alphabet nil decipher_ch nil decipher_i nil decipher_idx nil decipher_msg nil decipher_res nil] (try (do (set! decipher_alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZ") (set! decipher_msg (to_upper decipher_message)) (set! decipher_res "") (set! decipher_i 0) (while (< decipher_i (count decipher_msg)) (do (set! decipher_ch (nth decipher_msg decipher_i)) (set! decipher_idx (index_in_list decipher_cipher decipher_ch)) (if (>= decipher_idx 0) (set! decipher_res (str decipher_res (nth decipher_alphabet decipher_idx))) (set! decipher_res (str decipher_res decipher_ch))) (set! decipher_i (+ decipher_i 1)))) (throw (ex-info "return" {:v decipher_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_cipher_map (create_cipher_map "Goodbye!!"))

(def ^:dynamic main_encoded (encipher "Hello World!!" main_cipher_map))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println main_encoded)
      (println (decipher main_encoded main_cipher_map))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
