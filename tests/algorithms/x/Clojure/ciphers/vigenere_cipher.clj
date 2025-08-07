(ns main (:refer-clojure :exclude [find_index to_upper_char to_lower_char is_upper to_upper_string]))

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

(declare find_index to_upper_char to_lower_char is_upper to_upper_string)

(def ^:dynamic find_index_i nil)

(def ^:dynamic main_decrypted nil)

(def ^:dynamic main_encrypted nil)

(def ^:dynamic main_i nil)

(def ^:dynamic main_key_index nil)

(def ^:dynamic main_num nil)

(def ^:dynamic to_lower_char_idx nil)

(def ^:dynamic to_upper_char_idx nil)

(def ^:dynamic to_upper_string_i nil)

(def ^:dynamic to_upper_string_res nil)

(def ^:dynamic main_LETTERS "ABCDEFGHIJKLMNOPQRSTUVWXYZ")

(def ^:dynamic main_LETTERS_LOWER "abcdefghijklmnopqrstuvwxyz")

(defn find_index [find_index_s find_index_ch]
  (binding [find_index_i nil] (try (do (set! find_index_i 0) (while (< find_index_i (count find_index_s)) (do (when (= (nth find_index_s find_index_i) find_index_ch) (throw (ex-info "return" {:v find_index_i}))) (set! find_index_i (+ find_index_i 1)))) (throw (ex-info "return" {:v (- 1)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn to_upper_char [to_upper_char_ch]
  (binding [to_upper_char_idx nil] (try (do (set! to_upper_char_idx (find_index main_LETTERS_LOWER to_upper_char_ch)) (if (>= to_upper_char_idx 0) (nth main_LETTERS to_upper_char_idx) to_upper_char_ch)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn to_lower_char [to_lower_char_ch]
  (binding [to_lower_char_idx nil] (try (do (set! to_lower_char_idx (find_index main_LETTERS to_lower_char_ch)) (if (>= to_lower_char_idx 0) (nth main_LETTERS_LOWER to_lower_char_idx) to_lower_char_ch)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn is_upper [is_upper_ch]
  (try (throw (ex-info "return" {:v (>= (find_index main_LETTERS is_upper_ch) 0)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn to_upper_string [to_upper_string_s]
  (binding [to_upper_string_i nil to_upper_string_res nil] (try (do (set! to_upper_string_res "") (set! to_upper_string_i 0) (while (< to_upper_string_i (count to_upper_string_s)) (do (set! to_upper_string_res (str to_upper_string_res (to_upper_char (nth to_upper_string_s to_upper_string_i)))) (set! to_upper_string_i (+ to_upper_string_i 1)))) (throw (ex-info "return" {:v to_upper_string_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_key "HDarji")

(def ^:dynamic main_message "This is Harshil Darji from Dharmaj.")

(def ^:dynamic main_key_up (to_upper_string main_key))

(def ^:dynamic main_encrypted "")

(def ^:dynamic main_key_index 0)

(def ^:dynamic main_i 0)

(def ^:dynamic main_decrypted "")

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (while (< main_i (count main_message)) (do (def ^:dynamic main_symbol (nth main_message main_i)) (def ^:dynamic main_upper_symbol (to_upper_char main_symbol)) (def ^:dynamic main_num (find_index main_LETTERS main_upper_symbol)) (if (>= main_num 0) (do (def main_num (+ main_num (find_index main_LETTERS (nth main_key_up main_key_index)))) (def main_num (mod main_num (count main_LETTERS))) (if (is_upper main_symbol) (def main_encrypted (+ main_encrypted (nth main_LETTERS main_num))) (def main_encrypted (str main_encrypted (to_lower_char (nth main_LETTERS main_num))))) (def main_key_index (+ main_key_index 1)) (when (= main_key_index (count main_key_up)) (def main_key_index 0))) (def main_encrypted (+ main_encrypted main_symbol))) (def main_i (+ main_i 1))))
      (println main_encrypted)
      (def main_key_index 0)
      (def main_i 0)
      (while (< main_i (count main_encrypted)) (do (def ^:dynamic main_symbol (nth main_encrypted main_i)) (def ^:dynamic main_upper_symbol (to_upper_char main_symbol)) (def ^:dynamic main_num (find_index main_LETTERS main_upper_symbol)) (if (>= main_num 0) (do (def main_num (- main_num (find_index main_LETTERS (nth main_key_up main_key_index)))) (def main_num (mod main_num (count main_LETTERS))) (if (is_upper main_symbol) (def main_decrypted (+ main_decrypted (nth main_LETTERS main_num))) (def main_decrypted (str main_decrypted (to_lower_char (nth main_LETTERS main_num))))) (def main_key_index (+ main_key_index 1)) (when (= main_key_index (count main_key_up)) (def main_key_index 0))) (def main_decrypted (+ main_decrypted main_symbol))) (def main_i (+ main_i 1))))
      (println main_decrypted)
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
