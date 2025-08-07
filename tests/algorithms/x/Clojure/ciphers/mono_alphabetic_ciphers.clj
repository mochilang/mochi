(ns main (:refer-clojure :exclude [find_char encrypt_message decrypt_message main]))

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

(declare find_char encrypt_message decrypt_message main)

(def ^:dynamic decrypt_message_chars_a nil)

(def ^:dynamic decrypt_message_chars_b nil)

(def ^:dynamic decrypt_message_i nil)

(def ^:dynamic decrypt_message_sub_char nil)

(def ^:dynamic decrypt_message_sym_index nil)

(def ^:dynamic decrypt_message_symbol nil)

(def ^:dynamic decrypt_message_translated nil)

(def ^:dynamic decrypt_message_upper_sym nil)

(def ^:dynamic encrypt_message_chars_a nil)

(def ^:dynamic encrypt_message_chars_b nil)

(def ^:dynamic encrypt_message_i nil)

(def ^:dynamic encrypt_message_sub_char nil)

(def ^:dynamic encrypt_message_sym_index nil)

(def ^:dynamic encrypt_message_symbol nil)

(def ^:dynamic encrypt_message_translated nil)

(def ^:dynamic encrypt_message_upper_sym nil)

(def ^:dynamic find_char_i nil)

(def ^:dynamic main_key nil)

(def ^:dynamic main_message nil)

(def ^:dynamic main_mode nil)

(def ^:dynamic main_translated nil)

(def ^:dynamic main_LETTERS "ABCDEFGHIJKLMNOPQRSTUVWXYZ")

(defn find_char [find_char_s find_char_ch]
  (binding [find_char_i nil] (try (do (set! find_char_i 0) (while (< find_char_i (count find_char_s)) (do (when (= (nth find_char_s find_char_i) find_char_ch) (throw (ex-info "return" {:v find_char_i}))) (set! find_char_i (+ find_char_i 1)))) (throw (ex-info "return" {:v (- 1)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn encrypt_message [encrypt_message_key encrypt_message_message]
  (binding [encrypt_message_chars_a nil encrypt_message_chars_b nil encrypt_message_i nil encrypt_message_sub_char nil encrypt_message_sym_index nil encrypt_message_symbol nil encrypt_message_translated nil encrypt_message_upper_sym nil] (try (do (set! encrypt_message_chars_a encrypt_message_key) (set! encrypt_message_chars_b main_LETTERS) (set! encrypt_message_translated "") (set! encrypt_message_i 0) (while (< encrypt_message_i (count encrypt_message_message)) (do (set! encrypt_message_symbol (nth encrypt_message_message encrypt_message_i)) (set! encrypt_message_upper_sym (clojure.string/upper-case encrypt_message_symbol)) (set! encrypt_message_sym_index (find_char encrypt_message_chars_a encrypt_message_upper_sym)) (if (>= encrypt_message_sym_index 0) (do (set! encrypt_message_sub_char (nth encrypt_message_chars_b encrypt_message_sym_index)) (if (= encrypt_message_symbol encrypt_message_upper_sym) (set! encrypt_message_translated (str encrypt_message_translated (clojure.string/upper-case encrypt_message_sub_char))) (set! encrypt_message_translated (str encrypt_message_translated (clojure.string/lower-case encrypt_message_sub_char))))) (set! encrypt_message_translated (str encrypt_message_translated encrypt_message_symbol))) (set! encrypt_message_i (+ encrypt_message_i 1)))) (throw (ex-info "return" {:v encrypt_message_translated}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn decrypt_message [decrypt_message_key decrypt_message_message]
  (binding [decrypt_message_chars_a nil decrypt_message_chars_b nil decrypt_message_i nil decrypt_message_sub_char nil decrypt_message_sym_index nil decrypt_message_symbol nil decrypt_message_translated nil decrypt_message_upper_sym nil] (try (do (set! decrypt_message_chars_a main_LETTERS) (set! decrypt_message_chars_b decrypt_message_key) (set! decrypt_message_translated "") (set! decrypt_message_i 0) (while (< decrypt_message_i (count decrypt_message_message)) (do (set! decrypt_message_symbol (nth decrypt_message_message decrypt_message_i)) (set! decrypt_message_upper_sym (clojure.string/upper-case decrypt_message_symbol)) (set! decrypt_message_sym_index (find_char decrypt_message_chars_a decrypt_message_upper_sym)) (if (>= decrypt_message_sym_index 0) (do (set! decrypt_message_sub_char (nth decrypt_message_chars_b decrypt_message_sym_index)) (if (= decrypt_message_symbol decrypt_message_upper_sym) (set! decrypt_message_translated (str decrypt_message_translated (clojure.string/upper-case decrypt_message_sub_char))) (set! decrypt_message_translated (str decrypt_message_translated (clojure.string/lower-case decrypt_message_sub_char))))) (set! decrypt_message_translated (str decrypt_message_translated decrypt_message_symbol))) (set! decrypt_message_i (+ decrypt_message_i 1)))) (throw (ex-info "return" {:v decrypt_message_translated}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_key nil main_message nil main_mode nil main_translated nil] (do (set! main_message "Hello World") (set! main_key "QWERTYUIOPASDFGHJKLZXCVBNM") (set! main_mode "decrypt") (set! main_translated "") (if (= main_mode "encrypt") (set! main_translated (encrypt_message main_key main_message)) (when (= main_mode "decrypt") (set! main_translated (decrypt_message main_key main_message)))) (println (str (str (str (str (str "Using the key " main_key) ", the ") main_mode) "ed message is: ") main_translated)))))

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
