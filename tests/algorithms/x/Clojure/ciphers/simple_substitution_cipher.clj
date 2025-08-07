(ns main (:refer-clojure :exclude [rand get_random_key check_valid_key index_in char_to_upper char_to_lower is_upper translate_message encrypt_message decrypt_message]))

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

(declare rand get_random_key check_valid_key index_in char_to_upper char_to_lower is_upper translate_message encrypt_message decrypt_message)

(def ^:dynamic char_to_lower_i nil)

(def ^:dynamic char_to_upper_i nil)

(def ^:dynamic check_valid_key_ch nil)

(def ^:dynamic check_valid_key_i nil)

(def ^:dynamic check_valid_key_used nil)

(def ^:dynamic decrypt_message_res nil)

(def ^:dynamic encrypt_message_res nil)

(def ^:dynamic get_random_key_chars nil)

(def ^:dynamic get_random_key_i nil)

(def ^:dynamic get_random_key_j nil)

(def ^:dynamic get_random_key_k nil)

(def ^:dynamic get_random_key_res nil)

(def ^:dynamic get_random_key_tmp nil)

(def ^:dynamic index_in_i nil)

(def ^:dynamic is_upper_i nil)

(def ^:dynamic translate_message_chars_a nil)

(def ^:dynamic translate_message_chars_b nil)

(def ^:dynamic translate_message_i nil)

(def ^:dynamic translate_message_idx nil)

(def ^:dynamic translate_message_mapped nil)

(def ^:dynamic translate_message_symbol nil)

(def ^:dynamic translate_message_tmp nil)

(def ^:dynamic translate_message_translated nil)

(def ^:dynamic translate_message_upper_symbol nil)

(def ^:dynamic main_LETTERS "ABCDEFGHIJKLMNOPQRSTUVWXYZ")

(def ^:dynamic main_LOWERCASE "abcdefghijklmnopqrstuvwxyz")

(def ^:dynamic main_seed 1)

(defn rand [rand_n]
  (try (do (alter-var-root (var main_seed) (fn [_] (mod (+ (* main_seed 1664525) 1013904223) 2147483647))) (throw (ex-info "return" {:v (mod main_seed rand_n)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn get_random_key []
  (binding [get_random_key_chars nil get_random_key_i nil get_random_key_j nil get_random_key_k nil get_random_key_res nil get_random_key_tmp nil] (try (do (set! get_random_key_chars nil) (set! get_random_key_i 0) (while (< get_random_key_i (count main_LETTERS)) (do (set! get_random_key_chars (conj get_random_key_chars (nth main_LETTERS get_random_key_i))) (set! get_random_key_i (+ get_random_key_i 1)))) (set! get_random_key_j (- (count get_random_key_chars) 1)) (while (> get_random_key_j 0) (do (set! get_random_key_k (rand (+ get_random_key_j 1))) (set! get_random_key_tmp (nth get_random_key_chars get_random_key_j)) (set! get_random_key_chars (assoc get_random_key_chars get_random_key_j (nth get_random_key_chars get_random_key_k))) (set! get_random_key_chars (assoc get_random_key_chars get_random_key_k get_random_key_tmp)) (set! get_random_key_j (- get_random_key_j 1)))) (set! get_random_key_res "") (set! get_random_key_i 0) (while (< get_random_key_i (count get_random_key_chars)) (do (set! get_random_key_res (str get_random_key_res (nth get_random_key_chars get_random_key_i))) (set! get_random_key_i (+ get_random_key_i 1)))) (throw (ex-info "return" {:v get_random_key_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn check_valid_key [check_valid_key_key]
  (binding [check_valid_key_ch nil check_valid_key_i nil check_valid_key_used nil] (try (do (when (not= (count check_valid_key_key) (count main_LETTERS)) (throw (ex-info "return" {:v false}))) (set! check_valid_key_used {}) (set! check_valid_key_i 0) (while (< check_valid_key_i (count check_valid_key_key)) (do (set! check_valid_key_ch (nth check_valid_key_key check_valid_key_i)) (when (nth check_valid_key_used check_valid_key_ch) (throw (ex-info "return" {:v false}))) (set! check_valid_key_used (assoc check_valid_key_used check_valid_key_ch true)) (set! check_valid_key_i (+ check_valid_key_i 1)))) (set! check_valid_key_i 0) (while (< check_valid_key_i (count main_LETTERS)) (do (set! check_valid_key_ch (nth main_LETTERS check_valid_key_i)) (when (not (nth check_valid_key_used check_valid_key_ch)) (throw (ex-info "return" {:v false}))) (set! check_valid_key_i (+ check_valid_key_i 1)))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn index_in [index_in_s index_in_ch]
  (binding [index_in_i nil] (try (do (set! index_in_i 0) (while (< index_in_i (count index_in_s)) (do (when (= (nth index_in_s index_in_i) index_in_ch) (throw (ex-info "return" {:v index_in_i}))) (set! index_in_i (+ index_in_i 1)))) (throw (ex-info "return" {:v (- 1)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn char_to_upper [char_to_upper_c]
  (binding [char_to_upper_i nil] (try (do (set! char_to_upper_i 0) (while (< char_to_upper_i (count main_LOWERCASE)) (do (when (= char_to_upper_c (nth main_LOWERCASE char_to_upper_i)) (throw (ex-info "return" {:v (nth main_LETTERS char_to_upper_i)}))) (set! char_to_upper_i (+ char_to_upper_i 1)))) (throw (ex-info "return" {:v char_to_upper_c}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn char_to_lower [char_to_lower_c]
  (binding [char_to_lower_i nil] (try (do (set! char_to_lower_i 0) (while (< char_to_lower_i (count main_LETTERS)) (do (when (= char_to_lower_c (nth main_LETTERS char_to_lower_i)) (throw (ex-info "return" {:v (nth main_LOWERCASE char_to_lower_i)}))) (set! char_to_lower_i (+ char_to_lower_i 1)))) (throw (ex-info "return" {:v char_to_lower_c}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn is_upper [is_upper_c]
  (binding [is_upper_i nil] (try (do (set! is_upper_i 0) (while (< is_upper_i (count main_LETTERS)) (do (when (= is_upper_c (nth main_LETTERS is_upper_i)) (throw (ex-info "return" {:v true}))) (set! is_upper_i (+ is_upper_i 1)))) (throw (ex-info "return" {:v false}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn translate_message [translate_message_key translate_message_message translate_message_mode]
  (binding [translate_message_chars_a nil translate_message_chars_b nil translate_message_i nil translate_message_idx nil translate_message_mapped nil translate_message_symbol nil translate_message_tmp nil translate_message_translated nil translate_message_upper_symbol nil] (try (do (set! translate_message_chars_a main_LETTERS) (set! translate_message_chars_b translate_message_key) (when (= translate_message_mode "decrypt") (do (set! translate_message_tmp translate_message_chars_a) (set! translate_message_chars_a translate_message_chars_b) (set! translate_message_chars_b translate_message_tmp))) (set! translate_message_translated "") (set! translate_message_i 0) (while (< translate_message_i (count translate_message_message)) (do (set! translate_message_symbol (nth translate_message_message translate_message_i)) (set! translate_message_upper_symbol (char_to_upper translate_message_symbol)) (set! translate_message_idx (index_in translate_message_chars_a translate_message_upper_symbol)) (if (>= translate_message_idx 0) (do (set! translate_message_mapped (nth translate_message_chars_b translate_message_idx)) (if (is_upper translate_message_symbol) (set! translate_message_translated (str translate_message_translated translate_message_mapped)) (set! translate_message_translated (str translate_message_translated (char_to_lower translate_message_mapped))))) (set! translate_message_translated (str translate_message_translated translate_message_symbol))) (set! translate_message_i (+ translate_message_i 1)))) (throw (ex-info "return" {:v translate_message_translated}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn encrypt_message [encrypt_message_key encrypt_message_message]
  (binding [encrypt_message_res nil] (try (do (set! encrypt_message_res (translate_message encrypt_message_key encrypt_message_message "encrypt")) (throw (ex-info "return" {:v encrypt_message_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn decrypt_message [decrypt_message_key decrypt_message_message]
  (binding [decrypt_message_res nil] (try (do (set! decrypt_message_res (translate_message decrypt_message_key decrypt_message_message "decrypt")) (throw (ex-info "return" {:v decrypt_message_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_key "LFWOAYUISVKMNXPBDCRJTQEGHZ")

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (encrypt_message main_key "Harshil Darji"))
      (println (decrypt_message main_key "Ilcrism Olcvs"))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
