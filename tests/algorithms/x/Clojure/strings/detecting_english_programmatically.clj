(ns main (:refer-clojure :exclude [to_upper char_in remove_non_letters split_spaces load_dictionary get_english_count is_english]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(defn indexOf [s sub]
  (let [idx (clojure.string/index-of s sub)] (if (nil? idx) -1 idx)))

(defn split [s sep]
  (clojure.string/split s (re-pattern sep)))

(defn toi [s]
  (Integer/parseInt (str s)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare to_upper char_in remove_non_letters split_spaces load_dictionary get_english_count is_english)

(def ^:dynamic char_in_i nil)

(def ^:dynamic get_english_count_cleaned nil)

(def ^:dynamic get_english_count_matches nil)

(def ^:dynamic get_english_count_possible nil)

(def ^:dynamic get_english_count_total nil)

(def ^:dynamic get_english_count_upper nil)

(def ^:dynamic is_english_letters_match nil)

(def ^:dynamic is_english_letters_pct nil)

(def ^:dynamic is_english_num_letters nil)

(def ^:dynamic is_english_words_match nil)

(def ^:dynamic load_dictionary_dict nil)

(def ^:dynamic load_dictionary_words nil)

(def ^:dynamic remove_non_letters_ch nil)

(def ^:dynamic remove_non_letters_i nil)

(def ^:dynamic remove_non_letters_res nil)

(def ^:dynamic split_spaces_ch nil)

(def ^:dynamic split_spaces_current nil)

(def ^:dynamic split_spaces_i nil)

(def ^:dynamic split_spaces_res nil)

(def ^:dynamic to_upper_c nil)

(def ^:dynamic to_upper_i nil)

(def ^:dynamic to_upper_j nil)

(def ^:dynamic to_upper_res nil)

(def ^:dynamic to_upper_up nil)

(def ^:dynamic main_LETTERS_AND_SPACE "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz \t\n")

(def ^:dynamic main_LOWER "abcdefghijklmnopqrstuvwxyz")

(def ^:dynamic main_UPPER "ABCDEFGHIJKLMNOPQRSTUVWXYZ")

(defn to_upper [to_upper_s]
  (binding [to_upper_c nil to_upper_i nil to_upper_j nil to_upper_res nil to_upper_up nil] (try (do (set! to_upper_res "") (set! to_upper_i 0) (while (< to_upper_i (count to_upper_s)) (do (set! to_upper_c (subs to_upper_s to_upper_i (min (+ to_upper_i 1) (count to_upper_s)))) (set! to_upper_j 0) (set! to_upper_up to_upper_c) (loop [while_flag_1 true] (when (and while_flag_1 (< to_upper_j (count main_LOWER))) (cond (= to_upper_c (subs main_LOWER to_upper_j (min (+ to_upper_j 1) (count main_LOWER)))) (do (set! to_upper_up (subs main_UPPER to_upper_j (min (+ to_upper_j 1) (count main_UPPER)))) (recur false)) :else (do (set! to_upper_j (+ to_upper_j 1)) (recur while_flag_1))))) (set! to_upper_res (str to_upper_res to_upper_up)) (set! to_upper_i (+ to_upper_i 1)))) (throw (ex-info "return" {:v to_upper_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn char_in [char_in_chars char_in_c]
  (binding [char_in_i nil] (try (do (set! char_in_i 0) (while (< char_in_i (count char_in_chars)) (do (when (= (subs char_in_chars char_in_i (min (+ char_in_i 1) (count char_in_chars))) char_in_c) (throw (ex-info "return" {:v true}))) (set! char_in_i (+ char_in_i 1)))) (throw (ex-info "return" {:v false}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn remove_non_letters [remove_non_letters_message]
  (binding [remove_non_letters_ch nil remove_non_letters_i nil remove_non_letters_res nil] (try (do (set! remove_non_letters_res "") (set! remove_non_letters_i 0) (while (< remove_non_letters_i (count remove_non_letters_message)) (do (set! remove_non_letters_ch (subs remove_non_letters_message remove_non_letters_i (min (+ remove_non_letters_i 1) (count remove_non_letters_message)))) (when (char_in main_LETTERS_AND_SPACE remove_non_letters_ch) (set! remove_non_letters_res (str remove_non_letters_res remove_non_letters_ch))) (set! remove_non_letters_i (+ remove_non_letters_i 1)))) (throw (ex-info "return" {:v remove_non_letters_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn split_spaces [split_spaces_text]
  (binding [split_spaces_ch nil split_spaces_current nil split_spaces_i nil split_spaces_res nil] (try (do (set! split_spaces_res []) (set! split_spaces_current "") (set! split_spaces_i 0) (while (< split_spaces_i (count split_spaces_text)) (do (set! split_spaces_ch (subs split_spaces_text split_spaces_i (min (+ split_spaces_i 1) (count split_spaces_text)))) (if (= split_spaces_ch " ") (do (set! split_spaces_res (conj split_spaces_res split_spaces_current)) (set! split_spaces_current "")) (set! split_spaces_current (str split_spaces_current split_spaces_ch))) (set! split_spaces_i (+ split_spaces_i 1)))) (set! split_spaces_res (conj split_spaces_res split_spaces_current)) (throw (ex-info "return" {:v split_spaces_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn load_dictionary []
  (binding [load_dictionary_dict nil load_dictionary_words nil] (try (do (set! load_dictionary_words ["HELLO" "WORLD" "HOW" "ARE" "YOU" "THE" "QUICK" "BROWN" "FOX" "JUMPS" "OVER" "LAZY" "DOG"]) (set! load_dictionary_dict {}) (doseq [w load_dictionary_words] (set! load_dictionary_dict (assoc load_dictionary_dict w true))) (throw (ex-info "return" {:v load_dictionary_dict}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_ENGLISH_WORDS (load_dictionary))

(defn get_english_count [get_english_count_message]
  (binding [get_english_count_cleaned nil get_english_count_matches nil get_english_count_possible nil get_english_count_total nil get_english_count_upper nil] (try (do (set! get_english_count_upper (to_upper get_english_count_message)) (set! get_english_count_cleaned (remove_non_letters get_english_count_upper)) (set! get_english_count_possible (split_spaces get_english_count_cleaned)) (set! get_english_count_matches 0) (set! get_english_count_total 0) (doseq [w get_english_count_possible] (when (not= w "") (do (set! get_english_count_total (+ get_english_count_total 1)) (when (in w main_ENGLISH_WORDS) (set! get_english_count_matches (+ get_english_count_matches 1)))))) (if (= get_english_count_total 0) 0.0 (/ (double get_english_count_matches) (double get_english_count_total)))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn is_english [is_english_message is_english_word_percentage is_english_letter_percentage]
  (binding [is_english_letters_match nil is_english_letters_pct nil is_english_num_letters nil is_english_words_match nil] (try (do (set! is_english_words_match (>= (* (get_english_count is_english_message) 100.0) (double is_english_word_percentage))) (set! is_english_num_letters (count (remove_non_letters is_english_message))) (set! is_english_letters_pct (if (= (count is_english_message) 0) 0.0 (* (/ (double is_english_num_letters) (double (count is_english_message))) 100.0))) (set! is_english_letters_match (>= is_english_letters_pct (double is_english_letter_percentage))) (throw (ex-info "return" {:v (and is_english_words_match is_english_letters_match)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (is_english "Hello World" 20 85)))
      (println (str (is_english "llold HorWd" 20 85)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
