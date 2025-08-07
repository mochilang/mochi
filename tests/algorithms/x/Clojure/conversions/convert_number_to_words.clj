(ns main (:refer-clojure :exclude [pow10 max_value join_words convert_small_number convert_number]))

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

(declare pow10 max_value join_words convert_small_number convert_number)

(def ^:dynamic convert_number_digit_group nil)

(def ^:dynamic convert_number_divisor nil)

(def ^:dynamic convert_number_i nil)

(def ^:dynamic convert_number_joined nil)

(def ^:dynamic convert_number_n nil)

(def ^:dynamic convert_number_power nil)

(def ^:dynamic convert_number_powers nil)

(def ^:dynamic convert_number_unit nil)

(def ^:dynamic convert_number_units nil)

(def ^:dynamic convert_number_word_group nil)

(def ^:dynamic convert_number_word_groups nil)

(def ^:dynamic convert_small_number_hyphen nil)

(def ^:dynamic convert_small_number_ones_digit nil)

(def ^:dynamic convert_small_number_tail nil)

(def ^:dynamic convert_small_number_tens_digit nil)

(def ^:dynamic join_words_i nil)

(def ^:dynamic join_words_res nil)

(def ^:dynamic pow10_i nil)

(def ^:dynamic pow10_res nil)

(def ^:dynamic main_ones ["zero" "one" "two" "three" "four" "five" "six" "seven" "eight" "nine"])

(def ^:dynamic main_teens ["ten" "eleven" "twelve" "thirteen" "fourteen" "fifteen" "sixteen" "seventeen" "eighteen" "nineteen"])

(def ^:dynamic main_tens ["" "" "twenty" "thirty" "forty" "fifty" "sixty" "seventy" "eighty" "ninety"])

(def ^:dynamic main_short_powers [15 12 9 6 3 2])

(def ^:dynamic main_short_units ["quadrillion" "trillion" "billion" "million" "thousand" "hundred"])

(def ^:dynamic main_long_powers [15 9 6 3 2])

(def ^:dynamic main_long_units ["billiard" "milliard" "million" "thousand" "hundred"])

(def ^:dynamic main_indian_powers [14 12 7 5 3 2])

(def ^:dynamic main_indian_units ["crore crore" "lakh crore" "crore" "lakh" "thousand" "hundred"])

(defn pow10 [pow10_exp]
  (binding [pow10_i nil pow10_res nil] (try (do (set! pow10_res 1) (set! pow10_i 0) (while (< pow10_i pow10_exp) (do (set! pow10_res (* pow10_res 10)) (set! pow10_i (+ pow10_i 1)))) (throw (ex-info "return" {:v pow10_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn max_value [max_value_system]
  (try (do (when (= max_value_system "short") (throw (ex-info "return" {:v (- (pow10 18) 1)}))) (when (= max_value_system "long") (throw (ex-info "return" {:v (- (pow10 21) 1)}))) (if (= max_value_system "indian") (- (pow10 19) 1) 0)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn join_words [join_words_words]
  (binding [join_words_i nil join_words_res nil] (try (do (set! join_words_res "") (set! join_words_i 0) (while (< join_words_i (count join_words_words)) (do (when (> join_words_i 0) (set! join_words_res (str join_words_res " "))) (set! join_words_res (str join_words_res (nth join_words_words join_words_i))) (set! join_words_i (+ join_words_i 1)))) (throw (ex-info "return" {:v join_words_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn convert_small_number [convert_small_number_num]
  (binding [convert_small_number_hyphen nil convert_small_number_ones_digit nil convert_small_number_tail nil convert_small_number_tens_digit nil] (try (do (when (< convert_small_number_num 0) (throw (ex-info "return" {:v ""}))) (when (>= convert_small_number_num 100) (throw (ex-info "return" {:v ""}))) (set! convert_small_number_tens_digit (quot convert_small_number_num 10)) (set! convert_small_number_ones_digit (mod convert_small_number_num 10)) (when (= convert_small_number_tens_digit 0) (throw (ex-info "return" {:v (nth main_ones convert_small_number_ones_digit)}))) (when (= convert_small_number_tens_digit 1) (throw (ex-info "return" {:v (nth main_teens convert_small_number_ones_digit)}))) (set! convert_small_number_hyphen (if (> convert_small_number_ones_digit 0) "-" "")) (set! convert_small_number_tail (if (> convert_small_number_ones_digit 0) (nth main_ones convert_small_number_ones_digit) "")) (throw (ex-info "return" {:v (+ (+ (nth main_tens convert_small_number_tens_digit) convert_small_number_hyphen) convert_small_number_tail)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn convert_number [convert_number_num convert_number_system]
  (binding [convert_number_digit_group nil convert_number_divisor nil convert_number_i nil convert_number_joined nil convert_number_n nil convert_number_power nil convert_number_powers nil convert_number_unit nil convert_number_units nil convert_number_word_group nil convert_number_word_groups nil] (try (do (set! convert_number_word_groups []) (set! convert_number_n convert_number_num) (when (< convert_number_n 0) (do (set! convert_number_word_groups (conj convert_number_word_groups "negative")) (set! convert_number_n (- convert_number_n)))) (when (> convert_number_n (max_value convert_number_system)) (throw (ex-info "return" {:v ""}))) (set! convert_number_powers []) (set! convert_number_units []) (if (= convert_number_system "short") (do (set! convert_number_powers main_short_powers) (set! convert_number_units main_short_units)) (if (= convert_number_system "long") (do (set! convert_number_powers main_long_powers) (set! convert_number_units main_long_units)) (if (= convert_number_system "indian") (do (set! convert_number_powers main_indian_powers) (set! convert_number_units main_indian_units)) (throw (ex-info "return" {:v ""}))))) (set! convert_number_i 0) (while (< convert_number_i (count convert_number_powers)) (do (set! convert_number_power (nth convert_number_powers convert_number_i)) (set! convert_number_unit (nth convert_number_units convert_number_i)) (set! convert_number_divisor (pow10 convert_number_power)) (set! convert_number_digit_group (/ convert_number_n convert_number_divisor)) (set! convert_number_n (mod convert_number_n convert_number_divisor)) (when (> convert_number_digit_group 0) (do (set! convert_number_word_group (if (>= convert_number_digit_group 100) (convert_number convert_number_digit_group convert_number_system) (convert_small_number convert_number_digit_group))) (set! convert_number_word_groups (conj convert_number_word_groups (str (str convert_number_word_group " ") convert_number_unit))))) (set! convert_number_i (+ convert_number_i 1)))) (when (or (> convert_number_n 0) (= (count convert_number_word_groups) 0)) (set! convert_number_word_groups (conj convert_number_word_groups (convert_small_number convert_number_n)))) (set! convert_number_joined (join_words convert_number_word_groups)) (throw (ex-info "return" {:v convert_number_joined}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (convert_number 123456789012345 "short"))
      (println (convert_number 123456789012345 "long"))
      (println (convert_number 123456789012345 "indian"))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
