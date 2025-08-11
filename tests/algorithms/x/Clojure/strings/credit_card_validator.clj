(ns main (:refer-clojure :exclude [validate_initial_digits luhn_validation is_digit_string validate_credit_card_number main]))

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

(defn _fetch [url]
  {:data [{:from "" :intensity {:actual 0 :forecast 0 :index ""} :to ""}]})

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare validate_initial_digits luhn_validation is_digit_string validate_credit_card_number main)

(def ^:dynamic is_digit_string_c nil)

(def ^:dynamic is_digit_string_i nil)

(def ^:dynamic luhn_validation_double_digit nil)

(def ^:dynamic luhn_validation_i nil)

(def ^:dynamic luhn_validation_n nil)

(def ^:dynamic luhn_validation_sum nil)

(def ^:dynamic validate_credit_card_number_error_message nil)

(defn validate_initial_digits [validate_initial_digits_cc]
  (try (throw (ex-info "return" {:v (or (or (or (or (or (= (subs validate_initial_digits_cc 0 (min 2 (count validate_initial_digits_cc))) "34") (= (subs validate_initial_digits_cc 0 (min 2 (count validate_initial_digits_cc))) "35")) (= (subs validate_initial_digits_cc 0 (min 2 (count validate_initial_digits_cc))) "37")) (= (subs validate_initial_digits_cc 0 (min 1 (count validate_initial_digits_cc))) "4")) (= (subs validate_initial_digits_cc 0 (min 1 (count validate_initial_digits_cc))) "5")) (= (subs validate_initial_digits_cc 0 (min 1 (count validate_initial_digits_cc))) "6"))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn luhn_validation [luhn_validation_cc]
  (binding [luhn_validation_double_digit nil luhn_validation_i nil luhn_validation_n nil luhn_validation_sum nil] (try (do (set! luhn_validation_sum 0) (set! luhn_validation_double_digit false) (set! luhn_validation_i (- (count luhn_validation_cc) 1)) (while (>= luhn_validation_i 0) (do (set! luhn_validation_n (Long/parseLong (subs luhn_validation_cc luhn_validation_i (min (+ luhn_validation_i 1) (count luhn_validation_cc))))) (when luhn_validation_double_digit (do (set! luhn_validation_n (* luhn_validation_n 2)) (when (> luhn_validation_n 9) (set! luhn_validation_n (- luhn_validation_n 9))))) (set! luhn_validation_sum (+ luhn_validation_sum luhn_validation_n)) (set! luhn_validation_double_digit (not luhn_validation_double_digit)) (set! luhn_validation_i (- luhn_validation_i 1)))) (throw (ex-info "return" {:v (= (mod luhn_validation_sum 10) 0)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn is_digit_string [is_digit_string_s]
  (binding [is_digit_string_c nil is_digit_string_i nil] (try (do (set! is_digit_string_i 0) (while (< is_digit_string_i (count is_digit_string_s)) (do (set! is_digit_string_c (subs is_digit_string_s is_digit_string_i (min (+ is_digit_string_i 1) (count is_digit_string_s)))) (when (or (< (compare is_digit_string_c "0") 0) (> (compare is_digit_string_c "9") 0)) (throw (ex-info "return" {:v false}))) (set! is_digit_string_i (+ is_digit_string_i 1)))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn validate_credit_card_number [validate_credit_card_number_cc]
  (binding [validate_credit_card_number_error_message nil] (try (do (set! validate_credit_card_number_error_message (str validate_credit_card_number_cc " is an invalid credit card number because")) (when (not (is_digit_string validate_credit_card_number_cc)) (do (println (str validate_credit_card_number_error_message " it has nonnumerical characters.")) (throw (ex-info "return" {:v false})))) (when (not (and (>= (count validate_credit_card_number_cc) 13) (<= (count validate_credit_card_number_cc) 16))) (do (println (str validate_credit_card_number_error_message " of its length.")) (throw (ex-info "return" {:v false})))) (when (not (validate_initial_digits validate_credit_card_number_cc)) (do (println (str validate_credit_card_number_error_message " of its first two digits.")) (throw (ex-info "return" {:v false})))) (when (not (luhn_validation validate_credit_card_number_cc)) (do (println (str validate_credit_card_number_error_message " it fails the Luhn check.")) (throw (ex-info "return" {:v false})))) (println (str validate_credit_card_number_cc " is a valid credit card number.")) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (do (validate_credit_card_number "4111111111111111") (validate_credit_card_number "32323")))

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
