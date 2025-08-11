(ns main (:refer-clojure :exclude [to_upper is_digit clean_id is_spain_national_id main]))

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

(declare to_upper is_digit clean_id is_spain_national_id main)

(def ^:dynamic clean_id_ch nil)

(def ^:dynamic clean_id_cleaned nil)

(def ^:dynamic clean_id_i nil)

(def ^:dynamic clean_id_upper_id nil)

(def ^:dynamic is_digit_i nil)

(def ^:dynamic is_spain_national_id_expected nil)

(def ^:dynamic is_spain_national_id_i nil)

(def ^:dynamic is_spain_national_id_letter nil)

(def ^:dynamic is_spain_national_id_number nil)

(def ^:dynamic is_spain_national_id_sid nil)

(def ^:dynamic to_upper_ch nil)

(def ^:dynamic to_upper_converted nil)

(def ^:dynamic to_upper_i nil)

(def ^:dynamic to_upper_j nil)

(def ^:dynamic to_upper_res nil)

(def ^:dynamic main_DIGITS "0123456789")

(def ^:dynamic main_UPPER "ABCDEFGHIJKLMNOPQRSTUVWXYZ")

(def ^:dynamic main_LOWER "abcdefghijklmnopqrstuvwxyz")

(def ^:dynamic main_LOOKUP_LETTERS "TRWAGMYFPDXBNJZSQVHLCKE")

(def ^:dynamic main_ERROR_MSG "Input must be a string of 8 numbers plus letter")

(defn to_upper [to_upper_s]
  (binding [to_upper_ch nil to_upper_converted nil to_upper_i nil to_upper_j nil to_upper_res nil] (try (do (set! to_upper_res "") (set! to_upper_i 0) (while (< to_upper_i (count to_upper_s)) (do (set! to_upper_ch (subs to_upper_s to_upper_i (+ to_upper_i 1))) (set! to_upper_j 0) (set! to_upper_converted to_upper_ch) (loop [while_flag_1 true] (when (and while_flag_1 (< to_upper_j (count main_LOWER))) (cond (= (subs main_LOWER to_upper_j (+ to_upper_j 1)) to_upper_ch) (do (set! to_upper_converted (subs main_UPPER to_upper_j (+ to_upper_j 1))) (recur false)) :else (do (set! to_upper_j (+ to_upper_j 1)) (recur while_flag_1))))) (set! to_upper_res (str to_upper_res to_upper_converted)) (set! to_upper_i (+ to_upper_i 1)))) (throw (ex-info "return" {:v to_upper_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn is_digit [is_digit_ch]
  (binding [is_digit_i nil] (try (do (set! is_digit_i 0) (while (< is_digit_i (count main_DIGITS)) (do (when (= (subs main_DIGITS is_digit_i (+ is_digit_i 1)) is_digit_ch) (throw (ex-info "return" {:v true}))) (set! is_digit_i (+ is_digit_i 1)))) (throw (ex-info "return" {:v false}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn clean_id [clean_id_spanish_id]
  (binding [clean_id_ch nil clean_id_cleaned nil clean_id_i nil clean_id_upper_id nil] (try (do (set! clean_id_upper_id (to_upper clean_id_spanish_id)) (set! clean_id_cleaned "") (set! clean_id_i 0) (while (< clean_id_i (count clean_id_upper_id)) (do (set! clean_id_ch (subs clean_id_upper_id clean_id_i (+ clean_id_i 1))) (when (not= clean_id_ch "-") (set! clean_id_cleaned (str clean_id_cleaned clean_id_ch))) (set! clean_id_i (+ clean_id_i 1)))) (throw (ex-info "return" {:v clean_id_cleaned}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn is_spain_national_id [is_spain_national_id_spanish_id]
  (binding [is_spain_national_id_expected nil is_spain_national_id_i nil is_spain_national_id_letter nil is_spain_national_id_number nil is_spain_national_id_sid nil] (try (do (set! is_spain_national_id_sid (clean_id is_spain_national_id_spanish_id)) (when (not= (count is_spain_national_id_sid) 9) (throw (Exception. main_ERROR_MSG))) (set! is_spain_national_id_i 0) (while (< is_spain_national_id_i 8) (do (when (not (is_digit (subs is_spain_national_id_sid is_spain_national_id_i (+ is_spain_national_id_i 1)))) (throw (Exception. main_ERROR_MSG))) (set! is_spain_national_id_i (+ is_spain_national_id_i 1)))) (set! is_spain_national_id_number (int (subs is_spain_national_id_sid 0 (min 8 (count is_spain_national_id_sid))))) (set! is_spain_national_id_letter (subs is_spain_national_id_sid 8 (+ 8 1))) (when (is_digit is_spain_national_id_letter) (throw (Exception. main_ERROR_MSG))) (set! is_spain_national_id_expected (subs main_LOOKUP_LETTERS (mod is_spain_national_id_number 23) (+ (mod is_spain_national_id_number 23) 1))) (throw (ex-info "return" {:v (= is_spain_national_id_letter is_spain_national_id_expected)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (do (println (is_spain_national_id "12345678Z")) (println (is_spain_national_id "12345678z")) (println (is_spain_national_id "12345678x")) (println (is_spain_national_id "12345678I")) (println (is_spain_national_id "12345678-Z"))))

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
