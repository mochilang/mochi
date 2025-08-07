(ns main (:refer-clojure :exclude [panic trim_spaces char_to_digit oct_to_decimal main]))

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

(declare panic trim_spaces char_to_digit oct_to_decimal main)

(def ^:dynamic oct_to_decimal_ch nil)

(def ^:dynamic oct_to_decimal_decimal_number nil)

(def ^:dynamic oct_to_decimal_digit nil)

(def ^:dynamic oct_to_decimal_i nil)

(def ^:dynamic oct_to_decimal_is_negative nil)

(def ^:dynamic oct_to_decimal_s nil)

(def ^:dynamic trim_spaces_end nil)

(def ^:dynamic trim_spaces_start nil)

(defn panic [panic_msg]
  (println panic_msg))

(defn trim_spaces [trim_spaces_s]
  (binding [trim_spaces_end nil trim_spaces_start nil] (try (do (set! trim_spaces_start 0) (set! trim_spaces_end (- (count trim_spaces_s) 1)) (while (and (<= trim_spaces_start trim_spaces_end) (= (subs trim_spaces_s trim_spaces_start (min (+ trim_spaces_start 1) (count trim_spaces_s))) " ")) (set! trim_spaces_start (+ trim_spaces_start 1))) (while (and (>= trim_spaces_end trim_spaces_start) (= (subs trim_spaces_s trim_spaces_end (min (+ trim_spaces_end 1) (count trim_spaces_s))) " ")) (set! trim_spaces_end (- trim_spaces_end 1))) (if (> trim_spaces_start trim_spaces_end) "" (subs trim_spaces_s trim_spaces_start (min (+ trim_spaces_end 1) (count trim_spaces_s))))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn char_to_digit [char_to_digit_ch]
  (try (do (when (= char_to_digit_ch "0") (throw (ex-info "return" {:v 0}))) (when (= char_to_digit_ch "1") (throw (ex-info "return" {:v 1}))) (when (= char_to_digit_ch "2") (throw (ex-info "return" {:v 2}))) (when (= char_to_digit_ch "3") (throw (ex-info "return" {:v 3}))) (when (= char_to_digit_ch "4") (throw (ex-info "return" {:v 4}))) (when (= char_to_digit_ch "5") (throw (ex-info "return" {:v 5}))) (when (= char_to_digit_ch "6") (throw (ex-info "return" {:v 6}))) (when (= char_to_digit_ch "7") (throw (ex-info "return" {:v 7}))) (panic "Non-octal value was passed to the function") (throw (ex-info "return" {:v 0}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn oct_to_decimal [oct_to_decimal_oct_string]
  (binding [oct_to_decimal_ch nil oct_to_decimal_decimal_number nil oct_to_decimal_digit nil oct_to_decimal_i nil oct_to_decimal_is_negative nil oct_to_decimal_s nil] (try (do (set! oct_to_decimal_s (trim_spaces oct_to_decimal_oct_string)) (when (= (count oct_to_decimal_s) 0) (do (panic "Empty string was passed to the function") (throw (ex-info "return" {:v 0})))) (set! oct_to_decimal_is_negative false) (when (= (subs oct_to_decimal_s 0 (min 1 (count oct_to_decimal_s))) "-") (do (set! oct_to_decimal_is_negative true) (set! oct_to_decimal_s (subs oct_to_decimal_s 1 (min (count oct_to_decimal_s) (count oct_to_decimal_s)))))) (when (= (count oct_to_decimal_s) 0) (do (panic "Non-octal value was passed to the function") (throw (ex-info "return" {:v 0})))) (set! oct_to_decimal_decimal_number 0) (set! oct_to_decimal_i 0) (while (< oct_to_decimal_i (count oct_to_decimal_s)) (do (set! oct_to_decimal_ch (subs oct_to_decimal_s oct_to_decimal_i (min (+ oct_to_decimal_i 1) (count oct_to_decimal_s)))) (set! oct_to_decimal_digit (char_to_digit oct_to_decimal_ch)) (set! oct_to_decimal_decimal_number (+ (* 8 oct_to_decimal_decimal_number) oct_to_decimal_digit)) (set! oct_to_decimal_i (+ oct_to_decimal_i 1)))) (when oct_to_decimal_is_negative (set! oct_to_decimal_decimal_number (- oct_to_decimal_decimal_number))) (throw (ex-info "return" {:v oct_to_decimal_decimal_number}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (do (println (str (oct_to_decimal "1"))) (println (str (oct_to_decimal "-1"))) (println (str (oct_to_decimal "12"))) (println (str (oct_to_decimal " 12   "))) (println (str (oct_to_decimal "-45"))) (println (str (oct_to_decimal "0")))))

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
