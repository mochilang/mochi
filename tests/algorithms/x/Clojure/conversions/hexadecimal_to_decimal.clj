(ns main (:refer-clojure :exclude [strip hex_digit_value hex_to_decimal main]))

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

(declare strip hex_digit_value hex_to_decimal main)

(def ^:dynamic hex_to_decimal_c nil)

(def ^:dynamic hex_to_decimal_decimal_number nil)

(def ^:dynamic hex_to_decimal_i nil)

(def ^:dynamic hex_to_decimal_is_negative nil)

(def ^:dynamic hex_to_decimal_s nil)

(def ^:dynamic hex_to_decimal_value nil)

(def ^:dynamic strip_end nil)

(def ^:dynamic strip_start nil)

(defn strip [strip_s]
  (binding [strip_end nil strip_start nil] (try (do (set! strip_start 0) (set! strip_end (count strip_s)) (while (and (< strip_start strip_end) (= (subs strip_s strip_start (min (+ strip_start 1) (count strip_s))) " ")) (set! strip_start (+ strip_start 1))) (while (and (> strip_end strip_start) (= (subs strip_s (- strip_end 1) (min strip_end (count strip_s))) " ")) (set! strip_end (- strip_end 1))) (throw (ex-info "return" {:v (subs strip_s strip_start (min strip_end (count strip_s)))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn hex_digit_value [hex_digit_value_c]
  (try (do (when (= hex_digit_value_c "0") (throw (ex-info "return" {:v 0}))) (when (= hex_digit_value_c "1") (throw (ex-info "return" {:v 1}))) (when (= hex_digit_value_c "2") (throw (ex-info "return" {:v 2}))) (when (= hex_digit_value_c "3") (throw (ex-info "return" {:v 3}))) (when (= hex_digit_value_c "4") (throw (ex-info "return" {:v 4}))) (when (= hex_digit_value_c "5") (throw (ex-info "return" {:v 5}))) (when (= hex_digit_value_c "6") (throw (ex-info "return" {:v 6}))) (when (= hex_digit_value_c "7") (throw (ex-info "return" {:v 7}))) (when (= hex_digit_value_c "8") (throw (ex-info "return" {:v 8}))) (when (= hex_digit_value_c "9") (throw (ex-info "return" {:v 9}))) (when (or (= hex_digit_value_c "a") (= hex_digit_value_c "A")) (throw (ex-info "return" {:v 10}))) (when (or (= hex_digit_value_c "b") (= hex_digit_value_c "B")) (throw (ex-info "return" {:v 11}))) (when (or (= hex_digit_value_c "c") (= hex_digit_value_c "C")) (throw (ex-info "return" {:v 12}))) (when (or (= hex_digit_value_c "d") (= hex_digit_value_c "D")) (throw (ex-info "return" {:v 13}))) (when (or (= hex_digit_value_c "e") (= hex_digit_value_c "E")) (throw (ex-info "return" {:v 14}))) (when (or (= hex_digit_value_c "f") (= hex_digit_value_c "F")) (throw (ex-info "return" {:v 15}))) (println "Non-hexadecimal value was passed to the function") (throw (ex-info "return" {:v 0}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn hex_to_decimal [hex_to_decimal_hex_string]
  (binding [hex_to_decimal_c nil hex_to_decimal_decimal_number nil hex_to_decimal_i nil hex_to_decimal_is_negative nil hex_to_decimal_s nil hex_to_decimal_value nil] (try (do (set! hex_to_decimal_s (strip hex_to_decimal_hex_string)) (when (= (count hex_to_decimal_s) 0) (do (println "Empty string was passed to the function") (throw (ex-info "return" {:v 0})))) (set! hex_to_decimal_is_negative false) (when (= (subs hex_to_decimal_s 0 (min 1 (count hex_to_decimal_s))) "-") (do (set! hex_to_decimal_is_negative true) (set! hex_to_decimal_s (subs hex_to_decimal_s 1 (min (count hex_to_decimal_s) (count hex_to_decimal_s)))))) (set! hex_to_decimal_decimal_number 0) (set! hex_to_decimal_i 0) (while (< hex_to_decimal_i (count hex_to_decimal_s)) (do (set! hex_to_decimal_c (subs hex_to_decimal_s hex_to_decimal_i (min (+ hex_to_decimal_i 1) (count hex_to_decimal_s)))) (set! hex_to_decimal_value (hex_digit_value hex_to_decimal_c)) (set! hex_to_decimal_decimal_number (+ (* 16 hex_to_decimal_decimal_number) hex_to_decimal_value)) (set! hex_to_decimal_i (+ hex_to_decimal_i 1)))) (if hex_to_decimal_is_negative (- hex_to_decimal_decimal_number) hex_to_decimal_decimal_number)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (do (println (str (hex_to_decimal "a"))) (println (str (hex_to_decimal "12f"))) (println (str (hex_to_decimal "   12f   "))) (println (str (hex_to_decimal "FfFf"))) (println (str (hex_to_decimal "-Ff")))))

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
