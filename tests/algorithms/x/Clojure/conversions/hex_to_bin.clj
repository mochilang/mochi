(ns main (:refer-clojure :exclude [panic trim_spaces hex_digit_value hex_to_bin]))

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

(declare panic trim_spaces hex_digit_value hex_to_bin)

(def ^:dynamic hex_to_bin_bin_str nil)

(def ^:dynamic hex_to_bin_ch nil)

(def ^:dynamic hex_to_bin_i nil)

(def ^:dynamic hex_to_bin_int_num nil)

(def ^:dynamic hex_to_bin_is_negative nil)

(def ^:dynamic hex_to_bin_n nil)

(def ^:dynamic hex_to_bin_result nil)

(def ^:dynamic hex_to_bin_s nil)

(def ^:dynamic hex_to_bin_trimmed nil)

(def ^:dynamic hex_to_bin_val nil)

(def ^:dynamic trim_spaces_end nil)

(def ^:dynamic trim_spaces_start nil)

(defn panic [panic_msg]
  (println panic_msg))

(defn trim_spaces [trim_spaces_s]
  (binding [trim_spaces_end nil trim_spaces_start nil] (try (do (set! trim_spaces_start 0) (set! trim_spaces_end (count trim_spaces_s)) (while (and (< trim_spaces_start trim_spaces_end) (= (subs trim_spaces_s trim_spaces_start (min (+ trim_spaces_start 1) (count trim_spaces_s))) " ")) (set! trim_spaces_start (+ trim_spaces_start 1))) (while (and (> trim_spaces_end trim_spaces_start) (= (subs trim_spaces_s (- trim_spaces_end 1) (min trim_spaces_end (count trim_spaces_s))) " ")) (set! trim_spaces_end (- trim_spaces_end 1))) (throw (ex-info "return" {:v (subs trim_spaces_s trim_spaces_start (min trim_spaces_end (count trim_spaces_s)))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn hex_digit_value [hex_digit_value_ch]
  (try (do (when (= hex_digit_value_ch "0") (throw (ex-info "return" {:v 0}))) (when (= hex_digit_value_ch "1") (throw (ex-info "return" {:v 1}))) (when (= hex_digit_value_ch "2") (throw (ex-info "return" {:v 2}))) (when (= hex_digit_value_ch "3") (throw (ex-info "return" {:v 3}))) (when (= hex_digit_value_ch "4") (throw (ex-info "return" {:v 4}))) (when (= hex_digit_value_ch "5") (throw (ex-info "return" {:v 5}))) (when (= hex_digit_value_ch "6") (throw (ex-info "return" {:v 6}))) (when (= hex_digit_value_ch "7") (throw (ex-info "return" {:v 7}))) (when (= hex_digit_value_ch "8") (throw (ex-info "return" {:v 8}))) (when (= hex_digit_value_ch "9") (throw (ex-info "return" {:v 9}))) (when (or (= hex_digit_value_ch "a") (= hex_digit_value_ch "A")) (throw (ex-info "return" {:v 10}))) (when (or (= hex_digit_value_ch "b") (= hex_digit_value_ch "B")) (throw (ex-info "return" {:v 11}))) (when (or (= hex_digit_value_ch "c") (= hex_digit_value_ch "C")) (throw (ex-info "return" {:v 12}))) (when (or (= hex_digit_value_ch "d") (= hex_digit_value_ch "D")) (throw (ex-info "return" {:v 13}))) (when (or (= hex_digit_value_ch "e") (= hex_digit_value_ch "E")) (throw (ex-info "return" {:v 14}))) (when (or (= hex_digit_value_ch "f") (= hex_digit_value_ch "F")) (throw (ex-info "return" {:v 15}))) (panic "Invalid value was passed to the function")) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn hex_to_bin [hex_to_bin_hex_num]
  (binding [hex_to_bin_bin_str nil hex_to_bin_ch nil hex_to_bin_i nil hex_to_bin_int_num nil hex_to_bin_is_negative nil hex_to_bin_n nil hex_to_bin_result nil hex_to_bin_s nil hex_to_bin_trimmed nil hex_to_bin_val nil] (try (do (set! hex_to_bin_trimmed (trim_spaces hex_to_bin_hex_num)) (when (= (count hex_to_bin_trimmed) 0) (panic "No value was passed to the function")) (set! hex_to_bin_s hex_to_bin_trimmed) (set! hex_to_bin_is_negative false) (when (= (subs hex_to_bin_s 0 (min 1 (count hex_to_bin_s))) "-") (do (set! hex_to_bin_is_negative true) (set! hex_to_bin_s (subs hex_to_bin_s 1 (min (count hex_to_bin_s) (count hex_to_bin_s)))))) (set! hex_to_bin_int_num 0) (set! hex_to_bin_i 0) (while (< hex_to_bin_i (count hex_to_bin_s)) (do (set! hex_to_bin_ch (subs hex_to_bin_s hex_to_bin_i (min (+ hex_to_bin_i 1) (count hex_to_bin_s)))) (set! hex_to_bin_val (hex_digit_value hex_to_bin_ch)) (set! hex_to_bin_int_num (+ (* hex_to_bin_int_num 16) hex_to_bin_val)) (set! hex_to_bin_i (+ hex_to_bin_i 1)))) (set! hex_to_bin_bin_str "") (set! hex_to_bin_n hex_to_bin_int_num) (when (= hex_to_bin_n 0) (set! hex_to_bin_bin_str "0")) (while (> hex_to_bin_n 0) (do (set! hex_to_bin_bin_str (str (str (mod hex_to_bin_n 2)) hex_to_bin_bin_str)) (set! hex_to_bin_n (quot hex_to_bin_n 2)))) (set! hex_to_bin_result (Long/parseLong hex_to_bin_bin_str)) (when hex_to_bin_is_negative (set! hex_to_bin_result (- hex_to_bin_result))) (throw (ex-info "return" {:v hex_to_bin_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (hex_to_bin "AC")))
      (println (str (hex_to_bin "9A4")))
      (println (str (hex_to_bin "   12f   ")))
      (println (str (hex_to_bin "FfFf")))
      (println (str (hex_to_bin "-fFfF")))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
