(ns main (:refer-clojure :exclude [trim bin_to_decimal]))

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

(declare trim bin_to_decimal)

(def ^:dynamic bin_to_decimal_c nil)

(def ^:dynamic bin_to_decimal_decimal_number nil)

(def ^:dynamic bin_to_decimal_digit nil)

(def ^:dynamic bin_to_decimal_i nil)

(def ^:dynamic bin_to_decimal_is_negative nil)

(def ^:dynamic bin_to_decimal_s nil)

(def ^:dynamic bin_to_decimal_trimmed nil)

(def ^:dynamic trim_ch nil)

(def ^:dynamic trim_end nil)

(def ^:dynamic trim_start nil)

(defn trim [trim_s]
  (binding [trim_ch nil trim_end nil trim_start nil] (try (do (set! trim_start 0) (loop [while_flag_1 true] (when (and while_flag_1 (< trim_start (count trim_s))) (do (set! trim_ch (subs trim_s trim_start (min (+ trim_start 1) (count trim_s)))) (cond (and (and (and (not= trim_ch " ") (not= trim_ch "\n")) (not= trim_ch "\t")) (not= trim_ch "\r")) (recur false) :else (do (set! trim_start (+ trim_start 1)) (recur while_flag_1)))))) (set! trim_end (count trim_s)) (loop [while_flag_2 true] (when (and while_flag_2 (> trim_end trim_start)) (do (set! trim_ch (subs trim_s (- trim_end 1) (min trim_end (count trim_s)))) (cond (and (and (and (not= trim_ch " ") (not= trim_ch "\n")) (not= trim_ch "\t")) (not= trim_ch "\r")) (recur false) :else (do (set! trim_end (- trim_end 1)) (recur while_flag_2)))))) (throw (ex-info "return" {:v (subs trim_s trim_start (min trim_end (count trim_s)))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn bin_to_decimal [bin_to_decimal_bin_string]
  (binding [bin_to_decimal_c nil bin_to_decimal_decimal_number nil bin_to_decimal_digit nil bin_to_decimal_i nil bin_to_decimal_is_negative nil bin_to_decimal_s nil bin_to_decimal_trimmed nil] (try (do (set! bin_to_decimal_trimmed (trim bin_to_decimal_bin_string)) (when (= bin_to_decimal_trimmed "") (throw (Exception. "Empty string was passed to the function"))) (set! bin_to_decimal_is_negative false) (set! bin_to_decimal_s bin_to_decimal_trimmed) (when (= (subs bin_to_decimal_s 0 (min 1 (count bin_to_decimal_s))) "-") (do (set! bin_to_decimal_is_negative true) (set! bin_to_decimal_s (subs bin_to_decimal_s 1 (min (count bin_to_decimal_s) (count bin_to_decimal_s)))))) (set! bin_to_decimal_i 0) (while (< bin_to_decimal_i (count bin_to_decimal_s)) (do (set! bin_to_decimal_c (subs bin_to_decimal_s bin_to_decimal_i (min (+ bin_to_decimal_i 1) (count bin_to_decimal_s)))) (when (and (not= bin_to_decimal_c "0") (not= bin_to_decimal_c "1")) (throw (Exception. "Non-binary value was passed to the function"))) (set! bin_to_decimal_i (+ bin_to_decimal_i 1)))) (set! bin_to_decimal_decimal_number 0) (set! bin_to_decimal_i 0) (while (< bin_to_decimal_i (count bin_to_decimal_s)) (do (set! bin_to_decimal_c (subs bin_to_decimal_s bin_to_decimal_i (min (+ bin_to_decimal_i 1) (count bin_to_decimal_s)))) (set! bin_to_decimal_digit (Long/parseLong bin_to_decimal_c)) (set! bin_to_decimal_decimal_number (+ (* 2 bin_to_decimal_decimal_number) bin_to_decimal_digit)) (set! bin_to_decimal_i (+ bin_to_decimal_i 1)))) (if bin_to_decimal_is_negative (- bin_to_decimal_decimal_number) bin_to_decimal_decimal_number)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (bin_to_decimal "101")))
      (println (str (bin_to_decimal " 1010   ")))
      (println (str (bin_to_decimal "-11101")))
      (println (str (bin_to_decimal "0")))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
