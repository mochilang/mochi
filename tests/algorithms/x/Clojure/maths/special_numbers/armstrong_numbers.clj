(ns main (:refer-clojure :exclude [pow_int armstrong_number pluperfect_number narcissistic_number]))

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

(declare pow_int armstrong_number pluperfect_number narcissistic_number)

(def ^:dynamic armstrong_number_digits nil)

(def ^:dynamic armstrong_number_rem nil)

(def ^:dynamic armstrong_number_temp nil)

(def ^:dynamic armstrong_number_total nil)

(def ^:dynamic narcissistic_number_digits nil)

(def ^:dynamic narcissistic_number_rem nil)

(def ^:dynamic narcissistic_number_temp nil)

(def ^:dynamic narcissistic_number_total nil)

(def ^:dynamic pluperfect_number_digit_histogram nil)

(def ^:dynamic pluperfect_number_digit_total nil)

(def ^:dynamic pluperfect_number_i nil)

(def ^:dynamic pluperfect_number_rem nil)

(def ^:dynamic pluperfect_number_temp nil)

(def ^:dynamic pluperfect_number_total nil)

(def ^:dynamic pow_int_i nil)

(def ^:dynamic pow_int_result nil)

(defn pow_int [pow_int_base pow_int_exp]
  (binding [pow_int_i nil pow_int_result nil] (try (do (set! pow_int_result 1) (set! pow_int_i 0) (while (< pow_int_i pow_int_exp) (do (set! pow_int_result (* pow_int_result pow_int_base)) (set! pow_int_i (+ pow_int_i 1)))) (throw (ex-info "return" {:v pow_int_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn armstrong_number [armstrong_number_n]
  (binding [armstrong_number_digits nil armstrong_number_rem nil armstrong_number_temp nil armstrong_number_total nil] (try (do (when (< armstrong_number_n 1) (throw (ex-info "return" {:v false}))) (set! armstrong_number_digits 0) (set! armstrong_number_temp armstrong_number_n) (while (> armstrong_number_temp 0) (do (set! armstrong_number_temp (quot armstrong_number_temp 10)) (set! armstrong_number_digits (+ armstrong_number_digits 1)))) (set! armstrong_number_total 0) (set! armstrong_number_temp armstrong_number_n) (while (> armstrong_number_temp 0) (do (set! armstrong_number_rem (mod armstrong_number_temp 10)) (set! armstrong_number_total (+ armstrong_number_total (pow_int armstrong_number_rem armstrong_number_digits))) (set! armstrong_number_temp (quot armstrong_number_temp 10)))) (throw (ex-info "return" {:v (= armstrong_number_total armstrong_number_n)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn pluperfect_number [pluperfect_number_n]
  (binding [pluperfect_number_digit_histogram nil pluperfect_number_digit_total nil pluperfect_number_i nil pluperfect_number_rem nil pluperfect_number_temp nil pluperfect_number_total nil] (try (do (when (< pluperfect_number_n 1) (throw (ex-info "return" {:v false}))) (set! pluperfect_number_digit_histogram []) (set! pluperfect_number_i 0) (while (< pluperfect_number_i 10) (do (set! pluperfect_number_digit_histogram (conj pluperfect_number_digit_histogram 0)) (set! pluperfect_number_i (+ pluperfect_number_i 1)))) (set! pluperfect_number_digit_total 0) (set! pluperfect_number_temp pluperfect_number_n) (while (> pluperfect_number_temp 0) (do (set! pluperfect_number_rem (mod pluperfect_number_temp 10)) (set! pluperfect_number_digit_histogram (assoc pluperfect_number_digit_histogram pluperfect_number_rem (+ (nth pluperfect_number_digit_histogram pluperfect_number_rem) 1))) (set! pluperfect_number_digit_total (+ pluperfect_number_digit_total 1)) (set! pluperfect_number_temp (quot pluperfect_number_temp 10)))) (set! pluperfect_number_total 0) (set! pluperfect_number_i 0) (while (< pluperfect_number_i 10) (do (when (> (nth pluperfect_number_digit_histogram pluperfect_number_i) 0) (set! pluperfect_number_total (+ pluperfect_number_total (* (nth pluperfect_number_digit_histogram pluperfect_number_i) (pow_int pluperfect_number_i pluperfect_number_digit_total))))) (set! pluperfect_number_i (+ pluperfect_number_i 1)))) (throw (ex-info "return" {:v (= pluperfect_number_total pluperfect_number_n)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn narcissistic_number [narcissistic_number_n]
  (binding [narcissistic_number_digits nil narcissistic_number_rem nil narcissistic_number_temp nil narcissistic_number_total nil] (try (do (when (< narcissistic_number_n 1) (throw (ex-info "return" {:v false}))) (set! narcissistic_number_digits 0) (set! narcissistic_number_temp narcissistic_number_n) (while (> narcissistic_number_temp 0) (do (set! narcissistic_number_temp (quot narcissistic_number_temp 10)) (set! narcissistic_number_digits (+ narcissistic_number_digits 1)))) (set! narcissistic_number_temp narcissistic_number_n) (set! narcissistic_number_total 0) (while (> narcissistic_number_temp 0) (do (set! narcissistic_number_rem (mod narcissistic_number_temp 10)) (set! narcissistic_number_total (+ narcissistic_number_total (pow_int narcissistic_number_rem narcissistic_number_digits))) (set! narcissistic_number_temp (quot narcissistic_number_temp 10)))) (throw (ex-info "return" {:v (= narcissistic_number_total narcissistic_number_n)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (armstrong_number 371))
      (println (armstrong_number 200))
      (println (pluperfect_number 371))
      (println (pluperfect_number 200))
      (println (narcissistic_number 371))
      (println (narcissistic_number 200))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
