(ns main (:refer-clojure :exclude [parse_decimal zeller_day zeller test_zeller main]))

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

(declare parse_decimal zeller_day zeller test_zeller main)

(def ^:dynamic parse_decimal_c nil)

(def ^:dynamic parse_decimal_i nil)

(def ^:dynamic parse_decimal_value nil)

(def ^:dynamic test_zeller_expected nil)

(def ^:dynamic test_zeller_i nil)

(def ^:dynamic test_zeller_inputs nil)

(def ^:dynamic test_zeller_res nil)

(def ^:dynamic zeller_day nil)

(def ^:dynamic zeller_day_c nil)

(def ^:dynamic zeller_day_d nil)

(def ^:dynamic zeller_day_days nil)

(def ^:dynamic zeller_day_f nil)

(def ^:dynamic zeller_day_k nil)

(def ^:dynamic zeller_day_m nil)

(def ^:dynamic zeller_day_month nil)

(def ^:dynamic zeller_day_sep1 nil)

(def ^:dynamic zeller_day_sep2 nil)

(def ^:dynamic zeller_day_t nil)

(def ^:dynamic zeller_day_u nil)

(def ^:dynamic zeller_day_v nil)

(def ^:dynamic zeller_day_w nil)

(def ^:dynamic zeller_day_x nil)

(def ^:dynamic zeller_day_y nil)

(def ^:dynamic zeller_day_year nil)

(def ^:dynamic zeller_day_z nil)

(defn parse_decimal [parse_decimal_s]
  (binding [parse_decimal_c nil parse_decimal_i nil parse_decimal_value nil] (try (do (set! parse_decimal_value 0) (set! parse_decimal_i 0) (while (< parse_decimal_i (count parse_decimal_s)) (do (set! parse_decimal_c (subs parse_decimal_s parse_decimal_i (+ parse_decimal_i 1))) (when (or (< (compare parse_decimal_c "0") 0) (> (compare parse_decimal_c "9") 0)) (throw (Exception. "invalid literal"))) (set! parse_decimal_value (+ (* parse_decimal_value 10) (Long/parseLong parse_decimal_c))) (set! parse_decimal_i (+ parse_decimal_i 1)))) (throw (ex-info "return" {:v parse_decimal_value}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn zeller_day [zeller_day_date_input]
  (binding [zeller_day_c nil zeller_day_d nil zeller_day_days nil zeller_day_f nil zeller_day_k nil zeller_day_m nil zeller_day_month nil zeller_day_sep1 nil zeller_day_sep2 nil zeller_day_t nil zeller_day_u nil zeller_day_v nil zeller_day_w nil zeller_day_x nil zeller_day_y nil zeller_day_year nil zeller_day_z nil] (try (do (set! zeller_day_days {0 "Sunday" 1 "Monday" 2 "Tuesday" 3 "Wednesday" 4 "Thursday" 5 "Friday" 6 "Saturday"}) (when (not= (count zeller_day_date_input) 10) (throw (Exception. "Must be 10 characters long"))) (set! zeller_day_m (parse_decimal (subs zeller_day_date_input 0 (min 2 (count zeller_day_date_input))))) (when (or (<= zeller_day_m 0) (>= zeller_day_m 13)) (throw (Exception. "Month must be between 1 - 12"))) (set! zeller_day_sep1 (subs zeller_day_date_input 2 (+ 2 1))) (when (and (not= zeller_day_sep1 "-") (not= zeller_day_sep1 "/")) (throw (Exception. "Date separator must be '-' or '/'"))) (set! zeller_day_d (parse_decimal (subs zeller_day_date_input 3 (min 5 (count zeller_day_date_input))))) (when (or (<= zeller_day_d 0) (>= zeller_day_d 32)) (throw (Exception. "Date must be between 1 - 31"))) (set! zeller_day_sep2 (subs zeller_day_date_input 5 (+ 5 1))) (when (and (not= zeller_day_sep2 "-") (not= zeller_day_sep2 "/")) (throw (Exception. "Date separator must be '-' or '/'"))) (set! zeller_day_y (parse_decimal (subs zeller_day_date_input 6 (min 10 (count zeller_day_date_input))))) (when (or (<= zeller_day_y 45) (>= zeller_day_y 8500)) (throw (Exception. "Year out of range. There has to be some sort of limit...right?"))) (set! zeller_day_year zeller_day_y) (set! zeller_day_month zeller_day_m) (when (<= zeller_day_month 2) (do (set! zeller_day_year (- zeller_day_year 1)) (set! zeller_day_month (+ zeller_day_month 12)))) (set! zeller_day_c (quot zeller_day_year 100)) (set! zeller_day_k (mod zeller_day_year 100)) (set! zeller_day_t (int (- (* 2.6 (double zeller_day_month)) 5.39))) (set! zeller_day_u (quot zeller_day_c 4)) (set! zeller_day_v (quot zeller_day_k 4)) (set! zeller_day_x (+ zeller_day_d zeller_day_k)) (set! zeller_day_z (+ (+ (+ zeller_day_t zeller_day_u) zeller_day_v) zeller_day_x)) (set! zeller_day_w (- zeller_day_z (* 2 zeller_day_c))) (set! zeller_day_f (mod zeller_day_w 7)) (when (< zeller_day_f 0) (set! zeller_day_f (+ zeller_day_f 7))) (throw (ex-info "return" {:v (get zeller_day_days zeller_day_f)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn zeller [zeller_date_input]
  (binding [zeller_day nil] (try (do (set! zeller_day (zeller_day zeller_date_input)) (throw (ex-info "return" {:v (str (str (str (str "Your date " zeller_date_input) ", is a ") zeller_day) "!")}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn test_zeller []
  (binding [test_zeller_expected nil test_zeller_i nil test_zeller_inputs nil test_zeller_res nil] (do (set! test_zeller_inputs ["01-31-2010" "02-01-2010" "11-26-2024" "07-04-1776"]) (set! test_zeller_expected ["Sunday" "Monday" "Tuesday" "Thursday"]) (set! test_zeller_i 0) (while (< test_zeller_i (count test_zeller_inputs)) (do (set! test_zeller_res (zeller_day (nth test_zeller_inputs test_zeller_i))) (when (not= test_zeller_res (nth test_zeller_expected test_zeller_i)) (throw (Exception. "zeller test failed"))) (set! test_zeller_i (+ test_zeller_i 1)))))))

(defn main []
  (do (test_zeller) (println (zeller "01-31-2010"))))

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
