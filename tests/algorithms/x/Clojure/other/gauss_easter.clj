(ns main (:refer-clojure :exclude [gauss_easter format_date]))

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

(declare gauss_easter format_date)

(def ^:dynamic format_date_day nil)

(def ^:dynamic format_date_month nil)

(def ^:dynamic gauss_easter_century_starting_point nil)

(def ^:dynamic gauss_easter_days_from_phm_to_sunday nil)

(def ^:dynamic gauss_easter_days_to_add nil)

(def ^:dynamic gauss_easter_julian_leap_year nil)

(def ^:dynamic gauss_easter_leap_day_inhibits nil)

(def ^:dynamic gauss_easter_leap_day_reinstall_number nil)

(def ^:dynamic gauss_easter_lunar_orbit_correction nil)

(def ^:dynamic gauss_easter_metonic_cycle nil)

(def ^:dynamic gauss_easter_non_leap_year nil)

(def ^:dynamic gauss_easter_offset nil)

(def ^:dynamic gauss_easter_secular_moon_shift nil)

(def ^:dynamic gauss_easter_total nil)

(def ^:dynamic main_i nil)

(defn gauss_easter [gauss_easter_year]
  (binding [gauss_easter_century_starting_point nil gauss_easter_days_from_phm_to_sunday nil gauss_easter_days_to_add nil gauss_easter_julian_leap_year nil gauss_easter_leap_day_inhibits nil gauss_easter_leap_day_reinstall_number nil gauss_easter_lunar_orbit_correction nil gauss_easter_metonic_cycle nil gauss_easter_non_leap_year nil gauss_easter_offset nil gauss_easter_secular_moon_shift nil gauss_easter_total nil] (try (do (set! gauss_easter_metonic_cycle (mod gauss_easter_year 19)) (set! gauss_easter_julian_leap_year (mod gauss_easter_year 4)) (set! gauss_easter_non_leap_year (mod gauss_easter_year 7)) (set! gauss_easter_leap_day_inhibits (/ gauss_easter_year 100)) (set! gauss_easter_lunar_orbit_correction (/ (+ 13 (* 8 gauss_easter_leap_day_inhibits)) 25)) (set! gauss_easter_leap_day_reinstall_number (/ (double gauss_easter_leap_day_inhibits) 4.0)) (set! gauss_easter_secular_moon_shift (mod (- (+ (- 15.0 (double gauss_easter_lunar_orbit_correction)) (double gauss_easter_leap_day_inhibits)) gauss_easter_leap_day_reinstall_number) 30.0)) (set! gauss_easter_century_starting_point (mod (- (+ 4.0 (double gauss_easter_leap_day_inhibits)) gauss_easter_leap_day_reinstall_number) 7.0)) (set! gauss_easter_days_to_add (mod (+ (* 19.0 (double gauss_easter_metonic_cycle)) gauss_easter_secular_moon_shift) 30.0)) (set! gauss_easter_days_from_phm_to_sunday (mod (+ (+ (+ (* 2.0 (double gauss_easter_julian_leap_year)) (* 4.0 (double gauss_easter_non_leap_year))) (* 6.0 gauss_easter_days_to_add)) gauss_easter_century_starting_point) 7.0)) (when (and (= gauss_easter_days_to_add 29.0) (= gauss_easter_days_from_phm_to_sunday 6.0)) (throw (ex-info "return" {:v {:day 19 :month 4}}))) (when (and (= gauss_easter_days_to_add 28.0) (= gauss_easter_days_from_phm_to_sunday 6.0)) (throw (ex-info "return" {:v {:day 18 :month 4}}))) (set! gauss_easter_offset (long (+ gauss_easter_days_to_add gauss_easter_days_from_phm_to_sunday))) (set! gauss_easter_total (+ 22 gauss_easter_offset)) (if (> gauss_easter_total 31) {:day (- gauss_easter_total 31) :month 4} {:day gauss_easter_total :month 3})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn format_date [format_date_year format_date_d]
  (binding [format_date_day nil format_date_month nil] (try (do (set! format_date_month (if (< (:month format_date_d) 10) (str "0" (str (:month format_date_d))) (str (:month format_date_d)))) (set! format_date_day (if (< (:day format_date_d) 10) (str "0" (str (:day format_date_d))) (str (:day format_date_d)))) (throw (ex-info "return" {:v (str (str (str (str (str format_date_year) "-") format_date_month) "-") format_date_day)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_years [1994 2000 2010 2021 2023 2032 2100])

(def ^:dynamic main_i 0)

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (while (< main_i (count main_years)) (do (def ^:dynamic main_y (nth main_years main_i)) (def ^:dynamic main_e (gauss_easter main_y)) (println (str (str (str "Easter in " (str main_y)) " is ") (format_date main_y main_e))) (def main_i (+ main_i 1))))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
