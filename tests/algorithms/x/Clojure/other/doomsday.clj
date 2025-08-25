(ns main (:refer-clojure :exclude [get_week_day]))

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
  (int (Double/valueOf (str s))))

(defn _ord [s]
  (int (first s)))

(defn mochi_str [v]
  (cond (float? v) (let [s (str v)] (if (clojure.string/ends-with? s ".0") (subs s 0 (- (count s) 2)) s)) :else (str v)))

(defn _fetch [url]
  {:data [{:from "" :intensity {:actual 0 :forecast 0 :index ""} :to ""}]})

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare get_week_day)

(declare _read_file)

(def ^:dynamic get_week_day_centurian nil)

(def ^:dynamic get_week_day_centurian_m nil)

(def ^:dynamic get_week_day_century nil)

(def ^:dynamic get_week_day_century_anchor nil)

(def ^:dynamic get_week_day_day_anchor nil)

(def ^:dynamic get_week_day_dooms_day nil)

(def ^:dynamic get_week_day_week_day nil)

(def ^:dynamic main_DOOMSDAY_LEAP nil)

(def ^:dynamic main_DOOMSDAY_NOT_LEAP nil)

(def ^:dynamic main_WEEK_DAY_NAMES nil)

(defn get_week_day [get_week_day_year get_week_day_month get_week_day_day]
  (binding [get_week_day_centurian nil get_week_day_centurian_m nil get_week_day_century nil get_week_day_century_anchor nil get_week_day_day_anchor nil get_week_day_dooms_day nil get_week_day_week_day nil] (try (do (when (< get_week_day_year 100) (throw (Exception. "year should be in YYYY format"))) (when (or (< get_week_day_month 1) (> get_week_day_month 12)) (throw (Exception. "month should be between 1 to 12"))) (when (or (< get_week_day_day 1) (> get_week_day_day 31)) (throw (Exception. "day should be between 1 to 31"))) (set! get_week_day_century (/ get_week_day_year 100)) (set! get_week_day_century_anchor (mod (+' (*' 5 (mod get_week_day_century 4)) 2) 7)) (set! get_week_day_centurian (mod get_week_day_year 100)) (set! get_week_day_centurian_m (mod get_week_day_centurian 12)) (set! get_week_day_dooms_day (mod (+' (+' (+' (/ get_week_day_centurian 12) get_week_day_centurian_m) (/ get_week_day_centurian_m 4)) get_week_day_century_anchor) 7)) (set! get_week_day_day_anchor (if (or (not= (mod get_week_day_year 4) 0) (and (= get_week_day_centurian 0) (not= (mod get_week_day_year 400) 0))) (nth main_DOOMSDAY_NOT_LEAP (- get_week_day_month 1)) (nth main_DOOMSDAY_LEAP (- get_week_day_month 1)))) (set! get_week_day_week_day (mod (- (+' get_week_day_dooms_day get_week_day_day) get_week_day_day_anchor) 7)) (when (< get_week_day_week_day 0) (set! get_week_day_week_day (+' get_week_day_week_day 7))) (throw (ex-info "return" {:v (get main_WEEK_DAY_NAMES get_week_day_week_day)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (alter-var-root (var main_DOOMSDAY_LEAP) (constantly [4 1 7 4 2 6 4 1 5 3 7 5]))
      (alter-var-root (var main_DOOMSDAY_NOT_LEAP) (constantly [3 7 7 4 2 6 4 1 5 3 7 5]))
      (alter-var-root (var main_WEEK_DAY_NAMES) (constantly {0 "Sunday" 1 "Monday" 2 "Tuesday" 3 "Wednesday" 4 "Thursday" 5 "Friday" 6 "Saturday"}))
      (println (get_week_day 2020 10 24))
      (println (get_week_day 2017 10 24))
      (println (get_week_day 2019 5 3))
      (println (get_week_day 1970 9 16))
      (println (get_week_day 1870 8 13))
      (println (get_week_day 2040 3 14))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
