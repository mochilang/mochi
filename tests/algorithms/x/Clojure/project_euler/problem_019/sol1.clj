(ns main (:refer-clojure :exclude [is_leap count_sundays]))

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

(declare is_leap count_sundays)

(declare _read_file)

(def ^:dynamic count_sundays_day nil)

(def ^:dynamic count_sundays_days_per_month nil)

(def ^:dynamic count_sundays_month nil)

(def ^:dynamic count_sundays_sundays nil)

(def ^:dynamic count_sundays_year nil)

(defn is_leap [is_leap_year]
  (try (if (or (and (= (mod is_leap_year 4) 0) (not= (mod is_leap_year 100) 0)) (= (mod is_leap_year 400) 0)) true false) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn count_sundays []
  (binding [count_sundays_day nil count_sundays_days_per_month nil count_sundays_month nil count_sundays_sundays nil count_sundays_year nil] (try (do (set! count_sundays_days_per_month [31 28 31 30 31 30 31 31 30 31 30 31]) (set! count_sundays_day 6) (set! count_sundays_month 1) (set! count_sundays_year 1901) (set! count_sundays_sundays 0) (while (< count_sundays_year 2001) (do (set! count_sundays_day (+ count_sundays_day 7)) (if (is_leap count_sundays_year) (if (and (> count_sundays_day (nth count_sundays_days_per_month (- count_sundays_month 1))) (not= count_sundays_month 2)) (do (set! count_sundays_month (+ count_sundays_month 1)) (set! count_sundays_day (- count_sundays_day (nth count_sundays_days_per_month (- count_sundays_month 2))))) (when (and (> count_sundays_day 29) (= count_sundays_month 2)) (do (set! count_sundays_month (+ count_sundays_month 1)) (set! count_sundays_day (- count_sundays_day 29))))) (when (> count_sundays_day (nth count_sundays_days_per_month (- count_sundays_month 1))) (do (set! count_sundays_month (+ count_sundays_month 1)) (set! count_sundays_day (- count_sundays_day (nth count_sundays_days_per_month (- count_sundays_month 2))))))) (when (> count_sundays_month 12) (do (set! count_sundays_year (+ count_sundays_year 1)) (set! count_sundays_month 1))) (when (and (< count_sundays_year 2001) (= count_sundays_day 1)) (set! count_sundays_sundays (+ count_sundays_sundays 1))))) (throw (ex-info "return" {:v count_sundays_sundays}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (count_sundays))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
