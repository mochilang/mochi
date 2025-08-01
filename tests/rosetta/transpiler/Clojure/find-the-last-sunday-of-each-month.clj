(ns main (:refer-clojure :exclude [leapYear monthDays zeller lastSunday monthName main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare leapYear monthDays zeller lastSunday monthName main)

(declare J K day days h m mm names year yy)

(defn leapYear [y]
  (try (throw (ex-info "return" {:v (or (and (= (mod y 4) 0) (not= (mod y 100) 0)) (= (mod y 400) 0))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn monthDays [y m]
  (try (do (def days [0 31 28 31 30 31 30 31 31 30 31 30 31]) (if (and (= m 2) (leapYear y)) 29 (nth days m))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn zeller [y m d]
  (try (do (def mm m) (def yy y) (when (< mm 3) (do (def mm (+' mm 12)) (def yy (- yy 1)))) (def K (mod yy 100)) (def J (/ yy 100)) (def h (mod (+' (+' (+' (+' (+' d (/ (* 13 (+' mm 1)) 5)) K) (/ K 4)) (/ J 4)) (* 5 J)) 7)) (throw (ex-info "return" {:v (mod (+' h 6) 7)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn lastSunday [y m]
  (try (do (def day (monthDays y m)) (while (and (> day 0) (not= (zeller y m day) 0)) (def day (- day 1))) (throw (ex-info "return" {:v day}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn monthName [m]
  (try (do (def names ["" "January" "February" "March" "April" "May" "June" "July" "August" "September" "October" "November" "December"]) (throw (ex-info "return" {:v (nth names m)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (do (def year (Integer/parseInt (read-line))) (println (str "Last Sundays of each month of " (str year))) (println "==================================") (def m 1) (while (<= m 12) (do (def day (lastSunday year m)) (println (str (str (monthName m) ": ") (str day))) (def m (+' m 1))))))

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
