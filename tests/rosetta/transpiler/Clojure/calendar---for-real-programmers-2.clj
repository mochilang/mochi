(ns main)

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare main_d main_day main_days main_daysInMonth main_m main_mi main_months main_qtr main_s main_start main_val main_week)

(def main_daysInMonth [31 28 31 30 31 30 31 31 30 31 30 31])

(def main_start [3 6 6 2 4 0 2 5 1 3 6 1])

(def main_months [" January " " February" "  March  " "  April  " "   May   " "   June  " "   July  " "  August " "September" " October " " November" " December"])

(def main_days ["Su" "Mo" "Tu" "We" "Th" "Fr" "Sa"])

(def main_qtr 0)

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println "                                [SNOOPY]\n")
      (println "                                  1969\n")
      (while (< main_qtr 4) (do (def main_mi 0) (while (< main_mi 3) (do (println (str (str "      " (nth main_months (+ (* main_qtr 3) main_mi))) "           ") false) (def main_mi (+ main_mi 1)))) (println "") (def main_mi 0) (while (< main_mi 3) (do (def main_d 0) (while (< main_d 7) (do (println (str " " (nth main_days main_d)) false) (def main_d (+ main_d 1)))) (println "     " false) (def main_mi (+ main_mi 1)))) (println "") (def main_week 0) (while (< main_week 6) (do (def main_mi 0) (while (< main_mi 3) (do (def main_day 0) (while (< main_day 7) (do (def main_m (+ (* main_qtr 3) main_mi)) (def main_val (+ (- (+ (* main_week 7) main_day) (nth main_start main_m)) 1)) (if (and (>= main_val 1) (<= main_val (nth main_daysInMonth main_m))) (do (def main_s (str main_val)) (when (= (count main_s) 1) (def main_s (str " " main_s))) (println (str " " main_s) false)) (println "   " false)) (def main_day (+ main_day 1)))) (println "     " false) (def main_mi (+ main_mi 1)))) (println "") (def main_week (+ main_week 1)))) (println "") (def main_qtr (+ main_qtr 1))))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
