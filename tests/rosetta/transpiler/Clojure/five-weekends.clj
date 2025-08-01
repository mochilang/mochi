(ns main (:refer-clojure :exclude [weekday main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare weekday main)

(declare a b c count_v day firstM firstY hasOne haveNone j k lastM lastY mm months31 names yy)

(defn weekday [y m d]
  (try (do (def yy y) (def mm m) (when (< mm 3) (do (def mm (+' mm 12)) (def yy (- yy 1)))) (def k (mod yy 100)) (def j (int (/ yy 100))) (def a (int (/ (* 13 (+' mm 1)) 5))) (def b (int (/ k 4))) (def c (int (/ j 4))) (throw (ex-info "return" {:v (mod (+' (+' (+' (+' (+' d a) k) b) c) (* 5 j)) 7)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (do (def months31 [1 3 5 7 8 10 12]) (def names ["January" "February" "March" "April" "May" "June" "July" "August" "September" "October" "November" "December"]) (def count_v 0) (def firstY 0) (def firstM 0) (def lastY 0) (def lastM 0) (def haveNone []) (println "Months with five weekends:") (doseq [year (range 1900 2101)] (do (def hasOne false) (doseq [m months31] (when (= (weekday year m 1) 6) (do (println (str (str (str "  " (str year)) " ") (nth names (- m 1)))) (def count_v (+' count_v 1)) (def hasOne true) (def lastY year) (def lastM m) (when (= firstY 0) (do (def firstY year) (def firstM m)))))) (when (not hasOne) (def haveNone (conj haveNone year))))) (println (str (str count_v) " total")) (println "") (println "First five dates of weekends:") (dotimes [i 5] (do (def day (+' 1 (* 7 i))) (println (str (str (str (str (str "  Friday, " (nth names (- firstM 1))) " ") (str day)) ", ") (str firstY))))) (println "Last five dates of weekends:") (dotimes [i 5] (do (def day (+' 1 (* 7 i))) (println (str (str (str (str (str "  Friday, " (nth names (- lastM 1))) " ") (str day)) ", ") (str lastY))))) (println "") (println "Years with no months with five weekends:") (doseq [y haveNone] (println (str "  " (str y)))) (println (str (str (count haveNone)) " total"))))

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
