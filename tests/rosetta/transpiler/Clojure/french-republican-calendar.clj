(ns main (:refer-clojure :exclude [greLeap repLeap greToDay repToDay dayToGre dayToRep formatRep formatGre]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare greLeap repLeap greToDay repToDay dayToGre dayToRep formatRep formatGre)

(declare a b c d dd gre gregorian gregorianStr m mm rep republicanStr sansculotidesStr sc y yy)

(def gregorianStr ["January" "February" "March" "April" "May" "June" "July" "August" "September" "October" "November" "December"])

(def gregorian [31 28 31 30 31 30 31 31 30 31 30 31])

(def republicanStr ["Vendemiaire" "Brumaire" "Frimaire" "Nivose" "Pluviose" "Ventose" "Germinal" "Floreal" "Prairial" "Messidor" "Thermidor" "Fructidor"])

(def sansculotidesStr ["Fete de la vertu" "Fete du genie" "Fete du travail" "Fete de l'opinion" "Fete des recompenses" "Fete de la Revolution"])

(defn greLeap [year]
  (try (do (def a (int (mod year 4))) (def b (int (mod year 100))) (def c (int (mod year 400))) (throw (ex-info "return" {:v (and (= a 0) (or (not= b 0) (= c 0)))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn repLeap [year]
  (try (do (def a (int (mod (+' year 1) 4))) (def b (int (mod (+' year 1) 100))) (def c (int (mod (+' year 1) 400))) (throw (ex-info "return" {:v (and (= a 0) (or (not= b 0) (= c 0)))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn greToDay [d m y]
  (try (do (def yy y) (def mm m) (when (< mm 3) (do (def yy (- yy 1)) (def mm (+' mm 12)))) (throw (ex-info "return" {:v (- (+' (+' (+' (- (/ (* yy 36525) 100) (/ yy 100)) (/ yy 400)) (/ (* 306 (+' mm 1)) 10)) d) 654842)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn repToDay [d m y]
  (try (do (def dd d) (def mm m) (when (= mm 13) (do (def mm (- mm 1)) (def dd (+' dd 30)))) (when (repLeap y) (def dd (- dd 1))) (throw (ex-info "return" {:v (- (+' (+' (+' (- (+' (* 365 y) (/ (+' y 1) 4)) (/ (+' y 1) 100)) (/ (+' y 1) 400)) (* 30 mm)) dd) 395)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn dayToGre [day]
  (try (do (def y (/ (* day 100) 36525)) (def d (+' (- day (/ (* y 36525) 100)) 21)) (def y (+' y 1792)) (def d (- (- (+' d (/ y 100)) (/ y 400)) 13)) (def m 8) (while (> d (nth gregorian m)) (do (def d (- d (nth gregorian m))) (def m (+' m 1)) (when (= m 12) (do (def m 0) (def y (+' y 1)) (if (greLeap y) (def gregorian (assoc gregorian 1 29)) (def gregorian (assoc gregorian 1 28))))))) (def m (+' m 1)) (throw (ex-info "return" {:v [d m y]}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn dayToRep [day]
  (try (do (def y (/ (* (- day 1) 100) 36525)) (when (repLeap y) (def y (- y 1))) (def d (- (+' (+' (- day (/ (* (+' y 1) 36525) 100)) 365) (/ (+' y 1) 100)) (/ (+' y 1) 400))) (def y (+' y 1)) (def m 1) (def sc 5) (when (repLeap y) (def sc 6)) (while (> d 30) (do (def d (- d 30)) (def m (+' m 1)) (when (= m 13) (when (> d sc) (do (def d (- d sc)) (def m 1) (def y (+' y 1)) (def sc 5) (when (repLeap y) (def sc 6))))))) (throw (ex-info "return" {:v [d m y]}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn formatRep [d m y]
  (try (if (= m 13) (str (str (nth sansculotidesStr (- d 1)) " ") (str y)) (str (str (str (str (str d) " ") (nth republicanStr (- m 1))) " ") (str y))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn formatGre [d m y]
  (try (throw (ex-info "return" {:v (str (str (str (str (str d) " ") (nth gregorianStr (- m 1))) " ") (str y))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(def rep (dayToRep (greToDay 20 5 1795)))

(def gre (dayToGre (repToDay 1 9 3)))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (formatRep (nth rep 0) (nth rep 1) (nth rep 2)))
      (println (formatGre (nth gre 0) (nth gre 1) (nth gre 2)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
