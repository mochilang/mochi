(ns main (:refer-clojure :exclude [sinApprox floor absFloat absInt parseIntStr parseDate leap daysInMonth addDays pad2 dateString day biorhythms main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare sinApprox floor absFloat absInt parseIntStr parseDate leap daysInMonth addDays pad2 dateString day biorhythms main)

(defn sinApprox [x]
  (try (do (def term x) (def sum x) (def n 1) (while (<= n 8) (do (def denom (double (* (* 2 n) (+ (* 2 n) 1)))) (def term (/ (* (* (- term) x) x) denom)) (def sum (+ sum term)) (def n (+ n 1)))) (throw (ex-info "return" {:v sum}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn floor [x]
  (try (do (def i (int x)) (when (> (double i) x) (def i (- i 1))) (throw (ex-info "return" {:v (double i)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn absFloat [x]
  (try (if (< x 0.0) (- x) x) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn absInt [n]
  (try (if (< n 0) (- n) n) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn parseIntStr [str]
  (try (do (def i 0) (def neg false) (when (and (> (count str) 0) (= (subs str 0 1) "-")) (do (def neg true) (def i 1))) (def n 0) (def digits {"0" 0 "1" 1 "2" 2 "3" 3 "4" 4 "5" 5 "6" 6 "7" 7 "8" 8 "9" 9}) (while (< i (count str)) (do (def n (+ (* n 10) (get digits (subs str i (+ i 1))))) (def i (+ i 1)))) (when neg (def n (- n))) (throw (ex-info "return" {:v n}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn parseDate [s]
  (try (do (def y (parseIntStr (subs s 0 4))) (def m (parseIntStr (subs s 5 7))) (def d (parseIntStr (subs s 8 10))) (throw (ex-info "return" {:v [y m d]}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn leap [y]
  (try (do (when (= (mod y 400) 0) (throw (ex-info "return" {:v true}))) (if (= (mod y 100) 0) false (= (mod y 4) 0))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn daysInMonth [y m]
  (try (do (def feb (if (leap y) 29 28)) (def lengths [31 feb 31 30 31 30 31 31 30 31 30 31]) (throw (ex-info "return" {:v (nth lengths (- m 1))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn addDays [y m d n]
  (try (do (def yy y) (def mm m) (def dd d) (if (>= n 0) (do (def i 0) (while (< i n) (do (def dd (+ dd 1)) (when (> dd (daysInMonth yy mm)) (do (def dd 1) (def mm (+ mm 1)) (when (> mm 12) (do (def mm 1) (def yy (+ yy 1)))))) (def i (+ i 1))))) (do (def i 0) (while (> i n) (do (def dd (- dd 1)) (when (< dd 1) (do (def mm (- mm 1)) (when (< mm 1) (do (def mm 12) (def yy (- yy 1)))) (def dd (daysInMonth yy mm)))) (def i (- i 1)))))) (throw (ex-info "return" {:v [yy mm dd]}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn pad2 [n]
  (try (if (< n 10) (str "0" (str n)) (str n)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn dateString [y m d]
  (try (throw (ex-info "return" {:v (str (str (str (str (str y) "-") (pad2 m)) "-") (pad2 d))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn day [y m d]
  (try (do (def part1 (* 367 y)) (def part2 (int (/ (* 7 (int (+ y (/ (+ m 9) 12)))) 4))) (def part3 (int (/ (* 275 m) 9))) (throw (ex-info "return" {:v (- (+ (+ (- part1 part2) part3) d) 730530)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn biorhythms [birth target]
  (do (def bparts (parseDate birth)) (def by (nth bparts 0)) (def bm (nth bparts 1)) (def bd (nth bparts 2)) (def tparts (parseDate target)) (def ty (nth tparts 0)) (def tm (nth tparts 1)) (def td (nth tparts 2)) (def diff (absInt (- (day ty tm td) (day by bm bd)))) (println (str (str (str "Born " birth) ", Target ") target)) (println (str "Day " (str diff))) (def cycles ["Physical day " "Emotional day" "Mental day   "]) (def lengths [23 28 33]) (def quadrants [["up and rising" "peak"] ["up but falling" "transition"] ["down and falling" "valley"] ["down but rising" "transition"]]) (def i 0) (while (< i 3) (do (def length (nth lengths i)) (def cycle (nth cycles i)) (def position (mod diff length)) (def quadrant (/ (* position 4) length)) (def percent (sinApprox (/ (* (* 2.0 PI) (double position)) (double length)))) (def percent (/ (floor (* percent 1000.0)) 10.0)) (def description "") (if (> percent 95.0) (def description " peak") (if (< percent (- 95.0)) (def description " valley") (if (< (absFloat percent) 5.0) (def description " critical transition") (do (def daysToAdd (- (/ (* (+ quadrant 1) length) 4) position)) (def res (addDays ty tm td daysToAdd)) (def ny (nth res 0)) (def nm (nth res 1)) (def nd (nth res 2)) (def transition (dateString ny nm nd)) (def trend (nth (nth quadrants quadrant) 0)) (def next (nth (nth quadrants quadrant) 1)) (def pct (str percent)) (when (not (contains pct ".")) (def pct (str pct ".0"))) (def description (str (str (str (str (str (str (str (str " " pct) "% (") trend) ", next ") next) " ") transition) ")")))))) (def posStr (str position)) (when (< position 10) (def posStr (str " " posStr))) (println (str (str (str cycle posStr) " : ") description)) (def i (+ i 1)))) (println "")))

(defn main []
  (do (def pairs [["1943-03-09" "1972-07-11"] ["1809-01-12" "1863-11-19"] ["1809-02-12" "1863-11-19"]]) (def idx 0) (while (< idx (count pairs)) (do (def p (nth pairs idx)) (biorhythms (nth p 0) (nth p 1)) (def idx (+ idx 1))))))

(defn -main []
  (def PI 3.141592653589793)
  (def TWO_PI 6.283185307179586)
  (main))

(-main)
