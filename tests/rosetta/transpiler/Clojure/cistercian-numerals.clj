(ns main (:refer-clojure :exclude [initN horiz verti diagd diagu initDraw printNumeral]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare initN horiz verti diagd diagu initDraw printNumeral)

(declare diagd_c diagu_c horiz_c initN_i initN_j initN_row main_draw main_hundreds main_n main_num main_numbers main_ones main_tens main_thousands printNumeral_i printNumeral_j printNumeral_line verti_r)

(def main_n [])

(defn initN []
  (do (def initN_i 0) (while (< initN_i 15) (do (def initN_row []) (def initN_j 0) (while (< initN_j 11) (do (def initN_row (conj initN_row " ")) (def initN_j (+ initN_j 1)))) (def initN_row (assoc initN_row 5 "x")) (def initN_n (conj main_n initN_row)) (def initN_i (+ initN_i 1))))))

(defn horiz [horiz_c1 horiz_c2 horiz_r]
  (do (def horiz_c horiz_c1) (while (<= horiz_c horiz_c2) (do (def horiz_n (assoc-in horiz_n [horiz_r horiz_c] "x")) (def horiz_c (+ horiz_c 1))))))

(defn verti [verti_r1 verti_r2 verti_c]
  (do (def verti_r verti_r1) (while (<= verti_r verti_r2) (do (def verti_n (assoc-in verti_n [verti_r verti_c] "x")) (def verti_r (+ verti_r 1))))))

(defn diagd [diagd_c1 diagd_c2 diagd_r]
  (do (def diagd_c diagd_c1) (while (<= diagd_c diagd_c2) (do (def diagd_n (assoc-in diagd_n [(- (+ diagd_r diagd_c) diagd_c1) diagd_c] "x")) (def diagd_c (+ diagd_c 1))))))

(defn diagu [diagu_c1 diagu_c2 diagu_r]
  (do (def diagu_c diagu_c1) (while (<= diagu_c diagu_c2) (do (def diagu_n (assoc-in diagu_n [(+ (- diagu_r diagu_c) diagu_c1) diagu_c] "x")) (def diagu_c (+ diagu_c 1))))))

(def main_draw {})

(defn initDraw []
  (do (def initDraw_draw (assoc initDraw_draw 1 (fn [] (horiz 6 10 0)))) (def initDraw_draw (assoc initDraw_draw 2 (fn [] (horiz 6 10 4)))) (def initDraw_draw (assoc initDraw_draw 3 (fn [] (diagd 6 10 0)))) (def initDraw_draw (assoc initDraw_draw 4 (fn [] (diagu 6 10 4)))) (def initDraw_draw (assoc initDraw_draw 5 (fn [] (do ((nth initDraw_draw 1)) ((nth initDraw_draw 4)))))) (def initDraw_draw (assoc initDraw_draw 6 (fn [] (verti 0 4 10)))) (def initDraw_draw (assoc initDraw_draw 7 (fn [] (do ((nth initDraw_draw 1)) ((nth initDraw_draw 6)))))) (def initDraw_draw (assoc initDraw_draw 8 (fn [] (do ((nth initDraw_draw 2)) ((nth initDraw_draw 6)))))) (def initDraw_draw (assoc initDraw_draw 9 (fn [] (do ((nth initDraw_draw 1)) ((nth initDraw_draw 8)))))) (def initDraw_draw (assoc initDraw_draw 10 (fn [] (horiz 0 4 0)))) (def initDraw_draw (assoc initDraw_draw 20 (fn [] (horiz 0 4 4)))) (def initDraw_draw (assoc initDraw_draw 30 (fn [] (diagu 0 4 4)))) (def initDraw_draw (assoc initDraw_draw 40 (fn [] (diagd 0 4 0)))) (def initDraw_draw (assoc initDraw_draw 50 (fn [] (do ((nth initDraw_draw 10)) ((nth initDraw_draw 40)))))) (def initDraw_draw (assoc initDraw_draw 60 (fn [] (verti 0 4 0)))) (def initDraw_draw (assoc initDraw_draw 70 (fn [] (do ((nth initDraw_draw 10)) ((nth initDraw_draw 60)))))) (def initDraw_draw (assoc initDraw_draw 80 (fn [] (do ((nth initDraw_draw 20)) ((nth initDraw_draw 60)))))) (def initDraw_draw (assoc initDraw_draw 90 (fn [] (do ((nth initDraw_draw 10)) ((nth initDraw_draw 80)))))) (def initDraw_draw (assoc initDraw_draw 100 (fn [] (horiz 6 10 14)))) (def initDraw_draw (assoc initDraw_draw 200 (fn [] (horiz 6 10 10)))) (def initDraw_draw (assoc initDraw_draw 300 (fn [] (diagu 6 10 14)))) (def initDraw_draw (assoc initDraw_draw 400 (fn [] (diagd 6 10 10)))) (def initDraw_draw (assoc initDraw_draw 500 (fn [] (do ((nth initDraw_draw 100)) ((nth initDraw_draw 400)))))) (def initDraw_draw (assoc initDraw_draw 600 (fn [] (verti 10 14 10)))) (def initDraw_draw (assoc initDraw_draw 700 (fn [] (do ((nth initDraw_draw 100)) ((nth initDraw_draw 600)))))) (def initDraw_draw (assoc initDraw_draw 800 (fn [] (do ((nth initDraw_draw 200)) ((nth initDraw_draw 600)))))) (def initDraw_draw (assoc initDraw_draw 900 (fn [] (do ((nth initDraw_draw 100)) ((nth initDraw_draw 800)))))) (def initDraw_draw (assoc initDraw_draw 1000 (fn [] (horiz 0 4 14)))) (def initDraw_draw (assoc initDraw_draw 2000 (fn [] (horiz 0 4 10)))) (def initDraw_draw (assoc initDraw_draw 3000 (fn [] (diagd 0 4 10)))) (def initDraw_draw (assoc initDraw_draw 4000 (fn [] (diagu 0 4 14)))) (def initDraw_draw (assoc initDraw_draw 5000 (fn [] (do ((nth initDraw_draw 1000)) ((nth initDraw_draw 4000)))))) (def initDraw_draw (assoc initDraw_draw 6000 (fn [] (verti 10 14 0)))) (def initDraw_draw (assoc initDraw_draw 7000 (fn [] (do ((nth initDraw_draw 1000)) ((nth initDraw_draw 6000)))))) (def initDraw_draw (assoc initDraw_draw 8000 (fn [] (do ((nth initDraw_draw 2000)) ((nth initDraw_draw 6000)))))) (def initDraw_draw (assoc initDraw_draw 9000 (fn [] (do ((nth initDraw_draw 1000)) ((nth initDraw_draw 8000))))))))

(defn printNumeral []
  (do (def printNumeral_i 0) (while (< printNumeral_i 15) (do (def printNumeral_line "") (def printNumeral_j 0) (while (< printNumeral_j 11) (do (def printNumeral_line (str (+ printNumeral_line (nth (nth main_n printNumeral_i) printNumeral_j)) " ")) (def printNumeral_j (+ printNumeral_j 1)))) (println printNumeral_line) (def printNumeral_i (+ printNumeral_i 1)))) (println "")))

(def main_numbers [0 1 20 300 4000 5555 6789 9999])

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (initDraw)
      (doseq [number main_numbers] (do (initN) (println (str (str number) ":")) (def main_num number) (def main_thousands (/ main_num 1000)) (def main_num (mod main_num 1000)) (def main_hundreds (/ main_num 100)) (def main_num (mod main_num 100)) (def main_tens (/ main_num 10)) (def main_ones (mod main_num 10)) (when (> main_thousands 0) ((nth main_draw (* main_thousands 1000)))) (when (> main_hundreds 0) ((nth main_draw (* main_hundreds 100)))) (when (> main_tens 0) ((nth main_draw (* main_tens 10)))) (when (> main_ones 0) ((nth main_draw main_ones))) (printNumeral)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
