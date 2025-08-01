(ns main (:refer-clojure :exclude [setChar]))

(require 'clojure.set)

(defrecord Stack [start len index])

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare frame i index j lenSeg main_height main_j main_lines main_row main_width seg stack start)

(declare setChar)

(def main_width 81)

(def main_height 5)

(def main_lines [])

(defn setChar [setChar_s setChar_idx setChar_ch]
  (try (throw (ex-info "return" {:v (str (str (subs setChar_s 0 setChar_idx) setChar_ch) (subs setChar_s (+ setChar_idx 1) (count setChar_s)))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(def stack [{"start" 0 "len" width "index" 1}])

(defn -main []
  (dotimes [i main_height] (do (def main_row "") (def main_j 0) (while (< main_j main_width) (do (def main_row (str main_row "*")) (def main_j (+ main_j 1)))) (def main_lines (conj main_lines main_row))))
  (loop [while_flag_1 true] (when (and while_flag_1 (> (count stack) 0)) (do (def frame (nth stack (- (count stack) 1))) (def stack (subvec stack 0 (- (count stack) 1))) (def start (get frame "start")) (def lenSeg (get frame "len")) (def index (get frame "index")) (def seg (int (/ lenSeg 3))) (cond (= seg 0) (recur true) :else (do (def i index) (while (< i height) (do (def j (+ start seg)) (while (< j (+ start (* 2 seg))) (do (def lines (assoc lines i (setChar (nth lines i) j " "))) (def j (+ j 1)))) (def i (+ i 1)))) (def stack (conj stack {"start" start "len" seg "index" (+ index 1)})) (def stack (conj stack {"start" (+ start (* seg 2)) "len" seg "index" (+ index 1)})) (recur while_flag_1))))))
  (doseq [line lines] (println line)))

(-main)
