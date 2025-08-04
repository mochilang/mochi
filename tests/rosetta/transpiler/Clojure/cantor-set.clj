(ns main (:refer-clojure :exclude [setChar]))

(require 'clojure.set)

(defrecord Stack [start len index])

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare main_frame main_height main_i main_index main_j main_lenSeg main_lines main_row main_seg main_stack main_start main_width)

(declare setChar)

(def main_width 81)

(def main_height 5)

(def main_lines [])

(defn setChar [setChar_s setChar_idx setChar_ch]
  (try (throw (ex-info "return" {:v (str (str (subs setChar_s 0 setChar_idx) setChar_ch) (subs setChar_s (+ setChar_idx 1) (count setChar_s)))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(def main_stack [{"start" 0 "len" main_width "index" 1}])

(defn -main []
  (dotimes [i main_height] (do (def main_row "") (def main_j 0) (while (< main_j main_width) (do (def main_row (str main_row "*")) (def main_j (+ main_j 1)))) (def main_lines (conj main_lines main_row))))
  (loop [while_flag_1 true] (when (and while_flag_1 (> (count main_stack) 0)) (do (def main_frame (nth main_stack (- (count main_stack) 1))) (def main_stack (subvec main_stack 0 (- (count main_stack) 1))) (def main_start (get main_frame "start")) (def main_lenSeg (get main_frame "len")) (def main_index (get main_frame "index")) (def main_seg (long (quot main_lenSeg 3))) (cond (= main_seg 0) (recur true) :else (do (def main_i main_index) (while (< main_i main_height) (do (def main_j (+ main_start main_seg)) (while (< main_j (+ main_start (* 2 main_seg))) (do (def main_lines (assoc main_lines main_i (setChar (nth main_lines main_i) main_j " "))) (def main_j (+ main_j 1)))) (def main_i (+ main_i 1)))) (def main_stack (conj main_stack {"start" main_start "len" main_seg "index" (+ main_index 1)})) (def main_stack (conj main_stack {"start" (+ main_start (* main_seg 2)) "len" main_seg "index" (+ main_index 1)})) (recur while_flag_1))))))
  (doseq [line main_lines] (println line)))

(-main)
