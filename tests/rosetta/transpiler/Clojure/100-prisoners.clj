(ns main (:refer-clojure :exclude [shuffle doTrials main]))

(require 'clojure.set)

(defn shuffle [xs]
  (def arr xs)
  (def i 99)
  (while (> i 0) (do (def j (mod (System/currentTimeMillis) (+ i 1))) (def tmp (nth arr i)) (def arr (assoc arr i (nth arr j))) (def arr (assoc arr j tmp)) (def i (- i 1))))
  arr)

(defn doTrials [trials np strategy]
  (def pardoned 0)
  (def t 0)
  (while (< t trials) (do (def drawers []) (def i 0) (while (< i 100) (do (def drawers (conj drawers i)) (def i (+ i 1)))) (def drawers (shuffle drawers)) (def p 0) (def success true) (loop [while_flag_1 true] (when (and while_flag_1 (< p np)) (do (def found false) (if (= strategy "optimal") (do (def prev p) (def d 0) (loop [while_flag_2 true] (when (and while_flag_2 (< d 50)) (do (def this (nth drawers prev)) (cond (= this p) (do (def found true) (recur false)) :else (do (def prev this) (def d (+ d 1)) (recur while_flag_2))))))) (do (def opened []) (def k 0) (while (< k 100) (do (def opened (conj opened false)) (def k (+ k 1)))) (def d 0) (loop [while_flag_3 true] (when (and while_flag_3 (< d 50)) (do (def n (mod (System/currentTimeMillis) 100)) (while (nth opened n) (def n (mod (System/currentTimeMillis) 100))) (def opened (assoc opened n true)) (cond (= (nth drawers n) p) (do (def found true) (recur false)) :else (do (def d (+ d 1)) (recur while_flag_3)))))))) (cond (not found) (do (def success false) (recur false)) :else (do (def p (+ p 1)) (recur while_flag_1)))))) (when success (def pardoned (+ pardoned 1))) (def t (+ t 1))))
  (def rf (* (/ (double pardoned) (double trials)) 100))
  (println (str (str (str (str (str (str "  strategy = " strategy) "  pardoned = ") (str pardoned)) " relative frequency = ") (str rf)) "%")))

(defn main []
  (def trials 1000)
  (doseq [np [10 100]] (do (println (str (str (str (str "Results from " (str trials)) " trials with ") (str np)) " prisoners:\n")) (doseq [strat ["random" "optimal"]] (doTrials trials np strat)))))

(defn -main []
  (main))

(-main)
