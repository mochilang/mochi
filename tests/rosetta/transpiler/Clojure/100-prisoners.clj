(ns main (:refer-clojure :exclude [shuffle doTrials main]))

(require 'clojure.set)

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare shuffle doTrials main)

(defn shuffle [xs]
  (try (do (def arr xs) (def i 99) (while (> i 0) (do (def j (mod (swap! nowSeed (fn [s] (mod (+ (* s 1664525) 1013904223) 2147483647))) (+ i 1))) (def tmp (nth arr i)) (def arr (assoc arr i (nth arr j))) (def arr (assoc arr j tmp)) (def i (- i 1)))) (throw (ex-info "return" {:v arr}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn doTrials [trials np strategy]
  (do (def pardoned 0) (def t 0) (while (< t trials) (do (def drawers []) (def i 0) (while (< i 100) (do (def drawers (conj drawers i)) (def i (+ i 1)))) (def drawers (shuffle drawers)) (def p 0) (def success true) (loop [while_flag_1 true] (when (and while_flag_1 (< p np)) (do (def found false) (if (= strategy "optimal") (do (def prev p) (def d 0) (loop [while_flag_2 true] (when (and while_flag_2 (< d 50)) (do (def this (nth drawers prev)) (cond (= this p) (do (def found true) (recur false)) :else (do (def prev this) (def d (+ d 1)) (recur while_flag_2))))))) (do (def opened []) (def k 0) (while (< k 100) (do (def opened (conj opened false)) (def k (+ k 1)))) (def d 0) (loop [while_flag_3 true] (when (and while_flag_3 (< d 50)) (do (def n (mod (swap! nowSeed (fn [s] (mod (+ (* s 1664525) 1013904223) 2147483647))) 100)) (while (nth opened n) (def n (mod (swap! nowSeed (fn [s] (mod (+ (* s 1664525) 1013904223) 2147483647))) 100))) (def opened (assoc opened n true)) (cond (= (nth drawers n) p) (do (def found true) (recur false)) :else (do (def d (+ d 1)) (recur while_flag_3)))))))) (cond (not found) (do (def success false) (recur false)) :else (do (def p (+ p 1)) (recur while_flag_1)))))) (when success (def pardoned (+ pardoned 1))) (def t (+ t 1)))) (def rf (* (/ (double pardoned) (double trials)) 100)) (println (str (str (str (str (str (str "  strategy = " strategy) "  pardoned = ") (str pardoned)) " relative frequency = ") (str rf)) "%"))))

(defn main []
  (do (def trials 1000) (doseq [np [10 100]] (do (println (str (str (str (str "Results from " (str trials)) " trials with ") (str np)) " prisoners:\n")) (doseq [strat ["random" "optimal"]] (doTrials trials np strat))))))

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
