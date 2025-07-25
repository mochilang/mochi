(ns main (:refer-clojure :exclude [parseIntStr main]))

(require 'clojure.set)

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare parseIntStr main)

(defn parseIntStr [str]
  (try (do (def i 0) (def neg false) (when (and (> (count str) 0) (= (subs str 0 1) "-")) (do (def neg true) (def i 1))) (def n 0) (def digits {"0" 0 "1" 1 "2" 2 "3" 3 "4" 4 "5" 5 "6" 6 "7" 7 "8" 8 "9" 9}) (while (< i (count str)) (do (def n (+ (* n 10) (nth digits (subs str i (+ i 1))))) (def i (+ i 1)))) (when neg (def n (- n))) (throw (ex-info "return" {:v n}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (do (def total 0) (def computer (= (mod (swap! nowSeed (fn [s] (mod (+ (* s 1664525) 1013904223) 2147483647))) 2) 0)) (println "Enter q to quit at any time\n") (if computer (println "The computer will choose first") (println "You will choose first")) (println "\n\nRunning total is now 0\n\n") (def round 1) (def done false) (while (not done) (do (println (str (str "ROUND " (str round)) ":\n\n")) (def i 0) (while (and (< i 2) (not done)) (do (if computer (do (def choice 0) (if (< total 18) (def choice (+ (mod (swap! nowSeed (fn [s] (mod (+ (* s 1664525) 1013904223) 2147483647))) 3) 1)) (def choice (- 21 total))) (def total (+ total choice)) (println (str "The computer chooses " (str choice))) (println (str "Running total is now " (str total))) (when (= total 21) (do (println "\nSo, commiserations, the computer has won!") (def done true)))) (do (loop [while_flag_1 true] (when (and while_flag_1 true) (do (println "Your choice 1 to 3 : ") (def line (read-line)) (cond (or (= line "q") (= line "Q")) (do (println "OK, quitting the game") (def done true) (recur false)) :else (do (def num (parseIntStr line)) (when (or (< num 1) (> num 3)) (do (if (> (+ total num) 21) (println "Too big, try again") (println "Out of range, try again")) (recur true))) (when (> (+ total num) 21) (do (println "Too big, try again") (recur true))) (def total (+ total num)) (println (str "Running total is now " (str total))) (recur false) (recur while_flag_1)))))) (when (= total 21) (do (println "\nSo, congratulations, you've won!") (def done true))))) (println "\n") (def computer (not computer)) (def i (+ i 1)))) (def round (+ round 1))))))

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
