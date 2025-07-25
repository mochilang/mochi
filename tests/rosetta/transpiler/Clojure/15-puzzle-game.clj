(ns main (:refer-clojure :exclude [randMove isSolved isValidMove doMove shuffle printBoard playOneMove play main]))

(require 'clojure.set)

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare randMove isSolved isValidMove doMove shuffle printBoard playOneMove play main)

(def board [1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 0])

(def solved [1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 0])

(def empty 15)

(def moves 0)

(def quit false)

(defn randMove []
  (try (throw (ex-info "return" {:v (mod (swap! nowSeed (fn [s] (mod (+ (* s 1664525) 1013904223) 2147483647))) 4)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn isSolved []
  (try (do (def i 0) (while (< i 16) (do (when (not= (nth board i) (nth solved i)) (throw (ex-info "return" {:v false}))) (def i (+ i 1)))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn isValidMove [m]
  (do (when (= m 0) (throw (ex-info "return" {:v {:idx (- empty 4) :ok (> (/ empty 4) 0)}}))) (when (= m 1) (throw (ex-info "return" {:v {:idx (+ empty 4) :ok (< (/ empty 4) 3)}}))) (when (= m 2) (throw (ex-info "return" {:v {:idx (+ empty 1) :ok (< (mod empty 4) 3)}}))) (if (= m 3) {:idx (- empty 1) :ok (> (mod empty 4) 0)} {:idx 0 :ok false})))

(defn doMove [m]
  (try (do (def r (isValidMove m)) (when (not (:ok r)) (throw (ex-info "return" {:v false}))) (def i empty) (def j (:idx r)) (def tmp (nth board i)) (def board (assoc board i (nth board j))) (def board (assoc board j tmp)) (def empty j) (def moves (+ moves 1)) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn shuffle [n]
  (do (def i 0) (while (or (< i n) (isSolved)) (when (doMove (randMove)) (def i (+ i 1))))))

(defn printBoard []
  (do (def line "") (def i 0) (while (< i 16) (do (def val (nth board i)) (if (= val 0) (def line (str line "  .")) (do (def s (str val)) (if (< val 10) (def line (str (str line "  ") s)) (def line (str (str line " ") s))))) (when (= (mod i 4) 3) (do (println line) (def line ""))) (def i (+ i 1))))))

(defn playOneMove []
  (loop [while_flag_1 true] (when (and while_flag_1 true) (do (println (str (str "Enter move #" (str (+ moves 1))) " (U, D, L, R, or Q): ")) (def s (read-line)) (cond (= s "") (recur true) :else (do (def c (subvec s 0 1)) (def m 0) (if (or (= c "U") (= c "u")) (def m 0) (if (or (= c "D") (= c "d")) (def m 1) (if (or (= c "R") (= c "r")) (def m 2) (if (or (= c "L") (= c "l")) (def m 3) (if (or (= c "Q") (= c "q")) (do (println (str (str "Quiting after " (str moves)) " moves.")) (def quit true) (throw (ex-info "return" {:v nil}))) (do (println (str (str (str "Please enter \"U\", \"D\", \"L\", or \"R\" to move the empty cell\n" "up, down, left, or right. You can also enter \"Q\" to quit.\n") "Upper or lowercase is accepted and only the first non-blank\n") "character is important (i.e. you may enter \"up\" if you like).")) (recur true))))))) (when (not (doMove m)) (do (println "That is not a valid move at the moment.") (recur true))) (throw (ex-info "return" {:v nil}))))))))

(defn play []
  (do (println "Starting board:") (while (and (not quit) (= (isSolved) false)) (do (println "") (printBoard) (playOneMove))) (when (isSolved) (println (str (str "You solved the puzzle in " (str moves)) " moves.")))))

(defn main []
  (do (shuffle 50) (play)))

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
