(ns main (:refer-clojure :exclude [newBoard spawnTile pad draw reverseRow slideLeft moveLeft moveRight getCol setCol moveUp moveDown hasMoves has2048]))

(require 'clojure.set)

(def SIZE 4)

(defn newBoard []
  (try (do (def b []) (def y 0) (while (< y SIZE) (do (def row []) (def x 0) (while (< x SIZE) (do (def row (conj row 0)) (def x (+ x 1)))) (def b (conj b row)) (def y (+ y 1)))) (throw (ex-info "return" {:v b}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn spawnTile [b]
  (try (do (def empty []) (def y 0) (while (< y SIZE) (do (def x 0) (while (< x SIZE) (do (when (= (nth (nth b y) x) 0) (def empty (conj empty [x y]))) (def x (+ x 1)))) (def y (+ y 1)))) (when (= (count empty) 0) (throw (ex-info "return" {:v {"board" b "full" true}}))) (def idx (mod (System/currentTimeMillis) (count empty))) (def cell (nth empty idx)) (def val 4) (when (< (mod (System/currentTimeMillis) 10) 9) (def val 2)) (def b (assoc-in b [(nth cell 1) (nth cell 0)] val)) (throw (ex-info "return" {:v {"board" b "full" (= (count empty) 1)}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn pad [n]
  (try (do (def s (str n)) (def pad (- 4 (count s))) (def i 0) (def out "") (while (< i pad) (do (def out (str out " ")) (def i (+ i 1)))) (throw (ex-info "return" {:v (+ out s)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn draw [b score]
  (do (println (str "Score: " (str score))) (def y 0) (while (< y SIZE) (do (println "+----+----+----+----+") (def line "|") (def x 0) (while (< x SIZE) (do (def v (nth (nth b y) x)) (if (= v 0) (def line (str line "    |")) (def line (str (+ line (pad v)) "|"))) (def x (+ x 1)))) (println line) (def y (+ y 1)))) (println "+----+----+----+----+") (println "W=Up S=Down A=Left D=Right Q=Quit")))

(defn reverseRow [r]
  (try (do (def out []) (def i (- (count r) 1)) (while (>= i 0) (do (def out (conj out (get r i))) (def i (- i 1)))) (throw (ex-info "return" {:v out}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn slideLeft [row]
  (try (do (def xs []) (def i 0) (while (< i (count row)) (do (when (not= (nth row i) 0) (def xs (conj xs (nth row i)))) (def i (+ i 1)))) (def res []) (def gain 0) (def i 0) (while (< i (count xs)) (if (and (< (+ i 1) (count xs)) (= (nth xs i) (nth xs (+ i 1)))) (do (def v (* (nth xs i) 2)) (def gain (+ gain v)) (def res (conj res v)) (def i (+ i 2))) (do (def res (conj res (nth xs i))) (def i (+ i 1))))) (while (< (count res) SIZE) (def res (conj res 0))) (throw (ex-info "return" {:v {"row" res "gain" gain}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn moveLeft [b score]
  (try (do (def moved false) (def y 0) (while (< y SIZE) (do (def r (slideLeft (nth b y))) (def new (get r "row")) (def score (+ score (get r "gain"))) (def x 0) (while (< x SIZE) (do (when (not= (nth (nth b y) x) (nth new x)) (def moved true)) (def b (assoc-in b [y x] (nth new x))) (def x (+ x 1)))) (def y (+ y 1)))) (throw (ex-info "return" {:v {"board" b "score" score "moved" moved}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn moveRight [b score]
  (try (do (def moved false) (def y 0) (while (< y SIZE) (do (def rev (reverseRow (nth b y))) (def r (slideLeft rev)) (def rev (get r "row")) (def score (+ score (get r "gain"))) (def rev (reverseRow rev)) (def x 0) (while (< x SIZE) (do (when (not= (nth (nth b y) x) (nth rev x)) (def moved true)) (def b (assoc-in b [y x] (nth rev x))) (def x (+ x 1)))) (def y (+ y 1)))) (throw (ex-info "return" {:v {"board" b "score" score "moved" moved}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn getCol [b x]
  (try (do (def col []) (def y 0) (while (< y SIZE) (do (def col (conj col (nth (nth b y) x))) (def y (+ y 1)))) (throw (ex-info "return" {:v col}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn setCol [b x col]
  (do (def y 0) (while (< y SIZE) (do (def b (assoc-in b [y x] (nth col y))) (def y (+ y 1))))))

(defn moveUp [b score]
  (try (do (def moved false) (def x 0) (while (< x SIZE) (do (def col (getCol b x)) (def r (slideLeft col)) (def new (get r "row")) (def score (+ score (get r "gain"))) (def y 0) (while (< y SIZE) (do (when (not= (nth (nth b y) x) (nth new y)) (def moved true)) (def b (assoc-in b [y x] (nth new y))) (def y (+ y 1)))) (def x (+ x 1)))) (throw (ex-info "return" {:v {"board" b "score" score "moved" moved}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn moveDown [b score]
  (try (do (def moved false) (def x 0) (while (< x SIZE) (do (def col (reverseRow (getCol b x))) (def r (slideLeft col)) (def col (get r "row")) (def score (+ score (get r "gain"))) (def col (reverseRow col)) (def y 0) (while (< y SIZE) (do (when (not= (nth (nth b y) x) (nth col y)) (def moved true)) (def b (assoc-in b [y x] (nth col y))) (def y (+ y 1)))) (def x (+ x 1)))) (throw (ex-info "return" {:v {"board" b "score" score "moved" moved}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn hasMoves [b]
  (try (do (def y 0) (while (< y SIZE) (do (def x 0) (while (< x SIZE) (do (when (= (nth (nth b y) x) 0) (throw (ex-info "return" {:v true}))) (when (and (< (+ x 1) SIZE) (= (nth (nth b y) x) (nth (nth b y) (+ x 1)))) (throw (ex-info "return" {:v true}))) (when (and (< (+ y 1) SIZE) (= (nth (nth b y) x) (nth (nth b (+ y 1)) x))) (throw (ex-info "return" {:v true}))) (def x (+ x 1)))) (def y (+ y 1)))) (throw (ex-info "return" {:v false}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn has2048 [b]
  (try (do (def y 0) (while (< y SIZE) (do (def x 0) (while (< x SIZE) (do (when (>= (nth (nth b y) x) 2048) (throw (ex-info "return" {:v true}))) (def x (+ x 1)))) (def y (+ y 1)))) (throw (ex-info "return" {:v false}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(def board (newBoard))

(def r (spawnTile board))

(def full (get r "full"))

(def score 0)

(defn -main []
  (def board (get r "board"))
  (def r (spawnTile board))
  (def board (get r "board"))
  (def full (get r "full"))
  (draw board score)
  (loop [while_flag_1 true] (when (and while_flag_1 true) (do (println "Move: ") (def cmd (read-line)) (def moved false) (when (or (= cmd "a") (= cmd "A")) (do (def m (moveLeft board score)) (def board (get m "board")) (def score (get m "score")) (def moved (get m "moved")))) (when (or (= cmd "d") (= cmd "D")) (do (def m (moveRight board score)) (def board (get m "board")) (def score (get m "score")) (def moved (get m "moved")))) (when (or (= cmd "w") (= cmd "W")) (do (def m (moveUp board score)) (def board (get m "board")) (def score (get m "score")) (def moved (get m "moved")))) (when (or (= cmd "s") (= cmd "S")) (do (def m (moveDown board score)) (def board (get m "board")) (def score (get m "score")) (def moved (get m "moved")))) (cond (or (= cmd "q") (= cmd "Q")) (recur false) (and moved (and full (not (hasMoves board)))) (do (def r2 (spawnTile board)) (def board (get r2 "board")) (def full (get r2 "full")) (do (draw board score) (println "Game Over") (recur false))) (has2048 board) (do (println "You win!") (recur false)) (not (hasMoves board)) (do (println "Game Over") (recur false)) :else (do (draw board score) (recur while_flag_1)))))))

(-main)
