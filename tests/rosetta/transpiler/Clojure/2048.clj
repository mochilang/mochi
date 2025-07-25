(ns main (:refer-clojure :exclude [newBoard spawnTile pad draw reverseRow slideLeft moveLeft moveRight getCol setCol moveUp moveDown hasMoves has2048]))

(require 'clojure.set)

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare newBoard spawnTile pad draw reverseRow slideLeft moveLeft moveRight getCol setCol moveUp moveDown hasMoves has2048)

(def SIZE 4)

(defn newBoard []
  (try (do (def b []) (def y 0) (while (< y SIZE) (do (def row []) (def x 0) (while (< x SIZE) (do (def row (conj row 0)) (def x (+ x 1)))) (def b (conj b row)) (def y (+ y 1)))) (throw (ex-info "return" {:v {:cells b}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn spawnTile [b]
  (try (do (def grid (:cells b)) (def empty []) (def y 0) (while (< y SIZE) (do (def x 0) (while (< x SIZE) (do (when (= (nth (nth grid y) x) 0) (def empty (conj empty [x y]))) (def x (+ x 1)))) (def y (+ y 1)))) (when (= (count empty) 0) (throw (ex-info "return" {:v {:board b :full true}}))) (def idx (mod (swap! nowSeed (fn [s] (mod (+ (* s 1664525) 1013904223) 2147483647))) (count empty))) (def cell (nth empty idx)) (def val 4) (when (< (mod (swap! nowSeed (fn [s] (mod (+ (* s 1664525) 1013904223) 2147483647))) 10) 9) (def val 2)) (def grid (assoc-in grid [(nth cell 1) (nth cell 0)] val)) (throw (ex-info "return" {:v {:board {:cells grid} :full (= (count empty) 1)}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn pad [n]
  (try (do (def s (str n)) (def pad_v (- 4 (count s))) (def i 0) (def out "") (while (< i pad_v) (do (def out (str out " ")) (def i (+ i 1)))) (throw (ex-info "return" {:v (str out s)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn draw [b score]
  (do (println (str "Score: " (str score))) (def y 0) (while (< y SIZE) (do (println "+----+----+----+----+") (def line "|") (def x 0) (while (< x SIZE) (do (def v (nth (nth (:cells b) y) x)) (if (= v 0) (def line (str line "    |")) (def line (str (str line (pad v)) "|"))) (def x (+ x 1)))) (println line) (def y (+ y 1)))) (println "+----+----+----+----+") (println "W=Up S=Down A=Left D=Right Q=Quit")))

(defn reverseRow [r]
  (try (do (def out []) (def i (- (count r) 1)) (while (>= i 0) (do (def out (conj out (nth r i))) (def i (- i 1)))) (throw (ex-info "return" {:v out}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn slideLeft [row]
  (try (do (def xs []) (def i 0) (while (< i (count row)) (do (when (not= (nth row i) 0) (def xs (conj xs (nth row i)))) (def i (+ i 1)))) (def res []) (def gain 0) (def i 0) (while (< i (count xs)) (if (and (< (+ i 1) (count xs)) (= (nth xs i) (nth xs (+ i 1)))) (do (def v (* (nth xs i) 2)) (def gain (+ gain v)) (def res (conj res v)) (def i (+ i 2))) (do (def res (conj res (nth xs i))) (def i (+ i 1))))) (while (< (count res) SIZE) (def res (conj res 0))) (throw (ex-info "return" {:v {:row res :gain gain}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn moveLeft [b score]
  (try (do (def grid (:cells b)) (def moved false) (def y 0) (while (< y SIZE) (do (def r (slideLeft (nth grid y))) (def new (:row r)) (def score (+ score (:gain r))) (def x 0) (while (< x SIZE) (do (when (not= (nth (nth grid y) x) (nth new x)) (def moved true)) (def grid (assoc-in grid [y x] (nth new x))) (def x (+ x 1)))) (def y (+ y 1)))) (throw (ex-info "return" {:v {:board {:cells grid} :score score :moved moved}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn moveRight [b score]
  (try (do (def grid (:cells b)) (def moved false) (def y 0) (while (< y SIZE) (do (def rev (reverseRow (nth grid y))) (def r (slideLeft rev)) (def rev (:row r)) (def score (+ score (:gain r))) (def rev (reverseRow rev)) (def x 0) (while (< x SIZE) (do (when (not= (nth (nth grid y) x) (nth rev x)) (def moved true)) (def grid (assoc-in grid [y x] (nth rev x))) (def x (+ x 1)))) (def y (+ y 1)))) (throw (ex-info "return" {:v {:board {:cells grid} :score score :moved moved}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn getCol [b x]
  (try (do (def col []) (def y 0) (while (< y SIZE) (do (def col (conj col (nth (nth (:cells b) y) x))) (def y (+ y 1)))) (throw (ex-info "return" {:v col}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn setCol [b x col]
  (do (def rows (:cells b)) (def y 0) (while (< y SIZE) (do (def row (nth rows y)) (def row (assoc row x (nth col y))) (def rows (assoc rows y row)) (def y (+ y 1)))) (def b (assoc b :cells rows))))

(defn moveUp [b score]
  (try (do (def grid (:cells b)) (def moved false) (def x 0) (while (< x SIZE) (do (def col (getCol b x)) (def r (slideLeft col)) (def new (:row r)) (def score (+ score (:gain r))) (def y 0) (while (< y SIZE) (do (when (not= (nth (nth grid y) x) (nth new y)) (def moved true)) (def grid (assoc-in grid [y x] (nth new y))) (def y (+ y 1)))) (def x (+ x 1)))) (throw (ex-info "return" {:v {:board {:cells grid} :score score :moved moved}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn moveDown [b score]
  (try (do (def grid (:cells b)) (def moved false) (def x 0) (while (< x SIZE) (do (def col (reverseRow (getCol b x))) (def r (slideLeft col)) (def col (:row r)) (def score (+ score (:gain r))) (def col (reverseRow col)) (def y 0) (while (< y SIZE) (do (when (not= (nth (nth grid y) x) (nth col y)) (def moved true)) (def grid (assoc-in grid [y x] (nth col y))) (def y (+ y 1)))) (def x (+ x 1)))) (throw (ex-info "return" {:v {:board {:cells grid} :score score :moved moved}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn hasMoves [b]
  (try (do (def y 0) (while (< y SIZE) (do (def x 0) (while (< x SIZE) (do (when (= (nth (nth (:cells b) y) x) 0) (throw (ex-info "return" {:v true}))) (when (and (< (+ x 1) SIZE) (= (nth (nth (:cells b) y) x) (nth (nth (:cells b) y) (+ x 1)))) (throw (ex-info "return" {:v true}))) (when (and (< (+ y 1) SIZE) (= (nth (nth (:cells b) y) x) (nth (nth (:cells b) (+ y 1)) x))) (throw (ex-info "return" {:v true}))) (def x (+ x 1)))) (def y (+ y 1)))) (throw (ex-info "return" {:v false}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn has2048 [b]
  (try (do (def y 0) (while (< y SIZE) (do (def x 0) (while (< x SIZE) (do (when (>= (nth (nth (:cells b) y) x) 2048) (throw (ex-info "return" {:v true}))) (def x (+ x 1)))) (def y (+ y 1)))) (throw (ex-info "return" {:v false}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(def board (newBoard))

(def r (spawnTile board))

(def full (:full r))

(def score 0)

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (def board (:board r))
      (def r (spawnTile board))
      (def board (:board r))
      (def full (:full r))
      (draw board score)
      (loop [while_flag_1 true] (when (and while_flag_1 true) (do (println "Move: ") (def cmd (read-line)) (def moved false) (when (or (= cmd "a") (= cmd "A")) (do (def m (moveLeft board score)) (def board (:board m)) (def score (:score m)) (def moved (:moved m)))) (when (or (= cmd "d") (= cmd "D")) (do (def m (moveRight board score)) (def board (:board m)) (def score (:score m)) (def moved (:moved m)))) (when (or (= cmd "w") (= cmd "W")) (do (def m (moveUp board score)) (def board (:board m)) (def score (:score m)) (def moved (:moved m)))) (when (or (= cmd "s") (= cmd "S")) (do (def m (moveDown board score)) (def board (:board m)) (def score (:score m)) (def moved (:moved m)))) (cond (or (= cmd "q") (= cmd "Q")) (recur false) :else (do (when moved (do (def r2 (spawnTile board)) (def board (:board r2)) (def full (:full r2)) (when (and full (not (hasMoves board))) (do (draw board score) (println "Game Over") (recur false))))) (draw board score) (when (has2048 board) (do (println "You win!") (recur false))) (when (not (hasMoves board)) (do (println "Game Over") (recur false))) (recur while_flag_1))))))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
