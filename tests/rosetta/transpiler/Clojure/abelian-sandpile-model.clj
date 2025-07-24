(ns main (:refer-clojure :exclude [newPile handlePile drawPile main]))

(require 'clojure.set)

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(def dim 16)

(defn newPile [d]
  (try (do (def b []) (def y 0) (while (< y d) (do (def row []) (def x 0) (while (< x d) (do (def row (conj row 0)) (def x (+ x 1)))) (def b (conj b row)) (def y (+ y 1)))) (throw (ex-info "return" {:v b}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn handlePile [pile x y]
  (try (do (when (>= (nth (nth pile y) x) 4) (do (def pile (assoc-in pile [y x] (- (nth (nth pile y) x) 4))) (when (> y 0) (do (def pile (assoc-in pile [(- y 1) x] (+ (nth (nth pile (- y 1)) x) 1))) (when (>= (nth (nth pile (- y 1)) x) 4) (def pile (handlePile pile x (- y 1)))))) (when (> x 0) (do (def pile (assoc-in pile [y (- x 1)] (+ (nth (nth pile y) (- x 1)) 1))) (when (>= (nth (nth pile y) (- x 1)) 4) (def pile (handlePile pile (- x 1) y))))) (when (< y (- dim 1)) (do (def pile (assoc-in pile [(+ y 1) x] (+ (nth (nth pile (+ y 1)) x) 1))) (when (>= (nth (nth pile (+ y 1)) x) 4) (def pile (handlePile pile x (+ y 1)))))) (when (< x (- dim 1)) (do (def pile (assoc-in pile [y (+ x 1)] (+ (nth (nth pile y) (+ x 1)) 1))) (when (>= (nth (nth pile y) (+ x 1)) 4) (def pile (handlePile pile (+ x 1) y))))) (def pile (handlePile pile x y)))) (throw (ex-info "return" {:v pile}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn drawPile [pile d]
  (do (def chars [" " "░" "▓" "█"]) (def row 0) (while (< row d) (do (def line "") (def col 0) (while (< col d) (do (def v (nth (nth pile row) col)) (when (> v 3) (def v 3)) (def line (str line (nth chars v))) (def col (+ col 1)))) (println line) (def row (+ row 1))))))

(defn main []
  (do (def pile (newPile 16)) (def hdim 7) (def pile (assoc-in pile [hdim hdim] 16)) (def pile (handlePile pile hdim hdim)) (drawPile pile 16)))

(defn -main []
  (main))

(-main)
