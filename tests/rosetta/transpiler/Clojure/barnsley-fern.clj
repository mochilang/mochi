(ns main (:refer-clojure :exclude [randInt]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare randInt)

(defn randInt [s n]
  (try (do (def next (mod (+ (* s 1664525) 1013904223) 2147483647)) (throw (ex-info "return" {:v [next (mod next n)]}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn -main []
  (def xMin (- 2.182))
  (def xMax 2.6558)
  (def yMin 0.0)
  (def yMax 9.9983)
  (def width 60)
  (def nIter 10000)
  (def dx (- xMax xMin))
  (def dy (- yMax yMin))
  (def height (int (/ (* width dy) dx)))
  (def grid [])
  (def row 0)
  (while (< row height) (do (def line []) (def col 0) (while (< col width) (do (def line (conj line " ")) (def col (+ col 1)))) (def grid (conj grid line)) (def row (+ row 1))))
  (def seed 1)
  (def x 0.0)
  (def y 0.0)
  (def ix (int (/ (* (double width) (- x xMin)) dx)))
  (def iy (int (/ (* (double height) (- yMax y)) dy)))
  (when (and (and (and (>= ix 0) (< ix width)) (>= iy 0)) (< iy height)) (def grid (assoc-in grid [iy ix] "*")))
  (def i 0)
  (while (< i nIter) (do (def res (randInt seed 100)) (def seed (nth res 0)) (def r (nth res 1)) (if (< r 85) (do (def nx (+ (* 0.85 x) (* 0.04 y))) (def ny (+ (+ (* (- 0.04) x) (* 0.85 y)) 1.6)) (def x nx) (def y ny)) (if (< r 92) (do (def nx (- (* 0.2 x) (* 0.26 y))) (def ny (+ (+ (* 0.23 x) (* 0.22 y)) 1.6)) (def x nx) (def y ny)) (if (< r 99) (do (def nx (+ (* (- 0.15) x) (* 0.28 y))) (def ny (+ (+ (* 0.26 x) (* 0.24 y)) 0.44)) (def x nx) (def y ny)) (do (def x 0.0) (def y (* 0.16 y)))))) (def ix (int (/ (* (double width) (- x xMin)) dx))) (def iy (int (/ (* (double height) (- yMax y)) dy))) (when (and (and (and (>= ix 0) (< ix width)) (>= iy 0)) (< iy height)) (def grid (assoc-in grid [iy ix] "*"))) (def i (+ i 1))))
  (def row 0)
  (while (< row height) (do (def line "") (def col 0) (while (< col width) (do (def line (+ line (nth (nth grid row) col))) (def col (+ col 1)))) (println line) (def row (+ row 1)))))

(-main)
