(ns main (:refer-clojure :exclude [pixelFromRgb newBitmap setPx fill fillRgb line bezier2]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare pixelFromRgb newBitmap setPx fill fillRgb line bezier2)

(defn pixelFromRgb [rgb]
  (try (do (def r (int (mod (/ rgb 65536) 256))) (def g (int (mod (/ rgb 256) 256))) (def b (int (mod rgb 256))) (throw (ex-info "return" {:v {:r r :g g :b b}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn newBitmap [cols rows]
  (try (do (def d []) (def y 0) (while (< y rows) (do (def row []) (def x 0) (while (< x cols) (do (def row (conj row {:r 0 :g 0 :b 0})) (def x (+ x 1)))) (def d (conj d row)) (def y (+ y 1)))) (throw (ex-info "return" {:v {"cols" cols "rows" rows "data" d}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn setPx [b x y p]
  (do (def cols (int (get b "cols"))) (def rows (int (get b "rows"))) (when (and (and (and (>= x 0) (< x cols)) (>= y 0)) (< y rows)) (def b (assoc-in b ["data" y x] p)))))

(defn fill [b p]
  (do (def cols (int (get b "cols"))) (def rows (int (get b "rows"))) (def y 0) (while (< y rows) (do (def x 0) (while (< x cols) (do (def b (assoc-in b ["data" y x] p)) (def x (+ x 1)))) (def y (+ y 1))))))

(defn fillRgb [b rgb]
  (fill b (pixelFromRgb rgb)))

(defn line [b x0 y0 x1 y1 p]
  (do (def dx (- x1 x0)) (when (< dx 0) (def dx (- dx))) (def dy (- y1 y0)) (when (< dy 0) (def dy (- dy))) (def sx (- 1)) (when (< x0 x1) (def sx 1)) (def sy (- 1)) (when (< y0 y1) (def sy 1)) (def err (- dx dy)) (loop [while_flag_1 true] (when (and while_flag_1 true) (do (setPx b x0 y0 p) (cond (and (= x0 x1) (= y0 y1)) (recur false) :else (do (def e2 (* 2 err)) (when (> e2 (- 0 dy)) (do (def err (- err dy)) (def x0 (+ x0 sx)))) (when (< e2 dx) (do (def err (+ err dx)) (def y0 (+ y0 sy)))) (recur while_flag_1))))))))

(defn bezier2 [b x1 y1 x2 y2 x3 y3 p]
  (do (def px []) (def py []) (def i 0) (while (<= i b2Seg) (do (def px (conj px 0)) (def py (conj py 0)) (def i (+ i 1)))) (def fx1 (double x1)) (def fy1 (double y1)) (def fx2 (double x2)) (def fy2 (double y2)) (def fx3 (double x3)) (def fy3 (double y3)) (def i 0) (while (<= i b2Seg) (do (def c (/ (double i) (double b2Seg))) (def a (- 1.0 c)) (def a2 (* a a)) (def b2 (* (* 2.0 c) a)) (def c2 (* c c)) (def px (assoc px i (int (+ (+ (* a2 fx1) (* b2 fx2)) (* c2 fx3))))) (def py (assoc py i (int (+ (+ (* a2 fy1) (* b2 fy2)) (* c2 fy3))))) (def i (+ i 1)))) (def x0 (nth px 0)) (def y0 (nth py 0)) (def i 1) (while (<= i b2Seg) (do (def x (nth px i)) (def y (nth py i)) (line b x0 y0 x y p) (def x0 x) (def y0 y) (def i (+ i 1))))))

(defn -main []
  (def b2Seg 20)
  (def b (newBitmap 400 300))
  (fillRgb b 14614575)
  (bezier2 b 20 150 500 (- 100) 300 280 (pixelFromRgb 4165615)))

(-main)
