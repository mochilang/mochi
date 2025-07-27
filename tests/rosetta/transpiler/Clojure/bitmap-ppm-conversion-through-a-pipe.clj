(ns main (:refer-clojure :exclude [pixelFromRgb rgbFromPixel NewBitmap FillRgb SetPxRgb nextRand main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare pixelFromRgb rgbFromPixel NewBitmap FillRgb SetPxRgb nextRand main)

(defn pixelFromRgb [c]
  (try (do (def r (mod (int (/ c 65536)) 256)) (def g (mod (int (/ c 256)) 256)) (def b (mod c 256)) (throw (ex-info "return" {:v {:R r :G g :B b}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn rgbFromPixel [p]
  (try (throw (ex-info "return" {:v (+ (+ (* (:R p) 65536) (* (:G p) 256)) (:B p))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn NewBitmap [x y]
  (try (do (def data []) (def row 0) (while (< row y) (do (def r []) (def col 0) (while (< col x) (do (def r (conj r {:R 0 :G 0 :B 0})) (def col (+ col 1)))) (def data (conj data r)) (def row (+ row 1)))) (throw (ex-info "return" {:v {:cols x :rows y :px data}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn FillRgb [b c]
  (do (def y 0) (def p (pixelFromRgb c)) (while (< y (:rows b)) (do (def x 0) (while (< x (:cols b)) (do (def px (:px b)) (def row (nth px y)) (def row (assoc row x p)) (def px (assoc px y row)) (def b (assoc b :px px)) (def x (+ x 1)))) (def y (+ y 1))))))

(defn SetPxRgb [b x y c]
  (try (do (when (or (or (or (< x 0) (>= x (:cols b))) (< y 0)) (>= y (:rows b))) (throw (ex-info "return" {:v false}))) (def px (:px b)) (def row (nth px y)) (def row (assoc row x (pixelFromRgb c))) (def px (assoc px y row)) (def b (assoc b :px px)) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn nextRand [seed]
  (try (throw (ex-info "return" {:v (mod (+ (* seed 1664525) 1013904223) 2147483648)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (do (def bm (NewBitmap 400 300)) (FillRgb bm 12615744) (def seed (swap! nowSeed (fn [s] (mod (+ (* s 1664525) 1013904223) 2147483647)))) (def i 0) (while (< i 2000) (do (def seed (nextRand seed)) (def x (mod seed 400)) (def seed (nextRand seed)) (def y (mod seed 300)) (SetPxRgb bm x y 8405024) (def i (+ i 1)))) (def x 0) (while (< x 400) (do (def y 240) (while (< y 245) (do (SetPxRgb bm x y 8405024) (def y (+ y 1)))) (def y 260) (while (< y 265) (do (SetPxRgb bm x y 8405024) (def y (+ y 1)))) (def x (+ x 1)))) (def y 0) (while (< y 300) (do (def x 80) (while (< x 85) (do (SetPxRgb bm x y 8405024) (def x (+ x 1)))) (def x 95) (while (< x 100) (do (SetPxRgb bm x y 8405024) (def x (+ x 1)))) (def y (+ y 1))))))

(defn -main []
  (main))

(-main)
