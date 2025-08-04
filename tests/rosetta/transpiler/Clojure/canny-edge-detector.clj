(ns main (:refer-clojure :exclude [conv2d gradient threshold printMatrix main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare conv2d gradient threshold printMatrix main)

(declare conv2d_h conv2d_half conv2d_i conv2d_j conv2d_n conv2d_out conv2d_row conv2d_sum conv2d_w conv2d_x conv2d_xx conv2d_y conv2d_yy gradient_g gradient_gx gradient_gy gradient_h gradient_hx gradient_hy gradient_out gradient_row gradient_w gradient_x gradient_y main_PI main_edges main_g main_img printMatrix_line printMatrix_x printMatrix_y threshold_h threshold_out threshold_row threshold_w threshold_x threshold_y)

(def main_PI 3.141592653589793)

(defn conv2d [conv2d_img conv2d_k]
  (try (do (def conv2d_h (count conv2d_img)) (def conv2d_w (count (nth conv2d_img 0))) (def conv2d_n (count conv2d_k)) (def conv2d_half (quot conv2d_n 2)) (def conv2d_out []) (def conv2d_y 0) (while (< conv2d_y conv2d_h) (do (def conv2d_row []) (def conv2d_x 0) (while (< conv2d_x conv2d_w) (do (def conv2d_sum 0.0) (def conv2d_j 0) (while (< conv2d_j conv2d_n) (do (def conv2d_i 0) (while (< conv2d_i conv2d_n) (do (def conv2d_yy (- (+ conv2d_y conv2d_j) conv2d_half)) (when (< conv2d_yy 0) (def conv2d_yy 0)) (when (>= conv2d_yy conv2d_h) (def conv2d_yy (- conv2d_h 1))) (def conv2d_xx (- (+ conv2d_x conv2d_i) conv2d_half)) (when (< conv2d_xx 0) (def conv2d_xx 0)) (when (>= conv2d_xx conv2d_w) (def conv2d_xx (- conv2d_w 1))) (def conv2d_sum (+ conv2d_sum (* (nth (nth conv2d_img conv2d_yy) conv2d_xx) (nth (nth conv2d_k conv2d_j) conv2d_i)))) (def conv2d_i (+ conv2d_i 1)))) (def conv2d_j (+ conv2d_j 1)))) (def conv2d_row (conj conv2d_row conv2d_sum)) (def conv2d_x (+ conv2d_x 1)))) (def conv2d_out (conj conv2d_out conv2d_row)) (def conv2d_y (+ conv2d_y 1)))) (throw (ex-info "return" {:v conv2d_out}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn gradient [gradient_img]
  (try (do (def gradient_hx [[(- 1.0) 0.0 1.0] [(- 2.0) 0.0 2.0] [(- 1.0) 0.0 1.0]]) (def gradient_hy [[1.0 2.0 1.0] [0.0 0.0 0.0] [(- 1.0) (- 2.0) (- 1.0)]]) (def gradient_gx (conv2d gradient_img gradient_hx)) (def gradient_gy (conv2d gradient_img gradient_hy)) (def gradient_h (count gradient_img)) (def gradient_w (count (nth gradient_img 0))) (def gradient_out []) (def gradient_y 0) (while (< gradient_y gradient_h) (do (def gradient_row []) (def gradient_x 0) (while (< gradient_x gradient_w) (do (def gradient_g (+ (* (nth (nth gradient_gx gradient_y) gradient_x) (nth (nth gradient_gx gradient_y) gradient_x)) (* (nth (nth gradient_gy gradient_y) gradient_x) (nth (nth gradient_gy gradient_y) gradient_x)))) (def gradient_row (conj gradient_row gradient_g)) (def gradient_x (+ gradient_x 1)))) (def gradient_out (conj gradient_out gradient_row)) (def gradient_y (+ gradient_y 1)))) (throw (ex-info "return" {:v gradient_out}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn threshold [threshold_g threshold_t]
  (try (do (def threshold_h (count threshold_g)) (def threshold_w (count (nth threshold_g 0))) (def threshold_out []) (def threshold_y 0) (while (< threshold_y threshold_h) (do (def threshold_row []) (def threshold_x 0) (while (< threshold_x threshold_w) (do (if (>= (nth (nth threshold_g threshold_y) threshold_x) threshold_t) (def threshold_row (conj threshold_row 1)) (def threshold_row (conj threshold_row 0))) (def threshold_x (+ threshold_x 1)))) (def threshold_out (conj threshold_out threshold_row)) (def threshold_y (+ threshold_y 1)))) (throw (ex-info "return" {:v threshold_out}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn printMatrix [printMatrix_m]
  (do (def printMatrix_y 0) (while (< printMatrix_y (count printMatrix_m)) (do (def printMatrix_line "") (def printMatrix_x 0) (while (< printMatrix_x (count (nth printMatrix_m 0))) (do (def printMatrix_line (str printMatrix_line (str (nth (nth printMatrix_m printMatrix_y) printMatrix_x)))) (when (< printMatrix_x (- (count (nth printMatrix_m 0)) 1)) (def printMatrix_line (str printMatrix_line " "))) (def printMatrix_x (+ printMatrix_x 1)))) (println printMatrix_line) (def printMatrix_y (+ printMatrix_y 1))))))

(defn main []
  (do (def main_img [[0.0 0.0 0.0 0.0 0.0] [0.0 255.0 255.0 255.0 0.0] [0.0 255.0 255.0 255.0 0.0] [0.0 255.0 255.0 255.0 0.0] [0.0 0.0 0.0 0.0 0.0]]) (def main_g (gradient main_img)) (def main_edges (threshold main_g (* 1020.0 1020.0))) (printMatrix main_edges)))

(defn -main []
  (main))

(-main)
