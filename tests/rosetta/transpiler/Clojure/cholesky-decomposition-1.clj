(ns main (:refer-clojure :exclude [sqrtApprox makeSym unpackSym printMat printSym printLower choleskyLower demo]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare sqrtApprox makeSym unpackSym printMat printSym printLower choleskyLower demo)

(declare choleskyLower_ae choleskyLower_ci choleskyLower_col choleskyLower_cx choleskyLower_d choleskyLower_dc choleskyLower_dr choleskyLower_e choleskyLower_i choleskyLower_idx choleskyLower_j choleskyLower_le choleskyLower_n choleskyLower_row demo_l printLower_c printLower_ele printLower_idx printLower_mat printLower_n printLower_r printLower_row printMat_i printMat_j printMat_line sqrtApprox_guess sqrtApprox_i unpackSym_c unpackSym_ele unpackSym_idx unpackSym_mat unpackSym_n unpackSym_r unpackSym_row)

(defn sqrtApprox [sqrtApprox_x]
  (try (do (def sqrtApprox_guess sqrtApprox_x) (def sqrtApprox_i 0) (while (< sqrtApprox_i 20) (do (def sqrtApprox_guess (/ (+ sqrtApprox_guess (/ sqrtApprox_x sqrtApprox_guess)) 2.0)) (def sqrtApprox_i (+ sqrtApprox_i 1)))) (throw (ex-info "return" {:v sqrtApprox_guess}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn makeSym [makeSym_order makeSym_elements]
  (try (throw (ex-info "return" {:v {"order" makeSym_order "ele" makeSym_elements}})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn unpackSym [unpackSym_m]
  (try (do (def unpackSym_n (get unpackSym_m "order")) (def unpackSym_ele (get unpackSym_m "ele")) (def unpackSym_mat []) (def unpackSym_idx 0) (def unpackSym_r 0) (while (< unpackSym_r unpackSym_n) (do (def unpackSym_row []) (def unpackSym_c 0) (while (<= unpackSym_c unpackSym_r) (do (def unpackSym_row (conj unpackSym_row (nth unpackSym_ele unpackSym_idx))) (def unpackSym_idx (+ unpackSym_idx 1)) (def unpackSym_c (+ unpackSym_c 1)))) (while (< unpackSym_c unpackSym_n) (do (def unpackSym_row (conj unpackSym_row 0.0)) (def unpackSym_c (+ unpackSym_c 1)))) (def unpackSym_mat (conj unpackSym_mat unpackSym_row)) (def unpackSym_r (+ unpackSym_r 1)))) (def unpackSym_r 0) (while (< unpackSym_r unpackSym_n) (do (def unpackSym_c (+ unpackSym_r 1)) (while (< unpackSym_c unpackSym_n) (do (def unpackSym_mat (assoc-in unpackSym_mat [unpackSym_r unpackSym_c] (nth (nth unpackSym_mat unpackSym_c) unpackSym_r))) (def unpackSym_c (+ unpackSym_c 1)))) (def unpackSym_r (+ unpackSym_r 1)))) (throw (ex-info "return" {:v unpackSym_mat}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn printMat [printMat_m]
  (do (def printMat_i 0) (while (< printMat_i (count printMat_m)) (do (def printMat_line "") (def printMat_j 0) (while (< printMat_j (count (nth printMat_m printMat_i))) (do (def printMat_line (str printMat_line (str (nth (nth printMat_m printMat_i) printMat_j)))) (when (< printMat_j (- (count (nth printMat_m printMat_i)) 1)) (def printMat_line (str printMat_line " "))) (def printMat_j (+ printMat_j 1)))) (println printMat_line) (def printMat_i (+ printMat_i 1))))))

(defn printSym [printSym_m]
  (printMat (unpackSym printSym_m)))

(defn printLower [printLower_m]
  (do (def printLower_n (get printLower_m "order")) (def printLower_ele (get printLower_m "ele")) (def printLower_mat []) (def printLower_idx 0) (def printLower_r 0) (while (< printLower_r printLower_n) (do (def printLower_row []) (def printLower_c 0) (while (<= printLower_c printLower_r) (do (def printLower_row (conj printLower_row (nth printLower_ele printLower_idx))) (def printLower_idx (+ printLower_idx 1)) (def printLower_c (+ printLower_c 1)))) (while (< printLower_c printLower_n) (do (def printLower_row (conj printLower_row 0.0)) (def printLower_c (+ printLower_c 1)))) (def printLower_mat (conj printLower_mat printLower_row)) (def printLower_r (+ printLower_r 1)))) (printMat printLower_mat)))

(defn choleskyLower [choleskyLower_a]
  (try (do (def choleskyLower_n (get choleskyLower_a "order")) (def choleskyLower_ae (get choleskyLower_a "ele")) (def choleskyLower_le []) (def choleskyLower_idx 0) (while (< choleskyLower_idx (count choleskyLower_ae)) (do (def choleskyLower_le (conj choleskyLower_le 0.0)) (def choleskyLower_idx (+ choleskyLower_idx 1)))) (def choleskyLower_row 1) (def choleskyLower_col 1) (def choleskyLower_dr 0) (def choleskyLower_dc 0) (def choleskyLower_i 0) (while (< choleskyLower_i (count choleskyLower_ae)) (do (def choleskyLower_e (nth choleskyLower_ae choleskyLower_i)) (if (< choleskyLower_i choleskyLower_dr) (do (def choleskyLower_d (/ (- choleskyLower_e (nth choleskyLower_le choleskyLower_i)) (nth choleskyLower_le choleskyLower_dc))) (def choleskyLower_le (assoc choleskyLower_le choleskyLower_i choleskyLower_d)) (def choleskyLower_ci choleskyLower_col) (def choleskyLower_cx choleskyLower_dc) (def choleskyLower_j (+ choleskyLower_i 1)) (while (<= choleskyLower_j choleskyLower_dr) (do (def choleskyLower_cx (+ choleskyLower_cx choleskyLower_ci)) (def choleskyLower_ci (+ choleskyLower_ci 1)) (def choleskyLower_le (assoc choleskyLower_le choleskyLower_j (+ (nth choleskyLower_le choleskyLower_j) (* choleskyLower_d (nth choleskyLower_le choleskyLower_cx))))) (def choleskyLower_j (+ choleskyLower_j 1)))) (def choleskyLower_col (+ choleskyLower_col 1)) (def choleskyLower_dc (+ choleskyLower_dc choleskyLower_col))) (do (def choleskyLower_le (assoc choleskyLower_le choleskyLower_i (sqrtApprox (- choleskyLower_e (nth choleskyLower_le choleskyLower_i))))) (def choleskyLower_row (+ choleskyLower_row 1)) (def choleskyLower_dr (+ choleskyLower_dr choleskyLower_row)) (def choleskyLower_col 1) (def choleskyLower_dc 0))) (def choleskyLower_i (+ choleskyLower_i 1)))) (throw (ex-info "return" {:v {"order" choleskyLower_n "ele" choleskyLower_le}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn demo [demo_a]
  (do (println "A:") (printSym demo_a) (println "L:") (def demo_l (choleskyLower demo_a)) (printLower demo_l)))

(defn -main []
  (demo (makeSym 3 [25.0 15.0 18.0 (- 5.0) 0.0 11.0]))
  (demo (makeSym 4 [18.0 22.0 70.0 54.0 86.0 174.0 42.0 62.0 134.0 106.0])))

(-main)
