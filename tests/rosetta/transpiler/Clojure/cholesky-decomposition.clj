(ns main (:refer-clojure :exclude [sqrtApprox cholesky printMat demo]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare sqrtApprox cholesky printMat demo)

(declare cholesky_i cholesky_j cholesky_k cholesky_l cholesky_n cholesky_row cholesky_sum demo_l printMat_i printMat_j printMat_line sqrtApprox_guess sqrtApprox_i)

(defn sqrtApprox [sqrtApprox_x]
  (try (do (def sqrtApprox_guess sqrtApprox_x) (def sqrtApprox_i 0) (while (< sqrtApprox_i 20) (do (def sqrtApprox_guess (/ (+ sqrtApprox_guess (/ sqrtApprox_x sqrtApprox_guess)) 2.0)) (def sqrtApprox_i (+ sqrtApprox_i 1)))) (throw (ex-info "return" {:v sqrtApprox_guess}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn cholesky [cholesky_a]
  (try (do (def cholesky_n (count cholesky_a)) (def cholesky_l []) (def cholesky_i 0) (while (< cholesky_i cholesky_n) (do (def cholesky_row []) (def cholesky_j 0) (while (< cholesky_j cholesky_n) (do (def cholesky_row (conj cholesky_row 0.0)) (def cholesky_j (+ cholesky_j 1)))) (def cholesky_l (conj cholesky_l cholesky_row)) (def cholesky_i (+ cholesky_i 1)))) (def cholesky_i 0) (while (< cholesky_i cholesky_n) (do (def cholesky_j 0) (while (<= cholesky_j cholesky_i) (do (def cholesky_sum (nth (nth cholesky_a cholesky_i) cholesky_j)) (def cholesky_k 0) (while (< cholesky_k cholesky_j) (do (def cholesky_sum (- cholesky_sum (* (nth (nth cholesky_l cholesky_i) cholesky_k) (nth (nth cholesky_l cholesky_j) cholesky_k)))) (def cholesky_k (+ cholesky_k 1)))) (if (= cholesky_i cholesky_j) (def cholesky_l (assoc-in cholesky_l [cholesky_i cholesky_j] (sqrtApprox cholesky_sum))) (def cholesky_l (assoc-in cholesky_l [cholesky_i cholesky_j] (/ cholesky_sum (nth (nth cholesky_l cholesky_j) cholesky_j))))) (def cholesky_j (+ cholesky_j 1)))) (def cholesky_i (+ cholesky_i 1)))) (throw (ex-info "return" {:v cholesky_l}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn printMat [printMat_m]
  (do (def printMat_i 0) (while (< printMat_i (count printMat_m)) (do (def printMat_line "") (def printMat_j 0) (while (< printMat_j (count (nth printMat_m printMat_i))) (do (def printMat_line (str printMat_line (str (nth (nth printMat_m printMat_i) printMat_j)))) (when (< printMat_j (- (count (nth printMat_m printMat_i)) 1)) (def printMat_line (str printMat_line " "))) (def printMat_j (+ printMat_j 1)))) (println printMat_line) (def printMat_i (+ printMat_i 1))))))

(defn demo [demo_a]
  (do (println "A:") (printMat demo_a) (def demo_l (cholesky demo_a)) (println "L:") (printMat demo_l)))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (demo [[25.0 15.0 (- 5.0)] [15.0 18.0 0.0] [(- 5.0) 0.0 11.0]])
      (demo [[18.0 22.0 54.0 42.0] [22.0 70.0 86.0 62.0] [54.0 86.0 174.0 134.0] [42.0 62.0 134.0 106.0]])
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
