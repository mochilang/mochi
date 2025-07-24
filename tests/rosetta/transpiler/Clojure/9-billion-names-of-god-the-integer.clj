(ns main (:refer-clojure :exclude [minInt cumu row]))

(require 'clojure.set)

(defn bigTrim [a]
  a)

(defn bigFromInt [x]
  (bigint x))

(defn bigAdd [a b]
  (+ a b))

(defn bigSub [a b]
  (- a b))

(defn bigToString [a]
  (str a))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare minInt cumu row)

(defn minInt [a b]
  (if (< a b) (throw (ex-info "return" {:v a})) (throw (ex-info "return" {:v b}))))

(defn cumu [n]
  (try (do (def cache [[(bigFromInt 1)]]) (def y 1) (while (<= y n) (do (def row_v [(bigFromInt 0)]) (def x 1) (while (<= x y) (do (def val (nth (nth cache (- y x)) (minInt x (- y x)))) (def row_v (conj row_v (bigAdd (nth row_v (- (count row_v) 1)) val))) (def x (+ x 1)))) (def cache (conj cache row_v)) (def y (+ y 1)))) (throw (ex-info "return" {:v (nth cache n)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn row [n]
  (try (do (def e (cumu n)) (def out []) (def i 0) (while (< i n) (do (def diff (bigSub (nth e (+ i 1)) (nth e i))) (def out (conj out (bigToString diff))) (def i (+ i 1)))) (throw (ex-info "return" {:v out}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(def x 1)

(defn -main []
  (println "rows:")
  (while (< x 11) (do (def r (row x)) (def line "") (def i 0) (while (< i (count r)) (do (def line (str (str (str line " ") (nth r i)) " ")) (def i (+ i 1)))) (println line) (def x (+ x 1))))
  (println "")
  (println "sums:")
  (doseq [num [23 123 1234]] (do (def r (cumu num)) (println (str (str (str num) " ") (bigToString (nth r (- (count r) 1))))))))

(-main)
