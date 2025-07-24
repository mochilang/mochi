(ns main (:refer-clojure :exclude [bigTrim bigFromInt bigAdd bigSub bigToString minInt cumu row]))

(require 'clojure.set)

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare bigTrim bigFromInt bigAdd bigSub bigToString minInt cumu row)

(defn bigTrim [a]
  (try (do (def n (count a)) (while (and (> n 1) (= (nth a (- n 1)) 0)) (do (def a (subvec a 0 (- n 1))) (def n (- n 1)))) (throw (ex-info "return" {:v a}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn bigFromInt [x]
  (try (do (when (= x 0) (throw (ex-info "return" {:v [0]}))) (def digits []) (def n x) (while (> n 0) (do (def digits (conj digits (mod n 10))) (def n (/ n 10)))) (throw (ex-info "return" {:v digits}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn bigAdd [a b]
  (try (do (def res []) (def carry 0) (def i 0) (while (or (or (< i (count a)) (< i (count b))) (> carry 0)) (do (def av 0) (when (< i (count a)) (def av (nth a i))) (def bv 0) (when (< i (count b)) (def bv (nth b i))) (def s (+ (+ av bv) carry)) (def res (conj res (mod s 10))) (def carry (/ s 10)) (def i (+ i 1)))) (throw (ex-info "return" {:v (bigTrim res)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn bigSub [a b]
  (try (do (def res []) (def borrow 0) (def i 0) (while (< i (count a)) (do (def av (nth a i)) (def bv 0) (when (< i (count b)) (def bv (nth b i))) (def diff (- (- av bv) borrow)) (if (< diff 0) (do (def diff (+ diff 10)) (def borrow 1)) (def borrow 0)) (def res (conj res diff)) (def i (+ i 1)))) (throw (ex-info "return" {:v (bigTrim res)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn bigToString [a]
  (try (do (def s "") (def i (- (count a) 1)) (while (>= i 0) (do (def s (str s (str (nth a i)))) (def i (- i 1)))) (throw (ex-info "return" {:v s}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

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
