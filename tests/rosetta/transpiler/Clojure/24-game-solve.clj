(ns main (:refer-clojure :exclude [binEval binString newNum exprEval exprString solve main]))

(require 'clojure.set)

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare binEval binString newNum exprEval exprString solve main)

(def OP_ADD 1)

(def OP_SUB 2)

(def OP_MUL 3)

(def OP_DIV 4)

(defn binEval [op l r]
  (do (def lv (exprEval l)) (def rv (exprEval r)) (when (= op OP_ADD) (throw (ex-info "return" {:v {:num (+ (* (:num lv) (:denom rv)) (* (:denom lv) (:num rv))) :denom (* (:denom lv) (:denom rv))}}))) (when (= op OP_SUB) (throw (ex-info "return" {:v {:num (- (* (:num lv) (:denom rv)) (* (:denom lv) (:num rv))) :denom (* (:denom lv) (:denom rv))}}))) (if (= op OP_MUL) {:num (* (:num lv) (:num rv)) :denom (* (:denom lv) (:denom rv))} {:num (* (:num lv) (:denom rv)) :denom (* (:denom lv) (:num rv))})))

(defn binString [op l r]
  (try (do (def ls (exprString l)) (def rs (exprString r)) (def opstr "") (if (= op OP_ADD) (def opstr " + ") (if (= op OP_SUB) (def opstr " - ") (if (= op OP_MUL) (def opstr " * ") (def opstr " / ")))) (throw (ex-info "return" {:v (str (str (str (str "(" ls) opstr) rs) ")")}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn newNum [n]
  (try (throw (ex-info "return" {:v {:value {:num n :denom 1}}})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn exprEval [x]
  (try (throw (ex-info "return" {:v (cond true (let [v (:value x)] v) true (let [op (:op x) l (:left x) r (:right x)] (binEval op l r)))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn exprString [x]
  (try (throw (ex-info "return" {:v (cond true (let [v (:value x)] (str (:num v))) true (let [op (:op x) l (:left x) r (:right x)] (binString op l r)))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(def n_cards 4)

(def goal 24)

(def digit_range 9)

(defn solve [xs]
  (try (do (when (= (count xs) 1) (do (def f (exprEval (nth xs 0))) (when (and (not= (:denom f) 0) (= (:num f) (* (:denom f) goal))) (do (println (exprString (nth xs 0))) (throw (ex-info "return" {:v true})))) (throw (ex-info "return" {:v false})))) (def i 0) (while (< i (count xs)) (do (def j (+ i 1)) (while (< j (count xs)) (do (def rest []) (def k 0) (while (< k (count xs)) (do (when (and (not= k i) (not= k j)) (def rest (conj rest (nth xs k)))) (def k (+ k 1)))) (def a (nth xs i)) (def b (nth xs j)) (def node {:op OP_ADD :left a :right b}) (doseq [op [OP_ADD OP_SUB OP_MUL OP_DIV]] (do (def node {:op op :left a :right b}) (when (solve (conj rest node)) (throw (ex-info "return" {:v true}))))) (def node {:op OP_SUB :left b :right a}) (when (solve (conj rest node)) (throw (ex-info "return" {:v true}))) (def node {:op OP_DIV :left b :right a}) (when (solve (conj rest node)) (throw (ex-info "return" {:v true}))) (def j (+ j 1)))) (def i (+ i 1)))) (throw (ex-info "return" {:v false}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (do (def iter 0) (while (< iter 10) (do (def cards []) (def i 0) (while (< i n_cards) (do (def n (+ (mod (swap! nowSeed (fn [s] (mod (+ (* s 1664525) 1013904223) 2147483647))) (- digit_range 1)) 1)) (def cards (conj cards (newNum n))) (println (str " " (str n))) (def i (+ i 1)))) (println ":  ") (when (not (solve cards)) (println "No solution")) (def iter (+ iter 1))))))

(defn -main []
  (main))

(-main)
