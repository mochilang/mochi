(ns main (:refer-clojure :exclude [makeNode combine exprEval exprString solve main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare makeNode combine exprEval exprString solve main)

(def OP_ADD 1)

(def OP_SUB 2)

(def OP_MUL 3)

(def OP_DIV 4)

(defn makeNode [n]
  (try (throw (ex-info "return" {:v {:val {:num n :denom 1} :txt (str n)}})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn combine [op l r]
  (try (do (def res nil) (if (= op OP_ADD) (def res {:num (+ (* (:num (:val l)) (:denom (:val r))) (* (:denom (:val l)) (:num (:val r)))) :denom (* (:denom (:val l)) (:denom (:val r)))}) (if (= op OP_SUB) (def res {:num (- (* (:num (:val l)) (:denom (:val r))) (* (:denom (:val l)) (:num (:val r)))) :denom (* (:denom (:val l)) (:denom (:val r)))}) (if (= op OP_MUL) (def res {:num (* (:num (:val l)) (:num (:val r))) :denom (* (:denom (:val l)) (:denom (:val r)))}) (def res {:num (* (:num (:val l)) (:denom (:val r))) :denom (* (:denom (:val l)) (:num (:val r)))})))) (def opstr "") (if (= op OP_ADD) (def opstr " + ") (if (= op OP_SUB) (def opstr " - ") (if (= op OP_MUL) (def opstr " * ") (def opstr " / ")))) (throw (ex-info "return" {:v {:val res :txt (str (str (str (str "(" (:txt l)) opstr) (:txt r)) ")")}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn exprEval [x]
  (try (throw (ex-info "return" {:v (:val x)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn exprString [x]
  (try (throw (ex-info "return" {:v (:txt x)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(def n_cards 4)

(def goal 24)

(def digit_range 9)

(defn solve [xs]
  (try (do (when (= (count xs) 1) (do (def f (exprEval (nth xs 0))) (when (and (not= (:denom f) 0) (= (:num f) (* (:denom f) goal))) (do (println (exprString (nth xs 0))) (throw (ex-info "return" {:v true})))) (throw (ex-info "return" {:v false})))) (def i 0) (while (< i (count xs)) (do (def j (+ i 1)) (while (< j (count xs)) (do (def rest_v []) (def k 0) (while (< k (count xs)) (do (when (and (not= k i) (not= k j)) (def rest_v (conj rest_v (nth xs k)))) (def k (+ k 1)))) (def a (nth xs i)) (def b (nth xs j)) (def node nil) (doseq [op [OP_ADD OP_SUB OP_MUL OP_DIV]] (do (def node (combine op a b)) (when (solve (conj rest_v node)) (throw (ex-info "return" {:v true}))))) (def node (combine OP_SUB b a)) (when (solve (conj rest_v node)) (throw (ex-info "return" {:v true}))) (def node (combine OP_DIV b a)) (when (solve (conj rest_v node)) (throw (ex-info "return" {:v true}))) (def j (+ j 1)))) (def i (+ i 1)))) (throw (ex-info "return" {:v false}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (do (def iter 0) (while (< iter 10) (do (def cards []) (def i 0) (while (< i n_cards) (do (def n (+ (mod (swap! nowSeed (fn [s] (mod (+ (* s 1664525) 1013904223) 2147483647))) (- digit_range 1)) 1)) (def cards (conj cards (makeNode n))) (println (str " " (str n))) (def i (+ i 1)))) (println ":  ") (when (not (solve cards)) (println "No solution")) (def iter (+ iter 1))))))

(defn -main []
  (main))

(-main)
