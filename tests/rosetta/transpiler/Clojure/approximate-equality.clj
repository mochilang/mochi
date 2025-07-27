(ns main (:refer-clojure :exclude [abs maxf isClose sqrtApprox main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare abs maxf isClose sqrtApprox main)

(defn abs [x]
  (try (if (< x 0) (- x) x) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn maxf [a b]
  (try (if (> a b) a b) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn isClose [a b]
  (try (do (def relTol 0.000000001) (def t (abs (- a b))) (def u (* relTol (maxf (abs a) (abs b)))) (throw (ex-info "return" {:v (<= t u)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn sqrtApprox [x]
  (try (do (def guess x) (def i 0) (while (< i 10) (do (def guess (/ (+ guess (/ x guess)) 2)) (def i (+ i 1)))) (throw (ex-info "return" {:v guess}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (do (def root2 (sqrtApprox 2)) (def pairs [[100000000000000.02 100000000000000.02] [100.01 100.011] [(/ 10000000000000.002 10000) 1000000000.0000001] [0.001 0.0010000001] [0.000000000000000000000101 0] [(* root2 root2) 2] [(* (- root2) root2) (- 2)] [100000000000000000 100000000000000000] [3.141592653589793 3.141592653589793]]) (doseq [pair pairs] (do (def a (nth pair 0)) (def b (nth pair 1)) (def s (if (isClose a b) "≈" "≉")) (println (str (str (str (str (str a) " ") s) " ") (str b)))))))

(defn -main []
  (main))

(-main)
