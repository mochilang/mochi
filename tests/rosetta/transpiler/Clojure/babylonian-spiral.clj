(ns main (:refer-clojure :exclude [push step positions pad main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare push step positions pad main)

(defn push [h it]
  (try (do (def h (conj h it)) (def i (- (count h) 1)) (while (and (> i 0) (> (get (nth h (- i 1)) "s") (get (nth h i) "s"))) (do (def tmp (nth h (- i 1))) (def h (assoc h (- i 1) (nth h i))) (def h (assoc h i tmp)) (def i (- i 1)))) (throw (ex-info "return" {:v h}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn step [h nv dir]
  (try (do (while (or (= (count h) 0) (<= (* nv nv) (get (nth h 0) "s"))) (do (def h (push h {"s" (* nv nv) "a" nv "b" 0})) (def nv (+ nv 1)))) (def s (get (nth h 0) "s")) (def v []) (while (and (> (count h) 0) (= (get (nth h 0) "s") s)) (do (def it (nth h 0)) (def h (subvec h 1 (count h))) (def v (conj v [(get it "a") (get it "b")])) (when (> (get it "a") (get it "b")) (def h (push h {"s" (+ (* (get it "a") (get it "a")) (* (+ (get it "b") 1) (+ (get it "b") 1))) "a" (get it "a") "b" (+ (get it "b") 1)}))))) (def list []) (doseq [p v] (def list (conj list p))) (def temp list) (doseq [p temp] (when (not= (nth p 0) (nth p 1)) (def list (conj list [(nth p 1) (nth p 0)])))) (def temp list) (doseq [p temp] (when (not= (nth p 1) 0) (def list (conj list [(nth p 0) (- (nth p 1))])))) (def temp list) (doseq [p temp] (when (not= (nth p 0) 0) (def list (conj list [(- (nth p 0)) (nth p 1)])))) (def bestDot (- 999999999)) (def best dir) (doseq [p list] (do (def cross (- (* (nth p 0) (nth dir 1)) (* (nth p 1) (nth dir 0)))) (when (>= cross 0) (do (def dot (+ (* (nth p 0) (nth dir 0)) (* (nth p 1) (nth dir 1)))) (when (> dot bestDot) (do (def bestDot dot) (def best p))))))) (throw (ex-info "return" {:v {"d" best "heap" h "n" nv}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn positions [n]
  (try (do (def pos []) (def x 0) (def y 0) (def dir [0 1]) (def heap []) (def nv 1) (def i 0) (while (< i n) (do (def pos (conj pos [x y])) (def st (step heap nv dir)) (def dir (get st "d")) (def heap (get st "heap")) (def nv (int (get st "n"))) (def x (+ x (nth dir 0))) (def y (+ y (nth dir 1))) (def i (+ i 1)))) (throw (ex-info "return" {:v pos}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn pad [s w]
  (try (do (def r s) (while (< (count r) w) (def r (str r " "))) (throw (ex-info "return" {:v r}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (do (def pts (positions 40)) (println "The first 40 Babylonian spiral points are:") (def line "") (def i 0) (while (< i (count pts)) (do (def p (nth pts i)) (def s (pad (str (str (str (str "(" (str (nth p 0))) ", ") (str (nth p 1))) ")") 10)) (def line (str line s)) (when (= (mod (+ i 1) 10) 0) (do (println line) (def line ""))) (def i (+ i 1))))))

(defn -main []
  (main))

(-main)
