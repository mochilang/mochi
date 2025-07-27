(ns main (:refer-clojure :exclude [absf maxf minf max3 min3 subdivideQuadSpline subdivideQuadCurve rectsOverlap testIntersect seemsToBeDuplicate findIntersects main]))

(require 'clojure.set)

(defrecord Workload [p q])

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare absf maxf minf max3 min3 subdivideQuadSpline subdivideQuadCurve rectsOverlap testIntersect seemsToBeDuplicate findIntersects main)

(defn absf [x]
  (try (if (< x 0.0) (- x) x) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn maxf [a b]
  (try (if (> a b) a b) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn minf [a b]
  (try (if (< a b) a b) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn max3 [a b c]
  (try (do (def m a) (when (> b m) (def m b)) (when (> c m) (def m c)) (throw (ex-info "return" {:v m}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn min3 [a b c]
  (try (do (def m a) (when (< b m) (def m b)) (when (< c m) (def m c)) (throw (ex-info "return" {:v m}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn subdivideQuadSpline [q t]
  (try (do (def s (- 1.0 t)) (def u {:c0 (:c0 q) :c1 0.0 :c2 0.0}) (def v {:c0 0.0 :c1 0.0 :c2 (:c2 q)}) (def u (assoc u :c1 (+ (* s (:c0 q)) (* t (:c1 q))))) (def v (assoc v :c1 (+ (* s (:c1 q)) (* t (:c2 q))))) (def u (assoc u :c2 (+ (* s (:c1 u)) (* t (:c1 v))))) (def v (assoc v :c0 (:c2 u))) (throw (ex-info "return" {:v [u v]}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn subdivideQuadCurve [q t]
  (try (do (def xs (subdivideQuadSpline (:x q) t)) (def ys (subdivideQuadSpline (:y q) t)) (def u {:x (nth xs 0) :y (nth ys 0)}) (def v {:x (nth xs 1) :y (nth ys 1)}) (throw (ex-info "return" {:v [u v]}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn rectsOverlap [xa0 ya0 xa1 ya1 xb0 yb0 xb1 yb1]
  (try (throw (ex-info "return" {:v (and (and (and (<= xb0 xa1) (<= xa0 xb1)) (<= yb0 ya1)) (<= ya0 yb1))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn testIntersect [p q tol]
  (try (do (def pxmin (min3 (:c0 (:x p)) (:c1 (:x p)) (:c2 (:x p)))) (def pymin (min3 (:c0 (:y p)) (:c1 (:y p)) (:c2 (:y p)))) (def pxmax (max3 (:c0 (:x p)) (:c1 (:x p)) (:c2 (:x p)))) (def pymax (max3 (:c0 (:y p)) (:c1 (:y p)) (:c2 (:y p)))) (def qxmin (min3 (:c0 (:x q)) (:c1 (:x q)) (:c2 (:x q)))) (def qymin (min3 (:c0 (:y q)) (:c1 (:y q)) (:c2 (:y q)))) (def qxmax (max3 (:c0 (:x q)) (:c1 (:x q)) (:c2 (:x q)))) (def qymax (max3 (:c0 (:y q)) (:c1 (:y q)) (:c2 (:y q)))) (def exclude true) (def accept false) (def inter {:x 0.0 :y 0.0}) (when (rectsOverlap pxmin pymin pxmax pymax qxmin qymin qxmax qymax) (do (def exclude false) (def xmin (maxf pxmin qxmin)) (def xmax (minf pxmax qxmax)) (when (<= (- xmax xmin) tol) (do (def ymin (maxf pymin qymin)) (def ymax (minf pymax qymax)) (when (<= (- ymax ymin) tol) (do (def accept true) (def inter (assoc inter :x (* 0.5 (+ xmin xmax)))) (def inter (assoc inter :y (* 0.5 (+ ymin ymax)))))))))) (throw (ex-info "return" {:v {"exclude" exclude "accept" accept "intersect" inter}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn seemsToBeDuplicate [pts xy spacing]
  (try (do (def i 0) (while (< i (count pts)) (do (def pt (nth pts i)) (when (and (< (absf (- (:x pt) (:x xy))) spacing) (< (absf (- (:y pt) (:y xy))) spacing)) (throw (ex-info "return" {:v true}))) (def i (+ i 1)))) (throw (ex-info "return" {:v false}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn findIntersects [p q tol spacing]
  (try (do (def inters []) (def workload [{"p" p "q" q}]) (while (> (count workload) 0) (do (def idx (- (count workload) 1)) (def work (nth workload idx)) (def workload (subvec workload 0 idx)) (def res (testIntersect (get work "p") (get work "q") tol)) (def excl (get res "exclude")) (def acc (get res "accept")) (def inter (get res "intersect")) (if acc (when (not (seemsToBeDuplicate inters inter spacing)) (def inters (conj inters inter))) (when (not excl) (do (def ps (subdivideQuadCurve (get work "p") 0.5)) (def qs (subdivideQuadCurve (get work "q") 0.5)) (def p0 (nth ps 0)) (def p1 (nth ps 1)) (def q0 (nth qs 0)) (def q1 (nth qs 1)) (def workload (conj workload {"p" p0 "q" q0})) (def workload (conj workload {"p" p0 "q" q1})) (def workload (conj workload {"p" p1 "q" q0})) (def workload (conj workload {"p" p1 "q" q1}))))))) (throw (ex-info "return" {:v inters}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (do (def p {:x {:c0 (- 1.0) :c1 0.0 :c2 1.0} :y {:c0 0.0 :c1 10.0 :c2 0.0}}) (def q {:x {:c0 2.0 :c1 (- 8.0) :c2 2.0} :y {:c0 1.0 :c1 2.0 :c2 3.0}}) (def tol 0.0000001) (def spacing (* tol 10.0)) (def inters (findIntersects p q tol spacing)) (def i 0) (while (< i (count inters)) (do (def pt (nth inters i)) (println (str (str (str (str "(" (str (:x pt))) ", ") (str (:y pt))) ")")) (def i (+ i 1))))))

(defn -main []
  (main))

(-main)
