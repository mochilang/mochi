(ns main (:refer-clojure :exclude [main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare main)

(declare INF a alt b cur dist first_v i idx j k n next_v nrow p row s ui vi x)

(defn path [u v]
  (try (do (def ui (- u 1)) (def vi (- v 1)) (when (= (nth (nth next_v ui) vi) (- 0 1)) (throw (ex-info "return" {:v []}))) (def p [u]) (def cur ui) (while (not= cur vi) (do (def cur (nth (nth next_v cur) vi)) (def p (conj p (+' cur 1))))) (throw (ex-info "return" {:v p}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn pathStr [p]
  (try (do (def s "") (def first_v true) (def idx 0) (while (< idx (count p)) (do (def x (nth p idx)) (when (not first_v) (def s (str s " -> "))) (def s (str s (str x))) (def first_v false) (def idx (+' idx 1)))) (throw (ex-info "return" {:v s}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (try (do (def INF 1000000000) (def n 4) (def dist []) (def next_v []) (def i 0) (while (< i n) (do (def row []) (def nrow []) (def j 0) (while (< j n) (do (if (= i j) (def row (conj row 0)) (def row (conj row INF))) (def nrow (conj nrow (- 0 1))) (def j (+' j 1)))) (def dist (conj dist row)) (def next_v (conj next_v nrow)) (def i (+' i 1)))) (def dist (assoc-in dist [0 2] (- 2))) (def next_v (assoc-in next_v [0 2] 2)) (def dist (assoc-in dist [2 3] 2)) (def next_v (assoc-in next_v [2 3] 3)) (def dist (assoc-in dist [3 1] (- 1))) (def next_v (assoc-in next_v [3 1] 1)) (def dist (assoc-in dist [1 0] 4)) (def next_v (assoc-in next_v [1 0] 0)) (def dist (assoc-in dist [1 2] 3)) (def next_v (assoc-in next_v [1 2] 2)) (def k 0) (while (< k n) (do (def i 0) (while (< i n) (do (def j 0) (while (< j n) (do (when (and (< (nth (nth dist i) k) INF) (< (nth (nth dist k) j) INF)) (do (def alt (+' (nth (nth dist i) k) (nth (nth dist k) j))) (when (< alt (nth (nth dist i) j)) (do (def dist (assoc-in dist [i j] alt)) (def next_v (assoc-in next_v [i j] (nth (nth next_v i) k))))))) (def j (+' j 1)))) (def i (+' i 1)))) (def k (+' k 1)))) (println "pair\tdist\tpath") (def a 0) (while (< a n) (do (def b 0) (while (< b n) (do (when (not= a b) (println (str (str (str (str (str (str (str (+' a 1)) " -> ") (str (+' b 1))) "\t") (str (nth (nth dist a) b))) "\t") (pathStr (path (+' a 1) (+' b 1)))))) (def b (+' b 1)))) (def a (+' a 1))))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn -main []
  (main))

(-main)
