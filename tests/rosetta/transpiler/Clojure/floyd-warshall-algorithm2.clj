(ns main (:refer-clojure :exclude [floydWarshall path pathStr]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare floydWarshall path pathStr)

(declare INF alt dist drow g i j k n next_v nrow p res row s x)

(def INF 1000000)

(defn floydWarshall [graph]
  (try (do (def n (count graph)) (def dist []) (def next_v []) (def i 0) (while (< i n) (do (def drow []) (def nrow []) (def j 0) (while (< j n) (do (def drow (conj drow (nth (nth graph i) j))) (if (and (< (nth (nth graph i) j) INF) (not= i j)) (def nrow (conj nrow j)) (def nrow (conj nrow (- 1)))) (def j (+' j 1)))) (def dist (conj dist drow)) (def next_v (conj next_v nrow)) (def i (+' i 1)))) (def k 0) (while (< k n) (do (def i 0) (while (< i n) (do (def j 0) (while (< j n) (do (when (and (< (nth (nth dist i) k) INF) (< (nth (nth dist k) j) INF)) (do (def alt (+' (nth (nth dist i) k) (nth (nth dist k) j))) (when (< alt (nth (nth dist i) j)) (do (def dist (assoc-in dist [i j] alt)) (def next_v (assoc-in next_v [i j] (nth (nth next_v i) k))))))) (def j (+' j 1)))) (def i (+' i 1)))) (def k (+' k 1)))) (throw (ex-info "return" {:v {:dist dist :next next_v}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn path [u v next_v]
  (try (do (when (< (nth (nth next_v u) v) 0) (throw (ex-info "return" {:v []}))) (def p [u]) (def x u) (while (not= x v) (do (def x (nth (nth next_v x) v)) (def p (conj p x)))) (throw (ex-info "return" {:v p}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn pathStr [p]
  (try (do (def s "") (def i 0) (while (< i (count p)) (do (def s (str s (str (+' (nth p i) 1)))) (when (< i (- (count p) 1)) (def s (str s " -> "))) (def i (+' i 1)))) (throw (ex-info "return" {:v s}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(def n 4)

(def g [])

(def res (floydWarshall g))

(def i 0)

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (dotimes [i n] (do (def row []) (dotimes [j n] (if (= i j) (def row (conj row 0)) (def row (conj row INF)))) (def g (conj g row))))
      (def g (assoc-in g [0 2] (- 2)))
      (def g (assoc-in g [2 3] 2))
      (def g (assoc-in g [3 1] (- 1)))
      (def g (assoc-in g [1 0] 4))
      (def g (assoc-in g [1 2] 3))
      (println "pair\tdist\tpath")
      (while (< i n) (do (def j 0) (while (< j n) (do (when (not= i j) (do (def p (path i j (:next res))) (println (str (str (str (str (str (str (str (+' i 1)) " -> ") (str (+' j 1))) "\t") (str (nth (get (:dist res) i) j))) "\t") (pathStr p))))) (def j (+' j 1)))) (def i (+' i 1))))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
