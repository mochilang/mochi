(ns main (:refer-clojure :exclude [poolPut poolGet clearPool main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare poolPut poolGet clearPool main)

(defn poolPut [p x]
  (try (throw (ex-info "return" {:v (conj p x)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn poolGet [p]
  (try (do (when (= (count p) 0) (do (println "pool empty") (throw (ex-info "return" {:v {"pool" p "val" 0}})))) (def idx (- (count p) 1)) (def v (nth p idx)) (def p (subvec p 0 idx)) (throw (ex-info "return" {:v {"pool" p "val" v}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn clearPool [p]
  (try (throw (ex-info "return" {:v []})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (do (def pool []) (def i 1) (def j 2) (println (str (+ i j))) (def pool (poolPut pool i)) (def pool (poolPut pool j)) (def i 0) (def j 0) (def res1 (poolGet pool)) (def pool (get res1 "pool")) (def i (int (get res1 "val"))) (def res2 (poolGet pool)) (def pool (get res2 "pool")) (def j (int (get res2 "val"))) (def i 4) (def j 5) (println (str (+ i j))) (def pool (poolPut pool i)) (def pool (poolPut pool j)) (def i 0) (def j 0) (def pool (clearPool pool)) (def res3 (poolGet pool)) (def pool (get res3 "pool")) (def i (int (get res3 "val"))) (def res4 (poolGet pool)) (def pool (get res4 "pool")) (def j (int (get res4 "val"))) (def i 7) (def j 8) (println (str (+ i j)))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (main)
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
