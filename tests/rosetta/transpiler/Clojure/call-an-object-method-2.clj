(ns main (:refer-clojure :exclude [newFactory]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare newFactory)

(declare Count New New_b funcs newFactory_sn)

(defn New []
  (try (do (def New_sn (+ New_sn 1)) (def New_b {:secret New_sn}) (if (= New_sn 1) (def New_b (assoc New_b :Contents "rabbit")) (when (= New_sn 2) (def New_b (assoc New_b :Contents "rock")))) (throw (ex-info "return" {:v New_b}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn Count []
  (try (throw (ex-info "return" {:v Count_sn})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn newFactory []
  (try (do (def newFactory_sn 0) (throw (ex-info "return" {:v [New Count]}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(def funcs (newFactory))

(def New (nth funcs 0))

(def Count (nth funcs 1))

(defn -main []
)

(-main)
