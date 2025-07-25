(ns main (:refer-clojure :exclude [bottles main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare bottles main)

(defn bottles [n]
  (try (do (when (= n 0) (throw (ex-info "return" {:v "No more bottles"}))) (if (= n 1) "1 bottle" (str (str n) " bottles"))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (do (def i 99) (while (> i 0) (do (println (str (bottles i) " of beer on the wall")) (println (str (bottles i) " of beer")) (println "Take one down, pass it around") (println (str (bottles (- i 1)) " of beer on the wall")) (def i (- i 1))))))

(defn -main []
  (main))

(-main)
