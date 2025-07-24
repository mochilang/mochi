(ns main (:refer-clojure :exclude [accumulator main]))

(require 'clojure.set)

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare accumulator main)

(defn add [sum nv]
  (try (do (def store (assoc store 0 (+ (nth store 0) nv))) (throw (ex-info "return" {:v (nth store 0)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn accumulator [sum]
  (try (do (def store [sum]) (throw (ex-info "return" {:v add}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (do (def x (accumulator 1)) (x 5) (accumulator 3) (println (str (x 2.3)))))

(defn -main []
  (main))

(-main)
