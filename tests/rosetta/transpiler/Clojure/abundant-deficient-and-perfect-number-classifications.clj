(ns main (:refer-clojure :exclude [pfacSum main]))

(require 'clojure.set)

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare pfacSum main)

(defn pfacSum [i]
  (try (do (def sum 0) (def p 1) (while (<= p (/ i 2)) (do (when (= (mod i p) 0) (def sum (+ sum p))) (def p (+ p 1)))) (throw (ex-info "return" {:v sum}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (do (def d 0) (def a 0) (def pnum 0) (def i 1) (while (<= i 20000) (do (def j (pfacSum i)) (when (< j i) (def d (+ d 1))) (when (= j i) (def pnum (+ pnum 1))) (when (> j i) (def a (+ a 1))) (def i (+ i 1)))) (println (str (str "There are " (str d)) " deficient numbers between 1 and 20000")) (println (str (str "There are " (str a)) " abundant numbers  between 1 and 20000")) (println (str (str "There are " (str pnum)) " perfect numbers between 1 and 20000"))))

(defn -main []
  (main))

(-main)
