(ns main)

(require 'clojure.set)

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(def testpkg {:Add (fn [a b] (+ a b)) :Pi 3.14 :Answer 42 :FifteenPuzzleExample "Solution found in 52 moves: rrrulddluuuldrurdddrullulurrrddldluurddlulurruldrdrd"})

(defn -main []
  (println "Solution found in 52 moves: rrrulddluuuldrurdddrullulurrrddldluurddlulurruldrdrd"))

(-main)
