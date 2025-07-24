(ns main (:refer-clojure :exclude [pow10 totient getPerfectPowers getAchilles sortInts pad main]))

(require 'clojure.set)

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare pow10 totient getPerfectPowers getAchilles sortInts pad main)

(defn pow10 [exp]
  (try (do (def n 1) (def i 0) (while (< i exp) (do (def n (* n 10)) (def i (+ i 1)))) (throw (ex-info "return" {:v n}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn totient [n]
  (try (do (def tot n) (def nn n) (def i 2) (while (<= (* i i) nn) (do (when (= (mod nn i) 0) (do (while (= (mod nn i) 0) (def nn (/ nn i))) (def tot (- tot (/ tot i))))) (when (= i 2) (def i 1)) (def i (+ i 2)))) (when (> nn 1) (def tot (- tot (/ tot nn)))) (throw (ex-info "return" {:v tot}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(def pps {})

(defn getPerfectPowers [maxExp]
  (do (def upper (pow10 maxExp)) (def i 2) (while (< (* i i) upper) (do (def p i) (loop [while_flag_1 true] (when (and while_flag_1 true) (do (def p (* p i)) (cond (>= p upper) (recur false) :else (do (def pps (assoc pps p true)) (recur while_flag_1)))))) (def i (+ i 1))))))

(defn getAchilles [minExp maxExp]
  (try (do (def lower (pow10 minExp)) (def upper (pow10 maxExp)) (def achilles {}) (def b 1) (while (< (* (* b b) b) upper) (do (def b3 (* (* b b) b)) (def a 1) (loop [while_flag_2 true] (when (and while_flag_2 true) (do (def p (* (* b3 a) a)) (cond (>= p upper) (recur false) :else (do (when (>= p lower) (when (not (in p pps)) (def achilles (assoc achilles p true)))) (def a (+ a 1)) (recur while_flag_2)))))) (def b (+ b 1)))) (throw (ex-info "return" {:v achilles}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn sortInts [xs]
  (try (do (def res []) (def tmp xs) (while (> (count tmp) 0) (do (def min (nth tmp 0)) (def idx 0) (def i 1) (while (< i (count tmp)) (do (when (< (nth tmp i) min) (do (def min (nth tmp i)) (def idx i))) (def i (+ i 1)))) (def res (+ res [min])) (def out []) (def j 0) (while (< j (count tmp)) (do (when (not= j idx) (def out (+ out [(nth tmp j)]))) (def j (+ j 1)))) (def tmp out))) (throw (ex-info "return" {:v res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn pad [n width]
  (try (do (def s (str n)) (while (< (count s) width) (def s (str " " s))) (throw (ex-info "return" {:v s}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (do (def maxDigits 15) (getPerfectPowers 5) (def achSet (getAchilles 1 5)) (def ach []) (doseq [k ((:keys achSet))] (def ach (+ ach [k]))) (def ach (sortInts ach)) (println "First 50 Achilles numbers:") (def i 0) (while (< i 50) (do (def line "") (def j 0) (while (< j 10) (do (def line (str line (pad (nth ach i) 4))) (when (< j 9) (def line (str line " "))) (def i (+ i 1)) (def j (+ j 1)))) (println line))) (println "\nFirst 30 strong Achilles numbers:") (def strong []) (def count 0) (def idx 0) (while (< count 30) (do (def tot (totient (nth ach idx))) (when (in tot achSet) (do (def strong (+ strong [(nth ach idx)])) (def count (+ count 1)))) (def idx (+ idx 1)))) (def i 0) (while (< i 30) (do (def line "") (def j 0) (while (< j 10) (do (def line (str line (pad (nth strong i) 5))) (when (< j 9) (def line (str line " "))) (def i (+ i 1)) (def j (+ j 1)))) (println line))) (println "\nNumber of Achilles numbers with:") (def counts [1 12 47 192 664 2242 7395 24008 77330 247449 788855 2508051 7960336 25235383]) (def d 2) (while (<= d maxDigits) (do (def c (nth counts (- d 2))) (println (str (str (pad d 2) " digits: ") (str c))) (def d (+ d 1))))))

(defn -main []
  (main))

(-main)
