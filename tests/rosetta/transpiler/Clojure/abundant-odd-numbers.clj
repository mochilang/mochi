(ns main (:refer-clojure :exclude [divisors sum sumStr pad2 pad5 abundantOdd main]))

(require 'clojure.set)

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(defn divisors [n]
  (try (do (def divs [1]) (def divs2 []) (def i 2) (while (<= (* i i) n) (do (when (= (mod n i) 0) (do (def j (int (/ n i))) (def divs (conj divs i)) (when (not= i j) (def divs2 (conj divs2 j))))) (def i (+ i 1)))) (def j (- (count divs2) 1)) (while (>= j 0) (do (def divs (conj divs (nth divs2 j))) (def j (- j 1)))) (throw (ex-info "return" {:v divs}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn sum [xs]
  (try (do (def tot 0) (doseq [v xs] (def tot (+ tot v))) (throw (ex-info "return" {:v tot}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn sumStr [xs]
  (try (do (def s "") (def i 0) (while (< i (count xs)) (do (def s (str (str s (str (nth xs i))) " + ")) (def i (+ i 1)))) (throw (ex-info "return" {:v (subs s 0 (- (count s) 3))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn pad2 [n]
  (do (def s (str n)) (if (< (count s) 2) (str " " s) s)))

(defn pad5 [n]
  (try (do (def s (str n)) (while (< (count s) 5) (def s (str " " s))) (throw (ex-info "return" {:v s}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn abundantOdd [searchFrom countFrom countTo printOne]
  (try (do (def count countFrom) (def n searchFrom) (loop [while_flag_1 true] (when (and while_flag_1 (< count countTo)) (do (def divs (divisors n)) (def tot (reduce + 0 divs)) (when (> tot n) (do (def count (+ count 1)) (when (and printOne (< count countTo)) (do (def n (+ n 2)) (recur true))) (def s (sumStr divs)) (if (not printOne) (println (str (str (str (str (str (str (pad2 count) ". ") (pad5 n)) " < ") s) " = ") (str tot))) (println (str (str (str (str (str n) " < ") s) " = ") (str tot)))))) (def n (+ n 2)) (cond :else (recur while_flag_1))))) (throw (ex-info "return" {:v n}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (do (def max 25) (println (str (str "The first " (str max)) " abundant odd numbers are:")) (def n (abundantOdd 1 0 max false)) (println "\nThe one thousandth abundant odd number is:") (abundantOdd n max 1000 true) (println "\nThe first abundant odd number above one billion is:") (abundantOdd 1000000001 0 1 true)))

(defn -main []
  (main))

(-main)
