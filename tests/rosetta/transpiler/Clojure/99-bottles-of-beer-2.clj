(ns main (:refer-clojure :exclude [fields join numberName pluralizeFirst randInt slur main]))

(require 'clojure.set)

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare fields join numberName pluralizeFirst randInt slur main)

(defn fields [s]
  (try (do (def words []) (def cur "") (def i 0) (while (< i (count s)) (do (def ch (subs s i (+ i 1))) (if (or (or (= ch " ") (= ch "\n")) (= ch "\t")) (when (> (count cur) 0) (do (def words (conj words cur)) (def cur ""))) (def cur (str cur ch))) (def i (+ i 1)))) (when (> (count cur) 0) (def words (conj words cur))) (throw (ex-info "return" {:v words}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn join [xs sep]
  (try (do (def res "") (def i 0) (while (< i (count xs)) (do (when (> i 0) (def res (str res sep))) (def res (str res (nth xs i))) (def i (+ i 1)))) (throw (ex-info "return" {:v res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn numberName [n]
  (try (do (def small ["no" "one" "two" "three" "four" "five" "six" "seven" "eight" "nine" "ten" "eleven" "twelve" "thirteen" "fourteen" "fifteen" "sixteen" "seventeen" "eighteen" "nineteen"]) (def tens ["ones" "ten" "twenty" "thirty" "forty" "fifty" "sixty" "seventy" "eighty" "ninety"]) (when (< n 0) (throw (ex-info "return" {:v ""}))) (when (< n 20) (throw (ex-info "return" {:v (nth small n)}))) (when (< n 100) (do (def t (nth tens (int (/ n 10)))) (def s (mod n 10)) (when (> s 0) (def t (str (str t " ") (nth small s)))) (throw (ex-info "return" {:v t})))) (throw (ex-info "return" {:v ""}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn pluralizeFirst [s n]
  (try (do (when (= n 1) (throw (ex-info "return" {:v s}))) (def w (fields s)) (when (> (count w) 0) (def w (assoc w 0 (str (nth w 0) "s")))) (throw (ex-info "return" {:v (join w " ")}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn randInt [seed n]
  (try (do (def next (mod (+ (* seed 1664525) 1013904223) 2147483647)) (throw (ex-info "return" {:v (mod next n)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn slur [p d]
  (try (do (when (<= (count p) 2) (throw (ex-info "return" {:v p}))) (def a []) (def i 1) (while (< i (- (count p) 1)) (do (def a (conj a (subs p i (+ i 1)))) (def i (+ i 1)))) (def idx (- (count a) 1)) (def seed d) (while (>= idx 1) (do (def seed (mod (+ (* seed 1664525) 1013904223) 2147483647)) (when (>= (mod seed 100) d) (do (def j (mod seed (+ idx 1))) (def tmp (nth a idx)) (def a (assoc a idx (nth a j))) (def a (assoc a j tmp)))) (def idx (- idx 1)))) (def s (subs p 0 1)) (def k 0) (while (< k (count a)) (do (def s (+ s (nth a k))) (def k (+ k 1)))) (def s (+ s (subs p (- (count p) 1) (count p)))) (def w (fields s)) (throw (ex-info "return" {:v (join w " ")}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (do (def i 99) (while (> i 0) (do (println (str (str (str (str (slur (numberName i) i) " ") (pluralizeFirst (slur "bottle of" i) i)) " ") (slur "beer on the wall" i))) (println (str (str (str (str (slur (numberName i) i) " ") (pluralizeFirst (slur "bottle of" i) i)) " ") (slur "beer" i))) (println (str (str (str (str (slur "take one" i) " ") (slur "down" i)) " ") (slur "pass it around" i))) (println (str (str (str (str (slur (numberName (- i 1)) i) " ") (pluralizeFirst (slur "bottle of" i) (- i 1))) " ") (slur "beer on the wall" i))) (def i (- i 1))))))

(defn -main []
  (main))

(-main)
