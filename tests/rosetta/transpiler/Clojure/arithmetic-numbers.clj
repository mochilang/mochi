(ns main (:refer-clojure :exclude [sieve primesFrom pad3 commatize primeCount arithmeticNumbers main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare sieve primesFrom pad3 commatize primeCount arithmeticNumbers main)

(defn sieve [limit]
  (try (do (def spf []) (def i 0) (while (<= i limit) (do (def spf (conj spf 0)) (def i (+ i 1)))) (def i 2) (while (<= i limit) (do (when (= (nth spf i) 0) (do (def spf (assoc spf i i)) (when (<= (* i i) limit) (do (def j (* i i)) (while (<= j limit) (do (when (= (nth spf j) 0) (def spf (assoc spf j i))) (def j (+ j i)))))))) (def i (+ i 1)))) (throw (ex-info "return" {:v spf}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn primesFrom [spf limit]
  (try (do (def primes []) (def i 3) (while (<= i limit) (do (when (= (nth spf i) i) (def primes (conj primes i))) (def i (+ i 1)))) (throw (ex-info "return" {:v primes}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn pad3 [n]
  (try (do (def s (str n)) (while (< (count s) 3) (def s (str " " s))) (throw (ex-info "return" {:v s}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn commatize [n]
  (try (do (def s (str n)) (def out "") (def i (- (count s) 1)) (def c 0) (while (>= i 0) (do (def out (str (subs s i (+ i 1)) out)) (def c (+ c 1)) (when (and (= (mod c 3) 0) (> i 0)) (def out (str "," out))) (def i (- i 1)))) (throw (ex-info "return" {:v out}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn primeCount [primes last spf]
  (try (do (def lo 0) (def hi (count primes)) (while (< lo hi) (do (def mid (int (/ (+ lo hi) 2))) (if (< (nth primes mid) last) (def lo (+ mid 1)) (def hi mid)))) (def count_v (+ lo 1)) (when (not= (nth spf last) last) (def count_v (- count_v 1))) (throw (ex-info "return" {:v count_v}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn arithmeticNumbers [limit spf]
  (try (do (def arr [1]) (def n 3) (while (< (count arr) limit) (do (if (= (nth spf n) n) (def arr (conj arr n)) (do (def x n) (def sigma 1) (def tau 1) (while (> x 1) (do (def p (nth spf x)) (when (= p 0) (def p x)) (def cnt 0) (def power p) (def sum 1) (while (= (mod x p) 0) (do (def x (/ x p)) (def cnt (+ cnt 1)) (def sum (+ sum power)) (def power (* power p)))) (def sigma (* sigma sum)) (def tau (* tau (+ cnt 1))))) (when (= (mod sigma tau) 0) (def arr (conj arr n))))) (def n (+ n 1)))) (throw (ex-info "return" {:v arr}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (do (def limit 1228663) (def spf (sieve limit)) (def primes (primesFrom spf limit)) (def arr (arithmeticNumbers 1000000 spf)) (println "The first 100 arithmetic numbers are:") (def i 0) (while (< i 100) (do (def line "") (def j 0) (while (< j 10) (do (def line (str line (pad3 (nth arr (+ i j))))) (when (< j 9) (def line (str line " "))) (def j (+ j 1)))) (println line) (def i (+ i 10)))) (doseq [x [1000 10000 100000 1000000]] (do (def last (nth arr (- x 1))) (def lastc (commatize last)) (println (str (str (str "\nThe " (commatize x)) "th arithmetic number is: ") lastc)) (def pc (primeCount primes last spf)) (def comp (- (- x pc) 1)) (println (str (str (str (str "The count of such numbers <= " lastc) " which are composite is ") (commatize comp)) "."))))))

(defn -main []
  (main))

(-main)
