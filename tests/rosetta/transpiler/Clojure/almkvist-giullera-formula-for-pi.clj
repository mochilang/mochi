(ns main (:refer-clojure :exclude [bigCmp bigMulSmall bigMulBig bigMulPow10 bigDivSmall repeat sortInts primesUpTo factorialExp factorSmall computeIP formatTerm bigAbsDiff main]))

(require 'clojure.set)

(defn bigTrim [a]
  a)

(defn bigFromInt [x]
  (bigint x))

(defn bigAdd [a b]
  (+ a b))

(defn bigSub [a b]
  (- a b))

(defn bigToString [a]
  (str a))

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare bigCmp bigMulSmall bigMulBig bigMulPow10 bigDivSmall repeat sortInts primesUpTo factorialExp factorSmall computeIP formatTerm bigAbsDiff main)

(defn bigCmp [a b]
  (try (do (when (> (count a) (count b)) (throw (ex-info "return" {:v 1}))) (when (< (count a) (count b)) (throw (ex-info "return" {:v (- 1)}))) (def i (- (count a) 1)) (while (>= i 0) (do (when (> (nth a i) (nth b i)) (throw (ex-info "return" {:v 1}))) (when (< (nth a i) (nth b i)) (throw (ex-info "return" {:v (- 1)}))) (def i (- i 1)))) (throw (ex-info "return" {:v 0}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn bigMulSmall [a m]
  (try (do (when (= m 0) (throw (ex-info "return" {:v [0]}))) (def res []) (def carry 0) (def i 0) (while (< i (count a)) (do (def prod (+ (* (nth a i) m) carry)) (def res (conj res (mod prod 10))) (def carry (/ prod 10)) (def i (+ i 1)))) (while (> carry 0) (do (def res (conj res (mod carry 10))) (def carry (/ carry 10)))) (throw (ex-info "return" {:v (bigTrim res)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn bigMulBig [a b]
  (try (do (def res []) (def i 0) (while (< i (+ (count a) (count b))) (do (def res (conj res 0)) (def i (+ i 1)))) (def i 0) (while (< i (count a)) (do (def carry 0) (def j 0) (while (< j (count b)) (do (def idx (+ i j)) (def prod (+ (+ (nth res idx) (* (nth a i) (nth b j))) carry)) (def res (assoc res idx (mod prod 10))) (def carry (/ prod 10)) (def j (+ j 1)))) (def idx (+ i (count b))) (while (> carry 0) (do (def prod (+ (nth res idx) carry)) (def res (assoc res idx (mod prod 10))) (def carry (/ prod 10)) (def idx (+ idx 1)))) (def i (+ i 1)))) (throw (ex-info "return" {:v (bigTrim res)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn bigMulPow10 [a k]
  (try (do (def i 0) (while (< i k) (do (def a (+ [0] a)) (def i (+ i 1)))) (throw (ex-info "return" {:v a}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn bigDivSmall [a m]
  (try (do (def res []) (def rem 0) (def i (- (count a) 1)) (while (>= i 0) (do (def cur (+ (* rem 10) (nth a i))) (def q (/ cur m)) (def rem (mod cur m)) (def res (+ [q] res)) (def i (- i 1)))) (throw (ex-info "return" {:v (bigTrim res)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn repeat [ch n]
  (try (do (def s "") (def i 0) (while (< i n) (do (def s (str s ch)) (def i (+ i 1)))) (throw (ex-info "return" {:v s}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn sortInts [xs]
  (try (do (def res []) (def tmp xs) (while (> (count tmp) 0) (do (def min (nth tmp 0)) (def idx 0) (def i 1) (while (< i (count tmp)) (do (when (< (nth tmp i) min) (do (def min (nth tmp i)) (def idx i))) (def i (+ i 1)))) (def res (+ res [min])) (def out []) (def j 0) (while (< j (count tmp)) (do (when (not= j idx) (def out (+ out [(nth tmp j)]))) (def j (+ j 1)))) (def tmp out))) (throw (ex-info "return" {:v res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn primesUpTo [n]
  (try (do (def sieve []) (def i 0) (while (<= i n) (do (def sieve (conj sieve true)) (def i (+ i 1)))) (def p 2) (while (<= (* p p) n) (do (when (nth sieve p) (do (def m (* p p)) (while (<= m n) (do (def sieve (assoc sieve m false)) (def m (+ m p)))))) (def p (+ p 1)))) (def res []) (def x 2) (while (<= x n) (do (when (nth sieve x) (def res (conj res x))) (def x (+ x 1)))) (throw (ex-info "return" {:v res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn factorialExp [n primes]
  (try (do (def m {}) (loop [p_seq primes] (when (seq p_seq) (let [p (first p_seq)] (cond (> p n) (recur nil) :else (do (def t n) (def e 0) (while (> t 0) (do (def t (/ t p)) (def e (+ e t)))) (def m (assoc m (str p) e)) (recur (rest p_seq))))))) (throw (ex-info "return" {:v m}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn factorSmall [x primes]
  (try (do (def f {}) (def n x) (loop [p_seq primes] (when (seq p_seq) (let [p (first p_seq)] (cond (> (* p p) n) (recur nil) :else (do (def c 0) (while (= (mod n p) 0) (do (def c (+ c 1)) (def n (/ n p)))) (when (> c 0) (def f (assoc f (str p) c))) (recur (rest p_seq))))))) (when (> n 1) (def f (assoc f (str n) (+ ((:get f) (str n) 0) 1)))) (throw (ex-info "return" {:v f}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn computeIP [n primes]
  (try (do (def exps (factorialExp (* 6 n) primes)) (def fn (factorialExp n primes)) (doseq [k fn] (def exps (assoc exps k (- ((:get exps) k 0) (* 6 (nth fn k)))))) (def exps (assoc exps "2" (+ ((:get exps) "2" 0) 5))) (def t2 (+ (+ (* (* 532 n) n) (* 126 n)) 9)) (def ft2 (factorSmall t2 primes)) (doseq [k ft2] (def exps (assoc exps k (+ ((:get exps) k 0) (nth ft2 k))))) (def exps (assoc exps "3" (- ((:get exps) "3" 0) 1))) (def keys []) (doseq [k exps] (def keys (conj keys (Integer/parseInt k)))) (def keys (sortInts keys)) (def res (bigFromInt 1)) (doseq [p keys] (do (def e (get exps (str p))) (def i 0) (while (< i e) (do (def res (bigMulSmall res p)) (def i (+ i 1)))))) (throw (ex-info "return" {:v res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn formatTerm [ip pw]
  (try (do (def s (bigToString ip)) (when (>= pw (count s)) (do (def frac (+ (repeat "0" (- pw (count s))) s)) (when (< (count frac) 33) (def frac (+ frac (repeat "0" (- 33 (count frac)))))) (throw (ex-info "return" {:v (str "0." (subs frac 0 33))})))) (def intpart (subs s 0 (- (count s) pw))) (def frac (subs s (- (count s) pw) (count s))) (when (< (count frac) 33) (def frac (str frac (repeat "0" (- 33 (count frac)))))) (throw (ex-info "return" {:v (str (str intpart ".") (subs frac 0 33))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn bigAbsDiff [a b]
  (try (if (>= (bigCmp a b) 0) (bigSub a b) (bigSub b a)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (do (def primes (primesUpTo 2000)) (println "N                               Integer Portion  Pow  Nth Term (33 dp)") (def line (repeat "-" 89)) (println line) (def sum (bigFromInt 0)) (def prev (bigFromInt 0)) (def denomPow 0) (def n 0) (loop [while_flag_1 true] (when (and while_flag_1 true) (do (def ip (computeIP n primes)) (def pw (+ (* 6 n) 3)) (when (> pw denomPow) (do (def sum (bigMulPow10 sum (- pw denomPow))) (def prev (bigMulPow10 prev (- pw denomPow))) (def denomPow pw))) (when (< n 10) (do (def termStr (formatTerm ip pw)) (def ipStr (bigToString ip)) (while (< (count ipStr) 44) (def ipStr (str " " ipStr))) (def pwStr (str (- pw))) (while (< (count pwStr) 3) (def pwStr (str " " pwStr))) (def padTerm termStr) (while (< (count padTerm) 35) (def padTerm (str padTerm " "))) (println (str (str (str (str (str (str (str n) "  ") ipStr) "  ") pwStr) "  ") padTerm)))) (def sum (bigAdd sum ip)) (def diff (bigAbsDiff sum prev)) (cond (and (>= denomPow 70) (< (bigCmp diff (bigMulPow10 (bigFromInt 1) (- denomPow 70))) 0)) (recur false) :else (do (def prev sum) (def n (+ n 1)) (recur while_flag_1)))))) (def precision 70) (def target (bigMulPow10 (bigFromInt 1) (+ denomPow (* 2 precision)))) (def low (bigFromInt 0)) (def high (bigMulPow10 (bigFromInt 1) (+ precision 1))) (while (< (bigCmp low (bigSub high (bigFromInt 1))) 0) (do (def mid (bigDivSmall (bigAdd low high) 2)) (def prod (bigMulBig (bigMulBig mid mid) sum)) (if (<= (bigCmp prod target) 0) (def low mid) (def high (bigSub mid (bigFromInt 1)))))) (def piInt low) (def piStr (bigToString piInt)) (when (<= (count piStr) precision) (def piStr (+ (repeat "0" (+ (- precision (count piStr)) 1)) piStr))) (def out (str (str (subs piStr 0 (- (count piStr) precision)) ".") (subs piStr (- (count piStr) precision) (count piStr)))) (println "") (println "Pi to 70 decimal places is:") (println out)))

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
