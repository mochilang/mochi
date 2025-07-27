(ns main (:refer-clojure :exclude [primeFactors repeat D pad main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare primeFactors repeat D pad main)

(defn primeFactors [n]
  (try (do (def factors []) (def x n) (while (= (mod x 2) 0) (do (def factors (conj factors 2)) (def x (int (/ x 2))))) (def p 3) (while (<= (* p p) x) (do (while (= (mod x p) 0) (do (def factors (conj factors p)) (def x (int (/ x p))))) (def p (+ p 2)))) (when (> x 1) (def factors (conj factors x))) (throw (ex-info "return" {:v factors}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn repeat [ch n]
  (try (do (def s "") (def i 0) (while (< i n) (do (def s (str s ch)) (def i (+ i 1)))) (throw (ex-info "return" {:v s}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn D [n]
  (try (do (when (< n 0.0) (throw (ex-info "return" {:v (- (D (- n)))}))) (when (< n 2.0) (throw (ex-info "return" {:v 0.0}))) (def factors []) (if (< n 10000000000000000000.0) (def factors (primeFactors (int n))) (do (def g (int (/ n 100.0))) (def factors (primeFactors g)) (def factors (conj factors 2)) (def factors (conj factors 2)) (def factors (conj factors 5)) (def factors (conj factors 5)))) (def c (count factors)) (when (= c 1) (throw (ex-info "return" {:v 1.0}))) (when (= c 2) (throw (ex-info "return" {:v (double (+ (nth factors 0) (nth factors 1)))}))) (def d (/ n (double (nth factors 0)))) (throw (ex-info "return" {:v (+ (* (D d) (double (nth factors 0))) d)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn pad [n]
  (try (do (def s (str n)) (while (< (count s) 4) (def s (str " " s))) (throw (ex-info "return" {:v s}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (do (def vals []) (def n (- 99)) (while (< n 101) (do (def vals (conj vals (int (D (double n))))) (def n (+ n 1)))) (def i 0) (while (< i (count vals)) (do (def line "") (def j 0) (while (< j 10) (do (def line (str line (pad (nth vals (+ i j))))) (when (< j 9) (def line (str line " "))) (def j (+ j 1)))) (println line) (def i (+ i 10)))) (def pow 1.0) (def m 1) (while (< m 21) (do (def pow (* pow 10.0)) (def exp (str m)) (when (< (count exp) 2) (def exp (str exp " "))) (def res (str (str m) (repeat "0" (- m 1)))) (println (str (str (str "D(10^" exp) ") / 7 = ") res)) (def m (+ m 1))))))

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
