(ns main (:refer-clojure :exclude [absf floorf indexOf fmtF padInt padFloat avgLen ana main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare absf floorf indexOf fmtF padInt padFloat avgLen ana main)

(defn absf [x]
  (try (if (< x 0.0) (- x) x) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn floorf [x]
  (try (do (def y (int x)) (throw (ex-info "return" {:v (double y)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn indexOf [s ch]
  (try (do (def i 0) (while (< i (count s)) (do (when (= (subs s i (+ i 1)) ch) (throw (ex-info "return" {:v i}))) (def i (+ i 1)))) (throw (ex-info "return" {:v (- 1)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn fmtF [x]
  (try (do (def y (/ (floorf (+ (* x 10000.0) 0.5)) 10000.0)) (def s (str y)) (def dot (indexOf s ".")) (if (= dot (- 0 1)) (def s (str s ".0000")) (do (def decs (- (- (count s) dot) 1)) (if (> decs 4) (def s (subs s 0 (+ dot 5))) (while (< decs 4) (do (def s (str s "0")) (def decs (+ decs 1))))))) (throw (ex-info "return" {:v s}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn padInt [n width]
  (try (do (def s (str n)) (while (< (count s) width) (def s (str " " s))) (throw (ex-info "return" {:v s}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn padFloat [x width]
  (try (do (def s (fmtF x)) (while (< (count s) width) (def s (str " " s))) (throw (ex-info "return" {:v s}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn avgLen [n]
  (try (do (def tests 10000) (def sum 0) (def seed 1) (def t 0) (while (< t tests) (do (def visited []) (def i 0) (while (< i n) (do (def visited (conj visited false)) (def i (+ i 1)))) (def x 0) (while (not (nth visited x)) (do (def visited (assoc visited x true)) (def sum (+ sum 1)) (def seed (mod (+ (* seed 1664525) 1013904223) 2147483647)) (def x (mod seed n)))) (def t (+ t 1)))) (throw (ex-info "return" {:v (/ (double sum) tests)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn ana [n]
  (try (do (def nn (double n)) (def term 1.0) (def sum 1.0) (def i (- nn 1.0)) (while (>= i 1.0) (do (def term (* term (/ i nn))) (def sum (+ sum term)) (def i (- i 1.0)))) (throw (ex-info "return" {:v sum}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (do (def nmax 20) (println " N    average    analytical    (error)") (println "===  =========  ============  =========") (def n 1) (while (<= n nmax) (do (def a (avgLen n)) (def b (ana n)) (def err (* (/ (absf (- a b)) b) 100.0)) (def line (str (str (str (str (str (str (str (padInt n 3) "  ") (padFloat a 9)) "  ") (padFloat b 12)) "  (") (padFloat err 6)) "%)")) (println line) (def n (+ n 1))))))

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
