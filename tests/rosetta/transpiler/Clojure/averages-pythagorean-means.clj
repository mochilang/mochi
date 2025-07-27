(ns main (:refer-clojure :exclude [powf nthRoot main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare powf nthRoot main)

(defn powf [base exp]
  (try (do (def result 1.0) (def i 0) (while (< i exp) (do (def result (* result base)) (def i (+ i 1)))) (throw (ex-info "return" {:v result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn nthRoot [x n]
  (try (do (def low 0.0) (def high x) (def i 0) (while (< i 60) (do (def mid (/ (+ low high) 2.0)) (if (> (powf mid n) x) (def high mid) (def low mid)) (def i (+ i 1)))) (throw (ex-info "return" {:v low}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (do (def sum 0.0) (def sumRecip 0.0) (def prod 1.0) (def n 1) (while (<= n 10) (do (def f (double n)) (def sum (+ sum f)) (def sumRecip (+ sumRecip (/ 1.0 f))) (def prod (* prod f)) (def n (+ n 1)))) (def count_v 10.0) (def a (/ sum count_v)) (def g (nthRoot prod 10)) (def h (/ count_v sumRecip)) (println (str (str (str (str (str "A: " (str a)) " G: ") (str g)) " H: ") (str h))) (println (str "A >= G >= H: " (str (and (>= a g) (>= g h)))))))

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
