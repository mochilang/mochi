(ns main (:refer-clojure :exclude [isPrime countPrimeFactors pad4 main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare isPrime countPrimeFactors pad4 main)

(defn isPrime [n]
  (try (do (when (< n 2) (throw (ex-info "return" {:v false}))) (when (= (mod n 2) 0) (throw (ex-info "return" {:v (= n 2)}))) (when (= (mod n 3) 0) (throw (ex-info "return" {:v (= n 3)}))) (def d 5) (while (<= (* d d) n) (do (when (= (mod n d) 0) (throw (ex-info "return" {:v false}))) (def d (+ d 2)) (when (= (mod n d) 0) (throw (ex-info "return" {:v false}))) (def d (+ d 4)))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn countPrimeFactors [n]
  (try (do (when (= n 1) (throw (ex-info "return" {:v 0}))) (when (isPrime n) (throw (ex-info "return" {:v 1}))) (def count_v 0) (def f 2) (while true (if (= (mod n f) 0) (do (def count_v (+ count_v 1)) (def n (/ n f)) (when (= n 1) (throw (ex-info "return" {:v count_v}))) (when (isPrime n) (def f n))) (if (>= f 3) (def f (+ f 2)) (def f 3)))) (throw (ex-info "return" {:v count_v}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn pad4 [n]
  (try (do (def s (str n)) (while (< (count s) 4) (def s (str " " s))) (throw (ex-info "return" {:v s}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (do (def max 120) (println (str (str "The attractive numbers up to and including " (str max)) " are:")) (def count_v 0) (def line "") (def lineCount 0) (def i 1) (while (<= i max) (do (def c (countPrimeFactors i)) (when (isPrime c) (do (def line (str line (pad4 i))) (def count_v (+ count_v 1)) (def lineCount (+ lineCount 1)) (when (= lineCount 20) (do (println line) (def line "") (def lineCount 0))))) (def i (+ i 1)))) (when (> lineCount 0) (println line))))

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
