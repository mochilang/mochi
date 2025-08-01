(ns main (:refer-clojure :exclude [sieve goldbachCount pad main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare sieve goldbachCount pad main)

(declare c count_v i k line n p primes v val)

(defn sieve [limit]
  (try (do (def primes []) (def i 0) (while (< i limit) (do (def primes (conj primes true)) (def i (+' i 1)))) (def primes (assoc primes 0 false)) (def primes (assoc primes 1 false)) (def p 2) (while (< (* p p) limit) (do (when (nth primes p) (do (def k (* p p)) (while (< k limit) (do (def primes (assoc primes k false)) (def k (+' k p)))))) (def p (+' p 1)))) (throw (ex-info "return" {:v primes}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn goldbachCount [primes n]
  (try (do (def c 0) (def i 1) (while (<= i (/ n 2)) (do (when (and (nth primes i) (nth primes (- n i))) (def c (+' c 1))) (def i (+' i 1)))) (throw (ex-info "return" {:v c}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn pad [n]
  (try (do (when (< n 10) (throw (ex-info "return" {:v (str "  " (str n))}))) (if (< n 100) (str " " (str n)) (str n))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (do (def primes (sieve 1000)) (println "The first 100 Goldbach numbers:") (def line "") (def n 2) (def count_v 0) (while (< count_v 100) (do (def v (goldbachCount primes (* 2 n))) (def line (str (str line (pad v)) " ")) (def count_v (+' count_v 1)) (def n (+' n 1)) (when (= (mod count_v 10) 0) (do (println (subs line 0 (- (count line) 1))) (def line ""))))) (def val (goldbachCount primes 1000)) (println (str "\nThe 1,000th Goldbach number = " (str val)))))

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
