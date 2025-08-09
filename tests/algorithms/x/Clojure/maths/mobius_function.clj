(ns main (:refer-clojure :exclude [primeFactors isSquareFree mobius]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(defn indexOf [s sub]
  (let [idx (clojure.string/index-of s sub)] (if (nil? idx) -1 idx)))

(defn split [s sep]
  (clojure.string/split s (re-pattern sep)))

(defn toi [s]
  (Integer/parseInt (str s)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare primeFactors isSquareFree mobius)

(def ^:dynamic isSquareFree_seen nil)

(def ^:dynamic mobius_factors nil)

(def ^:dynamic primeFactors_factors nil)

(def ^:dynamic primeFactors_i nil)

(def ^:dynamic primeFactors_n nil)

(defn primeFactors [primeFactors_n_p]
  (binding [primeFactors_factors nil primeFactors_i nil primeFactors_n nil] (try (do (set! primeFactors_n primeFactors_n_p) (set! primeFactors_i 2) (set! primeFactors_factors []) (while (<= (* primeFactors_i primeFactors_i) primeFactors_n) (if (= (mod primeFactors_n primeFactors_i) 0) (do (set! primeFactors_factors (conj primeFactors_factors primeFactors_i)) (set! primeFactors_n (quot primeFactors_n primeFactors_i))) (set! primeFactors_i (+ primeFactors_i 1)))) (when (> primeFactors_n 1) (set! primeFactors_factors (conj primeFactors_factors primeFactors_n))) (throw (ex-info "return" {:v primeFactors_factors}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn isSquareFree [isSquareFree_factors]
  (binding [isSquareFree_seen nil] (try (do (set! isSquareFree_seen {}) (doseq [f isSquareFree_factors] (do (when (in f isSquareFree_seen) (throw (ex-info "return" {:v false}))) (set! isSquareFree_seen (assoc isSquareFree_seen f true)))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn mobius [mobius_n]
  (binding [mobius_factors nil] (try (do (set! mobius_factors (primeFactors mobius_n)) (if (isSquareFree mobius_factors) (if (= (mod (count mobius_factors) 2) 0) 1 (- 1)) 0)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (mobius 24))
      (println (mobius (- 1)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
