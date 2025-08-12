(ns main (:refer-clojure :exclude [isqrt solution]))

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

(defn _fetch [url]
  {:data [{:from "" :intensity {:actual 0 :forecast 0 :index ""} :to ""}]})

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare isqrt solution)

(def ^:dynamic isqrt_r nil)

(def ^:dynamic solution_i nil)

(def ^:dynamic solution_j nil)

(def ^:dynamic solution_k nil)

(def ^:dynamic solution_limit nil)

(def ^:dynamic solution_p nil)

(def ^:dynamic solution_sieve nil)

(def ^:dynamic solution_sum nil)

(defn isqrt [isqrt_n]
  (binding [isqrt_r nil] (try (do (set! isqrt_r 0) (while (<= (* (+ isqrt_r 1) (+ isqrt_r 1)) isqrt_n) (set! isqrt_r (+ isqrt_r 1))) (throw (ex-info "return" {:v isqrt_r}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn solution [solution_n]
  (binding [solution_i nil solution_j nil solution_k nil solution_limit nil solution_p nil solution_sieve nil solution_sum nil] (try (do (set! solution_sieve []) (set! solution_i 0) (while (<= solution_i solution_n) (do (set! solution_sieve (conj solution_sieve false)) (set! solution_i (+ solution_i 1)))) (set! solution_sieve (assoc solution_sieve 0 true)) (set! solution_sieve (assoc solution_sieve 1 true)) (set! solution_limit (isqrt solution_n)) (set! solution_p 2) (while (<= solution_p solution_limit) (do (when (not (nth solution_sieve solution_p)) (do (set! solution_j (* solution_p solution_p)) (while (<= solution_j solution_n) (do (set! solution_sieve (assoc solution_sieve solution_j true)) (set! solution_j (+ solution_j solution_p)))))) (set! solution_p (+ solution_p 1)))) (set! solution_sum 0) (set! solution_k 2) (while (< solution_k solution_n) (do (when (not (nth solution_sieve solution_k)) (set! solution_sum (+ solution_sum solution_k))) (set! solution_k (+ solution_k 1)))) (throw (ex-info "return" {:v solution_sum}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (solution 20000)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
