(ns main (:refer-clojure :exclude [step main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare step main)

(declare count_v den i limit line m n num pow primes program res two)

(defn step [n_p program]
  (try (do (def n n_p) (def i 0) (while (< i (count program)) (do (def num (nth (nth program i) 0)) (def den (nth (nth program i) 1)) (when (= (mod n den) (bigint 0)) (do (def n (* (/ n den) num)) (throw (ex-info "return" {:v {:n n :ok true}})))) (def i (+' i 1)))) (throw (ex-info "return" {:v {:n n :ok false}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (do (def program [[(bigint 17) (bigint 91)] [(bigint 78) (bigint 85)] [(bigint 19) (bigint 51)] [(bigint 23) (bigint 38)] [(bigint 29) (bigint 33)] [(bigint 77) (bigint 29)] [(bigint 95) (bigint 23)] [(bigint 77) (bigint 19)] [(bigint 1) (bigint 17)] [(bigint 11) (bigint 13)] [(bigint 13) (bigint 11)] [(bigint 15) (bigint 14)] [(bigint 15) (bigint 2)] [(bigint 55) (bigint 1)]]) (def n 2) (def primes 0) (def count_v 0) (def limit 1000000) (def two 2) (def line "") (loop [while_flag_1 true] (when (and while_flag_1 (and (< primes 20) (< count_v limit))) (do (def res (step n program)) (def n (:n res)) (cond (not (:ok res)) (recur false) :else (do (def m n) (def pow 0) (while (= (mod m two) (bigint 0)) (do (def m (/ m two)) (def pow (+' pow 1)))) (when (and (= m (bigint 1)) (> pow 1)) (do (def line (str (str line (str pow)) " ")) (def primes (+' primes 1)))) (def count_v (+' count_v 1)) (recur while_flag_1)))))) (if (> (count line) 0) (println (subs line 0 (- (count line) 1))) (println ""))))

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
