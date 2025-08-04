(ns main (:refer-clojure :exclude [binom catalan main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare binom catalan main)

(declare binom_i binom_kk binom_res)

(defn binom [binom_n binom_k]
  (try (do (when (or (< binom_k 0) (> binom_k binom_n)) (throw (ex-info "return" {:v 0}))) (def binom_kk binom_k) (when (> binom_kk (- binom_n binom_kk)) (def binom_kk (- binom_n binom_kk))) (def binom_res 1) (def binom_i 0) (while (< binom_i binom_kk) (do (def binom_res (* binom_res (- binom_n binom_i))) (def binom_i (+ binom_i 1)) (def binom_res (long (/ binom_res binom_i))))) (throw (ex-info "return" {:v binom_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn catalan [catalan_n]
  (try (throw (ex-info "return" {:v (long (/ (binom (* 2 catalan_n) catalan_n) (+ catalan_n 1)))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (dotimes [i 15] (println (str (catalan i)))))

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
