(ns main (:refer-clojure :exclude [sqrtApprox abs main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare sqrtApprox abs main)

(declare actual g i iters limit oldPhi phi)

(defn sqrtApprox [x]
  (try (do (when (<= x 0.0) (throw (ex-info "return" {:v 0.0}))) (def g x) (def i 0) (while (< i 20) (do (def g (/ (+' g (/ x g)) 2.0)) (def i (+' i 1)))) (throw (ex-info "return" {:v g}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn abs [x]
  (try (if (< x 0.0) (- x) x) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (do (def oldPhi 1.0) (def phi 0.0) (def iters 0) (def limit 0.00001) (loop [while_flag_1 true] (when (and while_flag_1 true) (do (def phi (+' 1.0 (/ 1.0 oldPhi))) (def iters (+' iters 1)) (cond (<= (abs (- phi oldPhi)) limit) (recur false) :else (do (def oldPhi phi) (recur while_flag_1)))))) (def actual (/ (+' 1.0 (sqrtApprox 5.0)) 2.0)) (println (str "Final value of phi : " (str phi))) (println (str "Number of iterations : " (str iters))) (println (str "Error (approx) : " (str (- phi actual))))))

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
