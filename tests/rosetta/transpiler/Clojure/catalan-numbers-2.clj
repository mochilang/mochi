(ns main (:refer-clojure :exclude [catalanRec main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare catalanRec main)

(declare catalanRec_t1 catalanRec_t2 catalanRec_t3 catalanRec_t5)

(defn catalanRec [catalanRec_n]
  (try (do (when (= catalanRec_n 0) (throw (ex-info "return" {:v 1}))) (def catalanRec_t1 (* 2 catalanRec_n)) (def catalanRec_t2 (- catalanRec_t1 1)) (def catalanRec_t3 (* 2 catalanRec_t2)) (def catalanRec_t5 (* catalanRec_t3 (catalanRec (- catalanRec_n 1)))) (throw (ex-info "return" {:v (long (/ catalanRec_t5 (+ catalanRec_n 1)))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (doseq [i (range 1 16)] (println (str (catalanRec i)))))

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
