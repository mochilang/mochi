(ns main (:refer-clojure :exclude [multiplier main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare multiplier main)

(declare i inverses mfs n1n2 numbers x xi y yi z zi)

(defn multiplier [n1 n2]
  (try (do (def n1n2 (* n1 n2)) (throw (ex-info "return" {:v (fn [m] (* n1n2 m))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (do (def x 2.0) (def xi 0.5) (def y 4.0) (def yi 0.25) (def z (+' x y)) (def zi (/ 1.0 (+' x y))) (def numbers [x y z]) (def inverses [xi yi zi]) (def mfs []) (def i 0) (while (< i (count numbers)) (do (def mfs (conj mfs (multiplier (nth numbers i) (nth inverses i)))) (def i (+' i 1)))) (doseq [mf mfs] (println (str (mf 1.0))))))

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
