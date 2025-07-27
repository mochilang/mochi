(ns main (:refer-clojure :exclude [concatInts concatAny]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare concatInts concatAny)

(defn concatInts [a b]
  (try (do (def out []) (doseq [v a] (def out (conj out v))) (doseq [v b] (def out (conj out v))) (throw (ex-info "return" {:v out}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn concatAny [a b]
  (try (do (def out []) (doseq [v a] (def out (conj out v))) (doseq [v b] (def out (conj out v))) (throw (ex-info "return" {:v out}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (def a [1 2 3])
      (def b [7 12 60])
      (println (str (concatInts a b)))
      (def i [1 2 3])
      (def j ["Crosby" "Stills" "Nash" "Young"])
      (println (str (concatAny i j)))
      (def l [1 2 3])
      (def m [7 12 60])
      (println (str (concatInts l m)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
