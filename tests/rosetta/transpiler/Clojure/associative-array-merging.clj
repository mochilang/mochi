(ns main (:refer-clojure :exclude [merge main]))

(require 'clojure.set)

(defrecord Update [price color year])

(defrecord Base [name price color])

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare merge main)

(defn merge [base update]
  (try (do (def result {}) (doseq [k base] (def result (assoc result k (nth base k)))) (doseq [k update] (def result (assoc result k (nth update k)))) (throw (ex-info "return" {:v result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (do (def base {"name" "Rocket Skates" "price" 12.75 "color" "yellow"}) (def update {"price" 15.25 "color" "red" "year" 1974}) (def result (merge base update)) (println result)))

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
