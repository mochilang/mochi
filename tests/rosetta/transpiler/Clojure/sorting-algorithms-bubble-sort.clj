(ns main (:refer-clojure :exclude [bubbleSort]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare bubbleSort)

(declare arr hasChanged index itemCount list tmp)

(defn bubbleSort [a]
  (try (do (def arr a) (def itemCount (- (count arr) 1)) (loop [while_flag_1 true] (when (and while_flag_1 true) (do (def hasChanged false) (def index 0) (while (< index itemCount) (do (when (> (nth arr index) (nth arr (+' index 1))) (do (def tmp (nth arr index)) (def arr (assoc arr index (nth arr (+' index 1)))) (def arr (assoc arr (+' index 1) tmp)) (def hasChanged true))) (def index (+' index 1)))) (cond (not hasChanged) (recur false) :else (do (def itemCount (- itemCount 1)) (recur while_flag_1)))))) (throw (ex-info "return" {:v arr}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(def list [31 41 59 26 53 58 97 93 23 84])

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str "unsorted: " (str list)))
      (def list (bubbleSort list))
      (println (str "sorted!  " (str list)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
