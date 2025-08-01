(ns main (:refer-clojure :exclude [state printState main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare state printState main)

(declare s v)

(defn state [v]
  (try (throw (ex-info "return" {:v {:entry (= v 0) :inc (< v 10) :dec (> v 0)}})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn printState [v]
  (do (def s (state v)) (println (str (str (str (str (str (str (str "value=" (str v)) " entry=") (str (:entry s))) " inc=") (str (:inc s))) " dec=") (str (:dec s))))))

(defn main []
  (do (def v 0) (printState v) (loop [while_flag_1 true] (when (and while_flag_1 true) (do (def s (state v)) (cond (not (:inc s)) (recur false) :else (do (def v (+' v 1)) (printState v) (recur while_flag_1)))))) (loop [while_flag_2 true] (when (and while_flag_2 true) (do (def s (state v)) (cond (not (:dec s)) (recur false) :else (do (def v (- v 1)) (printState v) (recur while_flag_2))))))))

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
