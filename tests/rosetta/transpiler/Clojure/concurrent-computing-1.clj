(ns main (:refer-clojure :exclude [shuffle]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare shuffle)

(declare shuffle_arr shuffle_i shuffle_j shuffle_tmp)

(defn shuffle [shuffle_xs]
  (try (do (def shuffle_arr shuffle_xs) (def shuffle_i (- (count shuffle_arr) 1)) (while (> shuffle_i 0) (do (def shuffle_j (mod (swap! nowSeed (fn [s] (mod (+ (* s 1664525) 1013904223) 2147483647))) (+ shuffle_i 1))) (def shuffle_tmp (nth shuffle_arr shuffle_i)) (def shuffle_arr (assoc shuffle_arr shuffle_i (nth shuffle_arr shuffle_j))) (def shuffle_arr (assoc shuffle_arr shuffle_j shuffle_tmp)) (def shuffle_i (- shuffle_i 1)))) (throw (ex-info "return" {:v shuffle_arr}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (doseq [w (shuffle ["Enjoy" "Rosetta" "Code"])] (println w))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
