(ns main (:refer-clojure :exclude [shuffle]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare shuffle)

(def ^:dynamic shuffle_arr nil)

(def ^:dynamic shuffle_i nil)

(def ^:dynamic shuffle_j nil)

(def ^:dynamic shuffle_tmp nil)

(defn shuffle [shuffle_xs]
  (binding [shuffle_arr nil shuffle_i nil shuffle_j nil shuffle_tmp nil] (try (do (set! shuffle_arr shuffle_xs) (set! shuffle_i (- (count shuffle_arr) 1)) (while (> shuffle_i 0) (do (set! shuffle_j (mod (swap! nowSeed (fn [s] (mod (+ (* s 1664525) 1013904223) 2147483647))) (+ shuffle_i 1))) (set! shuffle_tmp (nth shuffle_arr shuffle_i)) (set! shuffle_arr (assoc shuffle_arr shuffle_i (nth shuffle_arr shuffle_j))) (set! shuffle_arr (assoc shuffle_arr shuffle_j shuffle_tmp)) (set! shuffle_i (- shuffle_i 1)))) (throw (ex-info "return" {:v shuffle_arr}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

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
