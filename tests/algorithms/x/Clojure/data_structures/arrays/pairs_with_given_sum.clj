(ns main (:refer-clojure :exclude [pairs_with_sum]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(defn indexOf [s sub]
  (let [idx (clojure.string/index-of s sub)] (if (nil? idx) -1 idx)))

(defn split [s sep]
  (clojure.string/split s (re-pattern sep)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare pairs_with_sum)

(def ^:dynamic count_v nil)

(def ^:dynamic pairs_with_sum_i nil)

(def ^:dynamic pairs_with_sum_j nil)

(def ^:dynamic pairs_with_sum_n nil)

(defn pairs_with_sum [pairs_with_sum_arr pairs_with_sum_req_sum]
  (binding [count_v nil pairs_with_sum_i nil pairs_with_sum_j nil pairs_with_sum_n nil] (try (do (set! pairs_with_sum_n (count pairs_with_sum_arr)) (set! count_v 0) (set! pairs_with_sum_i 0) (while (< pairs_with_sum_i pairs_with_sum_n) (do (set! pairs_with_sum_j (+ pairs_with_sum_i 1)) (while (< pairs_with_sum_j pairs_with_sum_n) (do (when (= (+ (nth pairs_with_sum_arr pairs_with_sum_i) (nth pairs_with_sum_arr pairs_with_sum_j)) pairs_with_sum_req_sum) (set! count_v (+ count_v 1))) (set! pairs_with_sum_j (+ pairs_with_sum_j 1)))) (set! pairs_with_sum_i (+ pairs_with_sum_i 1)))) (throw (ex-info "return" {:v count_v}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (pairs_with_sum [1 5 7 1] 6))
      (println (pairs_with_sum [1 1 1 1 1 1 1 1] 2))
      (println (pairs_with_sum [1 7 6 2 5 4 3 1 9 8] 7))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
