(ns main (:refer-clojure :exclude [prefix_sum]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(defn indexOf [s sub]
  (let [idx (clojure.string/index-of s sub)] (if (nil? idx) -1 idx)))

(defn split [s sep]
  (clojure.string/split s (re-pattern sep)))

(defn toi [s]
  (Integer/parseInt (str s)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare prefix_sum)

(def ^:dynamic prefix_sum_dp nil)

(def ^:dynamic prefix_sum_i nil)

(def ^:dynamic prefix_sum_j nil)

(def ^:dynamic prefix_sum_q nil)

(def ^:dynamic prefix_sum_result nil)

(def ^:dynamic prefix_sum_sum nil)

(defn prefix_sum [prefix_sum_arr prefix_sum_queries]
  (binding [prefix_sum_dp nil prefix_sum_i nil prefix_sum_j nil prefix_sum_q nil prefix_sum_result nil prefix_sum_sum nil] (try (do (set! prefix_sum_dp []) (set! prefix_sum_i 0) (while (< prefix_sum_i (count prefix_sum_arr)) (do (if (= prefix_sum_i 0) (set! prefix_sum_dp (conj prefix_sum_dp (nth prefix_sum_arr 0))) (set! prefix_sum_dp (conj prefix_sum_dp (+ (nth prefix_sum_dp (- prefix_sum_i 1)) (nth prefix_sum_arr prefix_sum_i))))) (set! prefix_sum_i (+ prefix_sum_i 1)))) (set! prefix_sum_result []) (set! prefix_sum_j 0) (while (< prefix_sum_j (count prefix_sum_queries)) (do (set! prefix_sum_q (nth prefix_sum_queries prefix_sum_j)) (set! prefix_sum_sum (nth prefix_sum_dp (:right prefix_sum_q))) (when (> (:left prefix_sum_q) 0) (set! prefix_sum_sum (- prefix_sum_sum (nth prefix_sum_dp (- (:left prefix_sum_q) 1))))) (set! prefix_sum_result (conj prefix_sum_result prefix_sum_sum)) (set! prefix_sum_j (+ prefix_sum_j 1)))) (throw (ex-info "return" {:v prefix_sum_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_arr1 [1 4 6 2 61 12])

(def ^:dynamic main_queries1 [{:left 2 :right 5} {:left 1 :right 5} {:left 3 :right 4}])

(def ^:dynamic main_arr2 [4 2 1 6 3])

(def ^:dynamic main_queries2 [{:left 3 :right 4} {:left 1 :right 3} {:left 0 :right 2}])

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (prefix_sum main_arr1 main_queries1)))
      (println (str (prefix_sum main_arr2 main_queries2)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
