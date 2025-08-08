(ns main (:refer-clojure :exclude [calc_profit main]))

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

(declare calc_profit main)

(def ^:dynamic calc_profit_gain nil)

(def ^:dynamic calc_profit_i nil)

(def ^:dynamic calc_profit_j nil)

(def ^:dynamic calc_profit_k nil)

(def ^:dynamic calc_profit_limit nil)

(def ^:dynamic calc_profit_maxIndex nil)

(def ^:dynamic calc_profit_maxRatio nil)

(def ^:dynamic calc_profit_n nil)

(def ^:dynamic calc_profit_ratio nil)

(def ^:dynamic calc_profit_used nil)

(def ^:dynamic count_v nil)

(defn calc_profit [calc_profit_profit calc_profit_weight calc_profit_max_weight]
  (binding [calc_profit_gain nil calc_profit_i nil calc_profit_j nil calc_profit_k nil calc_profit_limit nil calc_profit_maxIndex nil calc_profit_maxRatio nil calc_profit_n nil calc_profit_ratio nil calc_profit_used nil count_v nil] (try (do (when (not= (count calc_profit_profit) (count calc_profit_weight)) (throw (Exception. "The length of profit and weight must be same."))) (when (<= calc_profit_max_weight 0) (throw (Exception. "max_weight must greater than zero."))) (set! calc_profit_i 0) (while (< calc_profit_i (count calc_profit_profit)) (do (when (< (nth calc_profit_profit calc_profit_i) 0) (throw (Exception. "Profit can not be negative."))) (when (< (nth calc_profit_weight calc_profit_i) 0) (throw (Exception. "Weight can not be negative."))) (set! calc_profit_i (+ calc_profit_i 1)))) (set! calc_profit_n (count calc_profit_profit)) (set! calc_profit_used []) (set! calc_profit_j 0) (while (< calc_profit_j calc_profit_n) (do (set! calc_profit_used (conj calc_profit_used false)) (set! calc_profit_j (+ calc_profit_j 1)))) (set! calc_profit_limit 0) (set! calc_profit_gain 0.0) (set! count_v 0) (loop [while_flag_1 true] (when (and while_flag_1 (and (< calc_profit_limit calc_profit_max_weight) (< count_v calc_profit_n))) (do (set! calc_profit_maxRatio (- 1.0)) (set! calc_profit_maxIndex (- 1)) (set! calc_profit_k 0) (while (< calc_profit_k calc_profit_n) (do (when (not (nth calc_profit_used calc_profit_k)) (do (set! calc_profit_ratio (quot (double (nth calc_profit_profit calc_profit_k)) (double (nth calc_profit_weight calc_profit_k)))) (when (> calc_profit_ratio calc_profit_maxRatio) (do (set! calc_profit_maxRatio calc_profit_ratio) (set! calc_profit_maxIndex calc_profit_k))))) (set! calc_profit_k (+ calc_profit_k 1)))) (cond (< calc_profit_maxIndex 0) (recur false) :else (do (set! calc_profit_used (assoc calc_profit_used calc_profit_maxIndex true)) (if (>= (- calc_profit_max_weight calc_profit_limit) (nth calc_profit_weight calc_profit_maxIndex)) (do (set! calc_profit_limit (+ calc_profit_limit (nth calc_profit_weight calc_profit_maxIndex))) (set! calc_profit_gain (+ calc_profit_gain (double (nth calc_profit_profit calc_profit_maxIndex))))) (do (set! calc_profit_gain (+ calc_profit_gain (* (quot (double (- calc_profit_max_weight calc_profit_limit)) (double (nth calc_profit_weight calc_profit_maxIndex))) (double (nth calc_profit_profit calc_profit_maxIndex))))) (recur false))) (set! count_v (+ count_v 1)) (recur while_flag_1)))))) (throw (ex-info "return" {:v calc_profit_gain}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (do (println (calc_profit [1 2 3] [3 4 5] 15)) (println (calc_profit [10 9 8] [3 4 5] 25)) (println (calc_profit [10 9 8] [3 4 5] 5))))

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
