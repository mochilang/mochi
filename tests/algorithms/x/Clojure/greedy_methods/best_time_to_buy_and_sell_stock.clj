(ns main (:refer-clojure :exclude [max_profit]))

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

(declare max_profit)

(def ^:dynamic max_profit_i nil)

(def ^:dynamic max_profit_min_price nil)

(def ^:dynamic max_profit_price nil)

(def ^:dynamic max_profit_profit nil)

(def ^:dynamic max_profit_v nil)

(defn max_profit [max_profit_prices]
  (binding [max_profit_i nil max_profit_min_price nil max_profit_price nil max_profit_profit nil max_profit_v nil] (try (do (when (= (count max_profit_prices) 0) (throw (ex-info "return" {:v 0}))) (set! max_profit_min_price (nth max_profit_prices 0)) (set! max_profit_v 0) (set! max_profit_i 0) (while (< max_profit_i (count max_profit_prices)) (do (set! max_profit_price (nth max_profit_prices max_profit_i)) (when (< max_profit_price max_profit_min_price) (set! max_profit_min_price max_profit_price)) (set! max_profit_profit (- max_profit_price max_profit_min_price)) (when (> max_profit_profit max_profit_v) (set! max_profit_v max_profit_profit)) (set! max_profit_i (+ max_profit_i 1)))) (throw (ex-info "return" {:v max_profit_v}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (max_profit [7 1 5 3 6 4]))
      (println (max_profit [7 6 4 3 1]))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
