(ns main (:refer-clojure :exclude [exponential_moving_average]))

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

(declare exponential_moving_average)

(def ^:dynamic exponential_moving_average_alpha nil)

(def ^:dynamic exponential_moving_average_i nil)

(def ^:dynamic exponential_moving_average_moving_average nil)

(def ^:dynamic exponential_moving_average_price nil)

(def ^:dynamic exponential_moving_average_result nil)

(defn exponential_moving_average [exponential_moving_average_stock_prices exponential_moving_average_window_size]
  (binding [exponential_moving_average_alpha nil exponential_moving_average_i nil exponential_moving_average_moving_average nil exponential_moving_average_price nil exponential_moving_average_result nil] (try (do (when (<= exponential_moving_average_window_size 0) (throw (Exception. "window_size must be > 0"))) (set! exponential_moving_average_alpha (/ 2.0 (+ 1.0 (double exponential_moving_average_window_size)))) (set! exponential_moving_average_moving_average 0.0) (set! exponential_moving_average_result []) (set! exponential_moving_average_i 0) (while (< exponential_moving_average_i (count exponential_moving_average_stock_prices)) (do (set! exponential_moving_average_price (nth exponential_moving_average_stock_prices exponential_moving_average_i)) (if (<= exponential_moving_average_i exponential_moving_average_window_size) (if (= exponential_moving_average_i 0) (set! exponential_moving_average_moving_average exponential_moving_average_price) (set! exponential_moving_average_moving_average (* (+ exponential_moving_average_moving_average exponential_moving_average_price) 0.5))) (set! exponential_moving_average_moving_average (+ (* exponential_moving_average_alpha exponential_moving_average_price) (* (- 1.0 exponential_moving_average_alpha) exponential_moving_average_moving_average)))) (set! exponential_moving_average_result (conj exponential_moving_average_result exponential_moving_average_moving_average)) (set! exponential_moving_average_i (+ exponential_moving_average_i 1)))) (throw (ex-info "return" {:v exponential_moving_average_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_stock_prices [2.0 5.0 3.0 8.2 6.0 9.0 10.0])

(def ^:dynamic main_window_size 3)

(def ^:dynamic main_result (exponential_moving_average main_stock_prices main_window_size))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str main_result))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
