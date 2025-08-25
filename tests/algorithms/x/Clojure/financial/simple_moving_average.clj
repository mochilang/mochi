(ns main (:refer-clojure :exclude [simple_moving_average]))

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
  (int (Double/valueOf (str s))))

(defn _ord [s]
  (int (first s)))

(defn mochi_str [v]
  (cond (float? v) (let [s (str v)] (if (clojure.string/ends-with? s ".0") (subs s 0 (- (count s) 2)) s)) :else (str v)))

(defn _fetch [url]
  {:data [{:from "" :intensity {:actual 0 :forecast 0 :index ""} :to ""}]})

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare simple_moving_average)

(declare _read_file)

(def ^:dynamic main_idx nil)

(def ^:dynamic simple_moving_average_avg nil)

(def ^:dynamic simple_moving_average_i nil)

(def ^:dynamic simple_moving_average_result nil)

(def ^:dynamic simple_moving_average_window_sum nil)

(defn simple_moving_average [simple_moving_average_data simple_moving_average_window_size]
  (binding [simple_moving_average_avg nil simple_moving_average_i nil simple_moving_average_result nil simple_moving_average_window_sum nil] (try (do (when (< simple_moving_average_window_size 1) (throw (Exception. "Window size must be a positive integer"))) (set! simple_moving_average_result []) (set! simple_moving_average_window_sum 0.0) (set! simple_moving_average_i 0) (while (< simple_moving_average_i (count simple_moving_average_data)) (do (set! simple_moving_average_window_sum (+ simple_moving_average_window_sum (nth simple_moving_average_data simple_moving_average_i))) (when (>= simple_moving_average_i simple_moving_average_window_size) (set! simple_moving_average_window_sum (- simple_moving_average_window_sum (nth simple_moving_average_data (- simple_moving_average_i simple_moving_average_window_size))))) (if (>= simple_moving_average_i (- simple_moving_average_window_size 1)) (do (set! simple_moving_average_avg (/ simple_moving_average_window_sum simple_moving_average_window_size)) (set! simple_moving_average_result (conj simple_moving_average_result {:ok true :value simple_moving_average_avg}))) (set! simple_moving_average_result (conj simple_moving_average_result {:ok false :value 0.0}))) (set! simple_moving_average_i (+ simple_moving_average_i 1)))) (throw (ex-info "return" {:v simple_moving_average_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_data nil)

(def ^:dynamic main_window_size nil)

(def ^:dynamic main_sma_values nil)

(def ^:dynamic main_idx nil)

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (alter-var-root (var main_data) (constantly [10.0 12.0 15.0 13.0 14.0 16.0 18.0 17.0 19.0 21.0]))
      (alter-var-root (var main_window_size) (constantly 3))
      (alter-var-root (var main_sma_values) (constantly (simple_moving_average main_data main_window_size)))
      (alter-var-root (var main_idx) (constantly 0))
      (while (< main_idx (count main_sma_values)) (do (def ^:dynamic main_item (nth main_sma_values main_idx)) (if (:ok main_item) (println (str (str (str "Day " (mochi_str (+ main_idx 1))) ": ") (mochi_str (:value main_item)))) (println (str (str "Day " (mochi_str (+ main_idx 1))) ": Not enough data for SMA"))) (alter-var-root (var main_idx) (constantly (+ main_idx 1)))))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
