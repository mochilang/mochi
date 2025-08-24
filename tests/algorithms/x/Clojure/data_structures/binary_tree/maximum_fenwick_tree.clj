(ns main (:refer-clojure :exclude [zeros update query]))

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

(declare zeros update query)

(declare _read_file)

(def ^:dynamic main_arr nil)

(def ^:dynamic query_i nil)

(def ^:dynamic query_result nil)

(def ^:dynamic update_arr nil)

(def ^:dynamic zeros_i nil)

(def ^:dynamic zeros_res nil)

(defn zeros [zeros_n]
  (binding [zeros_i nil zeros_res nil] (try (do (set! zeros_res []) (set! zeros_i 0) (while (< zeros_i zeros_n) (do (set! zeros_res (conj zeros_res 0)) (set! zeros_i (+ zeros_i 1)))) (throw (ex-info "return" {:v zeros_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn update [update_arr_p update_idx update_value]
  (binding [update_arr update_arr_p] (try (set! update_arr (assoc update_arr update_idx update_value)) (finally (alter-var-root (var update_arr) (constantly update_arr))))))

(defn query [query_arr query_left query_right]
  (binding [query_i nil query_result nil] (try (do (set! query_result 0) (set! query_i query_left) (while (< query_i query_right) (do (when (> (nth query_arr query_i) query_result) (set! query_result (nth query_arr query_i))) (set! query_i (+ query_i 1)))) (throw (ex-info "return" {:v query_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_arr nil)

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (alter-var-root (var main_arr) (constantly [0 0 0 0 0]))
      (println (query main_arr 0 5))
      (let [__res (update main_arr 4 100)] (do (alter-var-root (var main_arr) (constantly update_arr)) __res))
      (println (query main_arr 0 5))
      (let [__res (update main_arr 4 0)] (do (alter-var-root (var main_arr) (constantly update_arr)) __res))
      (let [__res (update main_arr 2 20)] (do (alter-var-root (var main_arr) (constantly update_arr)) __res))
      (println (query main_arr 0 5))
      (let [__res (update main_arr 4 10)] (do (alter-var-root (var main_arr) (constantly update_arr)) __res))
      (println (query main_arr 2 5))
      (println (query main_arr 1 5))
      (let [__res (update main_arr 2 0)] (do (alter-var-root (var main_arr) (constantly update_arr)) __res))
      (println (query main_arr 0 5))
      (alter-var-root (var main_arr) (constantly (zeros 10000)))
      (let [__res (update main_arr 255 30)] (do (alter-var-root (var main_arr) (constantly update_arr)) __res))
      (println (query main_arr 0 10000))
      (alter-var-root (var main_arr) (constantly (zeros 6)))
      (let [__res (update main_arr 5 1)] (do (alter-var-root (var main_arr) (constantly update_arr)) __res))
      (println (query main_arr 5 6))
      (alter-var-root (var main_arr) (constantly (zeros 6)))
      (let [__res (update main_arr 0 1000)] (do (alter-var-root (var main_arr) (constantly update_arr)) __res))
      (println (query main_arr 0 1))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
