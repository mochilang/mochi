(ns main (:refer-clojure :exclude [solution]))

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

(declare solution)

(declare _read_file)

(def ^:dynamic count_v nil)

(def ^:dynamic solution_den nil)

(def ^:dynamic solution_i nil)

(def ^:dynamic solution_num nil)

(def ^:dynamic solution_prev_den nil)

(def ^:dynamic solution_prev_num nil)

(defn solution [solution_n]
  (binding [count_v nil solution_den nil solution_i nil solution_num nil solution_prev_den nil solution_prev_num nil] (try (do (set! solution_prev_num 1) (set! solution_prev_den 1) (set! count_v 0) (set! solution_i 1) (while (<= solution_i solution_n) (do (set! solution_num (+ solution_prev_num (* 2 solution_prev_den))) (set! solution_den (+ solution_prev_num solution_prev_den)) (when (> (count (mochi_str solution_num)) (count (mochi_str solution_den))) (set! count_v (+ count_v 1))) (set! solution_prev_num solution_num) (set! solution_prev_den solution_den) (set! solution_i (+ solution_i 1)))) (throw (ex-info "return" {:v count_v}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (solution 14))
      (println (solution 100))
      (println (solution 1000))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
