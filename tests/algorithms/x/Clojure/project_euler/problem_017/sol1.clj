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

(def ^:dynamic solution_i nil)

(def ^:dynamic solution_ones_counts nil)

(def ^:dynamic solution_remainder nil)

(def ^:dynamic solution_tens_counts nil)

(defn solution [solution_n]
  (binding [count_v nil solution_i nil solution_ones_counts nil solution_remainder nil solution_tens_counts nil] (try (do (set! solution_ones_counts [0 3 3 5 4 4 3 5 5 4 3 6 6 8 8 7 7 9 8 8]) (set! solution_tens_counts [0 0 6 6 5 5 5 7 6 6]) (set! count_v 0) (set! solution_i 1) (while (<= solution_i solution_n) (do (if (< solution_i 1000) (do (when (>= solution_i 100) (do (set! count_v (+ (+ count_v (nth solution_ones_counts (quot solution_i 100))) 7)) (when (not= (mod solution_i 100) 0) (set! count_v (+ count_v 3))))) (set! solution_remainder (mod solution_i 100)) (if (and (> solution_remainder 0) (< solution_remainder 20)) (set! count_v (+ count_v (nth solution_ones_counts solution_remainder))) (do (set! count_v (+ count_v (nth solution_ones_counts (mod solution_i 10)))) (set! count_v (+ count_v (nth solution_tens_counts (quot (- solution_remainder (mod solution_i 10)) 10))))))) (set! count_v (+ (+ count_v (nth solution_ones_counts (quot solution_i 1000))) 8))) (set! solution_i (+ solution_i 1)))) (throw (ex-info "return" {:v count_v}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (mochi_str (solution 1000)))
      (println (mochi_str (solution 5)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
