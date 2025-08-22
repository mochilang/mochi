(ns main (:refer-clojure :exclude [solution main]))

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

(declare solution main)

(declare _read_file)

(def ^:dynamic solution_i nil)

(def ^:dynamic solution_index nil)

(def ^:dynamic solution_item nil)

(def ^:dynamic solution_j nil)

(def ^:dynamic solution_partitions nil)

(def ^:dynamic solution_sign nil)

(defn solution [solution_number]
  (binding [solution_i nil solution_index nil solution_item nil solution_j nil solution_partitions nil solution_sign nil] (try (do (set! solution_partitions [1]) (set! solution_i (count solution_partitions)) (loop [while_flag_1 true] (when (and while_flag_1 true) (do (set! solution_item 0) (set! solution_j 1) (loop [while_flag_2 true] (when (and while_flag_2 true) (do (set! solution_sign (if (= (mod solution_j 2) 0) (- 1) 1)) (set! solution_index (/ (- (* (* solution_j solution_j) 3) solution_j) 2)) (cond (> solution_index solution_i) (recur false) (> solution_index solution_i) (recur false) :else (do (set! solution_item (+ solution_item (* (nth solution_partitions (- solution_i solution_index)) solution_sign))) (set! solution_item (mod solution_item solution_number)) (set! solution_index (+ solution_index solution_j)) (set! solution_item (+ solution_item (* (nth solution_partitions (- solution_i solution_index)) solution_sign))) (set! solution_item (mod solution_item solution_number)) (set! solution_j (+ solution_j 1)) (recur while_flag_2)))))) (when (= solution_item 0) (throw (ex-info "return" {:v solution_i}))) (set! solution_partitions (conj solution_partitions solution_item)) (set! solution_i (+ solution_i 1)) (cond :else (do))))) (throw (ex-info "return" {:v 0}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (do (println (mochi_str (solution 1))) (println (mochi_str (solution 9))) (println (mochi_str (solution 1000000)))))

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
