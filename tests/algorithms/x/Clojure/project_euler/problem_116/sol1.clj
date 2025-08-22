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

(def ^:dynamic solution_i nil)

(def ^:dynamic solution_j nil)

(def ^:dynamic solution_remaining nil)

(def ^:dynamic solution_row nil)

(def ^:dynamic solution_row_length nil)

(def ^:dynamic solution_tile_length nil)

(def ^:dynamic solution_tile_start nil)

(def ^:dynamic solution_total nil)

(def ^:dynamic solution_ways nil)

(defn solution [solution_length]
  (binding [solution_i nil solution_j nil solution_remaining nil solution_row nil solution_row_length nil solution_tile_length nil solution_tile_start nil solution_total nil solution_ways nil] (try (do (set! solution_ways []) (set! solution_i 0) (while (<= solution_i solution_length) (do (set! solution_row []) (set! solution_row (conj solution_row 0)) (set! solution_row (conj solution_row 0)) (set! solution_row (conj solution_row 0)) (set! solution_ways (conj solution_ways solution_row)) (set! solution_i (+ solution_i 1)))) (set! solution_row_length 0) (while (<= solution_row_length solution_length) (do (set! solution_tile_length 2) (while (<= solution_tile_length 4) (do (set! solution_tile_start 0) (while (<= solution_tile_start (- solution_row_length solution_tile_length)) (do (set! solution_remaining (- (- solution_row_length solution_tile_start) solution_tile_length)) (set! solution_ways (assoc-in solution_ways [solution_row_length (- solution_tile_length 2)] (+ (+ (nth (nth solution_ways solution_row_length) (- solution_tile_length 2)) (nth (nth solution_ways solution_remaining) (- solution_tile_length 2))) 1))) (set! solution_tile_start (+ solution_tile_start 1)))) (set! solution_tile_length (+ solution_tile_length 1)))) (set! solution_row_length (+ solution_row_length 1)))) (set! solution_total 0) (set! solution_j 0) (while (< solution_j 3) (do (set! solution_total (+ solution_total (nth (nth solution_ways solution_length) solution_j))) (set! solution_j (+ solution_j 1)))) (throw (ex-info "return" {:v solution_total}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (solution 5))
      (println (solution 50))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
