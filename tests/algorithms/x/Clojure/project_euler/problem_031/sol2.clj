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

(def ^:dynamic solution_coin nil)

(def ^:dynamic solution_coins nil)

(def ^:dynamic solution_i nil)

(def ^:dynamic solution_idx nil)

(def ^:dynamic solution_j nil)

(def ^:dynamic solution_ways nil)

(defn solution [solution_pence]
  (binding [solution_coin nil solution_coins nil solution_i nil solution_idx nil solution_j nil solution_ways nil] (try (do (set! solution_coins [1 2 5 10 20 50 100 200]) (set! solution_ways []) (set! solution_i 0) (while (<= solution_i solution_pence) (do (set! solution_ways (conj solution_ways 0)) (set! solution_i (+ solution_i 1)))) (set! solution_ways (assoc solution_ways 0 1)) (set! solution_idx 0) (while (< solution_idx (count solution_coins)) (do (set! solution_coin (nth solution_coins solution_idx)) (set! solution_j solution_coin) (while (<= solution_j solution_pence) (do (set! solution_ways (assoc solution_ways solution_j (+ (nth solution_ways solution_j) (nth solution_ways (- solution_j solution_coin))))) (set! solution_j (+ solution_j 1)))) (set! solution_idx (+ solution_idx 1)))) (throw (ex-info "return" {:v (nth solution_ways solution_pence)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (solution 500))
      (println (solution 200))
      (println (solution 50))
      (println (solution 10))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
