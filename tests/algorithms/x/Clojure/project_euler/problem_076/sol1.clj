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

(def ^:dynamic solution_k nil)

(def ^:dynamic solution_memo nil)

(def ^:dynamic solution_n nil)

(def ^:dynamic solution_row nil)

(defn solution [solution_m]
  (binding [solution_i nil solution_j nil solution_k nil solution_memo nil solution_n nil solution_row nil] (try (do (set! solution_memo []) (set! solution_i 0) (while (<= solution_i solution_m) (do (set! solution_row []) (set! solution_j 0) (while (< solution_j solution_m) (do (set! solution_row (conj solution_row 0)) (set! solution_j (+ solution_j 1)))) (set! solution_memo (conj solution_memo solution_row)) (set! solution_i (+ solution_i 1)))) (set! solution_i 0) (while (<= solution_i solution_m) (do (set! solution_memo (assoc-in solution_memo [solution_i 0] 1)) (set! solution_i (+ solution_i 1)))) (set! solution_n 0) (while (<= solution_n solution_m) (do (set! solution_k 1) (while (< solution_k solution_m) (do (set! solution_memo (assoc-in solution_memo [solution_n solution_k] (+ (nth (nth solution_memo solution_n) solution_k) (nth (nth solution_memo solution_n) (- solution_k 1))))) (when (> solution_n solution_k) (set! solution_memo (assoc-in solution_memo [solution_n solution_k] (+ (nth (nth solution_memo solution_n) solution_k) (nth (nth solution_memo (- (- solution_n solution_k) 1)) solution_k))))) (set! solution_k (+ solution_k 1)))) (set! solution_n (+ solution_n 1)))) (throw (ex-info "return" {:v (- (nth (nth solution_memo solution_m) (- solution_m 1)) 1)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (mochi_str (solution 100)))
      (println (mochi_str (solution 50)))
      (println (mochi_str (solution 30)))
      (println (mochi_str (solution 10)))
      (println (mochi_str (solution 5)))
      (println (mochi_str (solution 3)))
      (println (mochi_str (solution 2)))
      (println (mochi_str (solution 1)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
