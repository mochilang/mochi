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

(def ^:dynamic solution_best nil)

(def ^:dynamic solution_i nil)

(def ^:dynamic solution_j nil)

(def ^:dynamic solution_k nil)

(def ^:dynamic solution_last nil)

(def ^:dynamic solution_max_val nil)

(def ^:dynamic solution_number1 nil)

(def ^:dynamic solution_number2 nil)

(def ^:dynamic solution_prev_row nil)

(def ^:dynamic solution_triangle nil)

(defn solution []
  (binding [solution_best nil solution_i nil solution_j nil solution_k nil solution_last nil solution_max_val nil solution_number1 nil solution_number2 nil solution_prev_row nil solution_triangle nil] (try (do (set! solution_triangle [[75] [95 64] [17 47 82] [18 35 87 10] [20 4 82 47 65] [19 1 23 75 3 34] [88 2 77 73 7 63 67] [99 65 4 28 6 16 70 92] [41 41 26 56 83 40 80 70 33] [41 48 72 33 47 32 37 16 94 29] [53 71 44 65 25 43 91 52 97 51 14] [70 11 33 28 77 73 17 78 39 68 17 57] [91 71 52 38 17 14 91 43 58 50 27 29 48] [63 66 4 68 89 53 67 30 73 16 69 87 40 31] [4 62 98 27 23 9 70 98 73 93 38 53 60 4 23]]) (set! solution_i 1) (while (< solution_i (count solution_triangle)) (do (set! solution_j 0) (while (< solution_j (count (nth solution_triangle solution_i))) (do (set! solution_prev_row (nth solution_triangle (- solution_i 1))) (set! solution_number1 (if (not= solution_j (count solution_prev_row)) (nth solution_prev_row solution_j) 0)) (set! solution_number2 (if (> solution_j 0) (nth solution_prev_row (- solution_j 1)) 0)) (set! solution_max_val (if (> solution_number1 solution_number2) solution_number1 solution_number2)) (set! solution_triangle (assoc-in solution_triangle [solution_i solution_j] (+ (nth (nth solution_triangle solution_i) solution_j) solution_max_val))) (set! solution_j (+ solution_j 1)))) (set! solution_i (+ solution_i 1)))) (set! solution_last (nth solution_triangle (- (count solution_triangle) 1))) (set! solution_k 0) (set! solution_best 0) (while (< solution_k (count solution_last)) (do (when (> (nth solution_last solution_k) solution_best) (set! solution_best (nth solution_last solution_k))) (set! solution_k (+ solution_k 1)))) (throw (ex-info "return" {:v solution_best}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (mochi_str (solution)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
