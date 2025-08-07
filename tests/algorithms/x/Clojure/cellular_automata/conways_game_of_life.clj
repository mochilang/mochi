(ns main (:refer-clojure :exclude [new_generation generate_generations main]))

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

(declare new_generation generate_generations main)

(def ^:dynamic count_v nil)

(def ^:dynamic generate_generations_current nil)

(def ^:dynamic generate_generations_i nil)

(def ^:dynamic generate_generations_result nil)

(def ^:dynamic main_frames nil)

(def ^:dynamic main_i nil)

(def ^:dynamic new_generation_alive nil)

(def ^:dynamic new_generation_cols nil)

(def ^:dynamic new_generation_i nil)

(def ^:dynamic new_generation_j nil)

(def ^:dynamic new_generation_row nil)

(def ^:dynamic new_generation_rows nil)

(def ^:dynamic next_v nil)

(def ^:dynamic main_GLIDER [[0 1 0 0 0 0 0 0] [0 0 1 0 0 0 0 0] [1 1 1 0 0 0 0 0] [0 0 0 0 0 0 0 0] [0 0 0 0 0 0 0 0] [0 0 0 0 0 0 0 0] [0 0 0 0 0 0 0 0] [0 0 0 0 0 0 0 0]])

(def ^:dynamic main_BLINKER [[0 1 0] [0 1 0] [0 1 0]])

(defn new_generation [new_generation_cells]
  (binding [count_v nil new_generation_alive nil new_generation_cols nil new_generation_i nil new_generation_j nil new_generation_row nil new_generation_rows nil next_v nil] (try (do (set! new_generation_rows (count new_generation_cells)) (set! new_generation_cols (count (nth new_generation_cells 0))) (set! next_v []) (set! new_generation_i 0) (while (< new_generation_i new_generation_rows) (do (set! new_generation_row []) (set! new_generation_j 0) (while (< new_generation_j new_generation_cols) (do (set! count_v 0) (when (and (> new_generation_i 0) (> new_generation_j 0)) (set! count_v (+ count_v (nth (nth new_generation_cells (- new_generation_i 1)) (- new_generation_j 1))))) (when (> new_generation_i 0) (set! count_v (+ count_v (nth (nth new_generation_cells (- new_generation_i 1)) new_generation_j)))) (when (and (> new_generation_i 0) (< new_generation_j (- new_generation_cols 1))) (set! count_v (+ count_v (nth (nth new_generation_cells (- new_generation_i 1)) (+ new_generation_j 1))))) (when (> new_generation_j 0) (set! count_v (+ count_v (nth (nth new_generation_cells new_generation_i) (- new_generation_j 1))))) (when (< new_generation_j (- new_generation_cols 1)) (set! count_v (+ count_v (nth (nth new_generation_cells new_generation_i) (+ new_generation_j 1))))) (when (and (< new_generation_i (- new_generation_rows 1)) (> new_generation_j 0)) (set! count_v (+ count_v (nth (nth new_generation_cells (+ new_generation_i 1)) (- new_generation_j 1))))) (when (< new_generation_i (- new_generation_rows 1)) (set! count_v (+ count_v (nth (nth new_generation_cells (+ new_generation_i 1)) new_generation_j)))) (when (and (< new_generation_i (- new_generation_rows 1)) (< new_generation_j (- new_generation_cols 1))) (set! count_v (+ count_v (nth (nth new_generation_cells (+ new_generation_i 1)) (+ new_generation_j 1))))) (set! new_generation_alive (= (nth (nth new_generation_cells new_generation_i) new_generation_j) 1)) (if (or (and (and new_generation_alive (>= count_v 2)) (<= count_v 3)) (and (not new_generation_alive) (= count_v 3))) (set! new_generation_row (conj new_generation_row 1)) (set! new_generation_row (conj new_generation_row 0))) (set! new_generation_j (+ new_generation_j 1)))) (set! next_v (conj next_v new_generation_row)) (set! new_generation_i (+ new_generation_i 1)))) (throw (ex-info "return" {:v next_v}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn generate_generations [generate_generations_cells generate_generations_frames]
  (binding [generate_generations_current nil generate_generations_i nil generate_generations_result nil] (try (do (set! generate_generations_result []) (set! generate_generations_i 0) (set! generate_generations_current generate_generations_cells) (while (< generate_generations_i generate_generations_frames) (do (set! generate_generations_result (conj generate_generations_result generate_generations_current)) (set! generate_generations_current (new_generation generate_generations_current)) (set! generate_generations_i (+ generate_generations_i 1)))) (throw (ex-info "return" {:v generate_generations_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_frames nil main_i nil] (do (set! main_frames (generate_generations main_GLIDER 4)) (set! main_i 0) (while (< main_i (count main_frames)) (do (println (nth main_frames main_i)) (set! main_i (+ main_i 1)))))))

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
