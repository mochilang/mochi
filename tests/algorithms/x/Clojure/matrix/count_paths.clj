(ns main (:refer-clojure :exclude [depth_first_search count_paths main]))

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
  (Integer/parseInt (str s)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare depth_first_search count_paths main)

(def ^:dynamic count_paths_cols nil)

(def ^:dynamic count_paths_i nil)

(def ^:dynamic count_paths_j nil)

(def ^:dynamic count_paths_row_visit nil)

(def ^:dynamic count_paths_rows nil)

(def ^:dynamic count_paths_visit nil)

(def ^:dynamic count_v nil)

(def ^:dynamic depth_first_search_col_length nil)

(def ^:dynamic depth_first_search_row_length nil)

(def ^:dynamic depth_first_search_visit nil)

(def ^:dynamic main_grid1 nil)

(def ^:dynamic main_grid2 nil)

(defn depth_first_search [depth_first_search_grid depth_first_search_row depth_first_search_col depth_first_search_visit_p]
  (binding [count_v nil depth_first_search_col_length nil depth_first_search_row_length nil depth_first_search_visit nil] (try (do (set! depth_first_search_visit depth_first_search_visit_p) (set! depth_first_search_row_length (count depth_first_search_grid)) (set! depth_first_search_col_length (count (nth depth_first_search_grid 0))) (when (or (or (or (< depth_first_search_row 0) (< depth_first_search_col 0)) (= depth_first_search_row depth_first_search_row_length)) (= depth_first_search_col depth_first_search_col_length)) (throw (ex-info "return" {:v 0}))) (when (nth (nth depth_first_search_visit depth_first_search_row) depth_first_search_col) (throw (ex-info "return" {:v 0}))) (when (= (nth (nth depth_first_search_grid depth_first_search_row) depth_first_search_col) 1) (throw (ex-info "return" {:v 0}))) (when (and (= depth_first_search_row (- depth_first_search_row_length 1)) (= depth_first_search_col (- depth_first_search_col_length 1))) (throw (ex-info "return" {:v 1}))) (set! depth_first_search_visit (assoc-in depth_first_search_visit [depth_first_search_row depth_first_search_col] true)) (set! count_v 0) (set! count_v (+ count_v (depth_first_search depth_first_search_grid (+ depth_first_search_row 1) depth_first_search_col depth_first_search_visit))) (set! count_v (+ count_v (depth_first_search depth_first_search_grid (- depth_first_search_row 1) depth_first_search_col depth_first_search_visit))) (set! count_v (+ count_v (depth_first_search depth_first_search_grid depth_first_search_row (+ depth_first_search_col 1) depth_first_search_visit))) (set! count_v (+ count_v (depth_first_search depth_first_search_grid depth_first_search_row (- depth_first_search_col 1) depth_first_search_visit))) (set! depth_first_search_visit (assoc-in depth_first_search_visit [depth_first_search_row depth_first_search_col] false)) (throw (ex-info "return" {:v count_v}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn count_paths [count_paths_grid]
  (binding [count_paths_cols nil count_paths_i nil count_paths_j nil count_paths_row_visit nil count_paths_rows nil count_paths_visit nil] (try (do (set! count_paths_rows (count count_paths_grid)) (set! count_paths_cols (count (nth count_paths_grid 0))) (set! count_paths_visit []) (set! count_paths_i 0) (while (< count_paths_i count_paths_rows) (do (set! count_paths_row_visit []) (set! count_paths_j 0) (while (< count_paths_j count_paths_cols) (do (set! count_paths_row_visit (conj count_paths_row_visit false)) (set! count_paths_j (+ count_paths_j 1)))) (set! count_paths_visit (conj count_paths_visit count_paths_row_visit)) (set! count_paths_i (+ count_paths_i 1)))) (throw (ex-info "return" {:v (depth_first_search count_paths_grid 0 0 count_paths_visit)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_grid1 nil main_grid2 nil] (do (set! main_grid1 [[0 0 0 0] [1 1 0 0] [0 0 0 1] [0 1 0 0]]) (println (str (count_paths main_grid1))) (set! main_grid2 [[0 0 0 0 0] [0 1 1 1 0] [0 1 1 1 0] [0 0 0 0 0]]) (println (str (count_paths main_grid2))))))

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
