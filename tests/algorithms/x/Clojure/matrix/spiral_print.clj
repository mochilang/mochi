(ns main (:refer-clojure :exclude [is_valid_matrix spiral_traversal spiral_print_clockwise main]))

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

(defn mochi_str [v]
  (cond (float? v) (let [s (str v)] (if (clojure.string/ends-with? s ".0") (subs s 0 (- (count s) 2)) s)) :else (str v)))

(defn _fetch [url]
  {:data [{:from "" :intensity {:actual 0 :forecast 0 :index ""} :to ""}]})

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare is_valid_matrix spiral_traversal spiral_print_clockwise main)

(def ^:dynamic is_valid_matrix_cols nil)

(def ^:dynamic is_valid_matrix_row nil)

(def ^:dynamic main_a nil)

(def ^:dynamic spiral_print_clockwise_value nil)

(def ^:dynamic spiral_traversal_bottom nil)

(def ^:dynamic spiral_traversal_cols nil)

(def ^:dynamic spiral_traversal_i nil)

(def ^:dynamic spiral_traversal_left nil)

(def ^:dynamic spiral_traversal_result nil)

(def ^:dynamic spiral_traversal_right nil)

(def ^:dynamic spiral_traversal_rows nil)

(def ^:dynamic spiral_traversal_top nil)

(defn is_valid_matrix [is_valid_matrix_matrix]
  (binding [is_valid_matrix_cols nil is_valid_matrix_row nil] (try (do (when (= (count is_valid_matrix_matrix) 0) (throw (ex-info "return" {:v false}))) (set! is_valid_matrix_cols (count (nth is_valid_matrix_matrix 0))) (doseq [is_valid_matrix_row is_valid_matrix_matrix] (when (not= (count is_valid_matrix_row) is_valid_matrix_cols) (throw (ex-info "return" {:v false})))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn spiral_traversal [spiral_traversal_matrix]
  (binding [spiral_traversal_bottom nil spiral_traversal_cols nil spiral_traversal_i nil spiral_traversal_left nil spiral_traversal_result nil spiral_traversal_right nil spiral_traversal_rows nil spiral_traversal_top nil] (try (do (when (not (is_valid_matrix spiral_traversal_matrix)) (throw (ex-info "return" {:v []}))) (set! spiral_traversal_rows (count spiral_traversal_matrix)) (set! spiral_traversal_cols (count (nth spiral_traversal_matrix 0))) (set! spiral_traversal_top 0) (set! spiral_traversal_bottom (- spiral_traversal_rows 1)) (set! spiral_traversal_left 0) (set! spiral_traversal_right (- spiral_traversal_cols 1)) (set! spiral_traversal_result []) (while (and (<= spiral_traversal_left spiral_traversal_right) (<= spiral_traversal_top spiral_traversal_bottom)) (do (set! spiral_traversal_i spiral_traversal_left) (while (<= spiral_traversal_i spiral_traversal_right) (do (set! spiral_traversal_result (conj spiral_traversal_result (nth (nth spiral_traversal_matrix spiral_traversal_top) spiral_traversal_i))) (set! spiral_traversal_i (+ spiral_traversal_i 1)))) (set! spiral_traversal_top (+ spiral_traversal_top 1)) (set! spiral_traversal_i spiral_traversal_top) (while (<= spiral_traversal_i spiral_traversal_bottom) (do (set! spiral_traversal_result (conj spiral_traversal_result (nth (nth spiral_traversal_matrix spiral_traversal_i) spiral_traversal_right))) (set! spiral_traversal_i (+ spiral_traversal_i 1)))) (set! spiral_traversal_right (- spiral_traversal_right 1)) (when (<= spiral_traversal_top spiral_traversal_bottom) (do (set! spiral_traversal_i spiral_traversal_right) (while (>= spiral_traversal_i spiral_traversal_left) (do (set! spiral_traversal_result (conj spiral_traversal_result (nth (nth spiral_traversal_matrix spiral_traversal_bottom) spiral_traversal_i))) (set! spiral_traversal_i (- spiral_traversal_i 1)))) (set! spiral_traversal_bottom (- spiral_traversal_bottom 1)))) (when (<= spiral_traversal_left spiral_traversal_right) (do (set! spiral_traversal_i spiral_traversal_bottom) (while (>= spiral_traversal_i spiral_traversal_top) (do (set! spiral_traversal_result (conj spiral_traversal_result (nth (nth spiral_traversal_matrix spiral_traversal_i) spiral_traversal_left))) (set! spiral_traversal_i (- spiral_traversal_i 1)))) (set! spiral_traversal_left (+ spiral_traversal_left 1)))))) (throw (ex-info "return" {:v spiral_traversal_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn spiral_print_clockwise [spiral_print_clockwise_matrix]
  (binding [spiral_print_clockwise_value nil] (do (doseq [spiral_print_clockwise_value (spiral_traversal spiral_print_clockwise_matrix)] (println (mochi_str spiral_print_clockwise_value))) spiral_print_clockwise_matrix)))

(defn main []
  (binding [main_a nil] (do (set! main_a [[1 2 3 4] [5 6 7 8] [9 10 11 12]]) (spiral_print_clockwise main_a) (println (mochi_str (spiral_traversal main_a))))))

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
