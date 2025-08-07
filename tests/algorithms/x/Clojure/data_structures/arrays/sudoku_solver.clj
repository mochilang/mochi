(ns main (:refer-clojure :exclude [string_to_grid print_grid is_safe find_empty solve]))

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

(declare string_to_grid print_grid is_safe find_empty solve)

(def ^:dynamic print_grid_line nil)

(def ^:dynamic solve_column nil)

(def ^:dynamic solve_grid nil)

(def ^:dynamic solve_loc nil)

(def ^:dynamic solve_row nil)

(def ^:dynamic string_to_grid_ch nil)

(def ^:dynamic string_to_grid_grid nil)

(def ^:dynamic string_to_grid_i nil)

(def ^:dynamic string_to_grid_j nil)

(def ^:dynamic string_to_grid_row nil)

(def ^:dynamic string_to_grid_val nil)

(defn string_to_grid [string_to_grid_s]
  (binding [string_to_grid_ch nil string_to_grid_grid nil string_to_grid_i nil string_to_grid_j nil string_to_grid_row nil string_to_grid_val nil] (try (do (set! string_to_grid_grid []) (set! string_to_grid_i 0) (while (< string_to_grid_i 9) (do (set! string_to_grid_row []) (set! string_to_grid_j 0) (while (< string_to_grid_j 9) (do (set! string_to_grid_ch (subs string_to_grid_s (+ (* string_to_grid_i 9) string_to_grid_j) (min (+ (+ (* string_to_grid_i 9) string_to_grid_j) 1) (count string_to_grid_s)))) (set! string_to_grid_val 0) (when (and (not= string_to_grid_ch "0") (not= string_to_grid_ch ".")) (set! string_to_grid_val (Integer/parseInt string_to_grid_ch))) (set! string_to_grid_row (conj string_to_grid_row string_to_grid_val)) (set! string_to_grid_j (+ string_to_grid_j 1)))) (set! string_to_grid_grid (conj string_to_grid_grid string_to_grid_row)) (set! string_to_grid_i (+ string_to_grid_i 1)))) (throw (ex-info "return" {:v string_to_grid_grid}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn print_grid [print_grid_grid]
  (binding [print_grid_line nil] (dotimes [r 9] (do (set! print_grid_line "") (dotimes [c 9] (do (set! print_grid_line (str print_grid_line (str (nth (nth print_grid_grid r) c)))) (when (< c 8) (set! print_grid_line (str print_grid_line " "))))) (println print_grid_line)))))

(defn is_safe [is_safe_grid is_safe_row is_safe_column is_safe_n]
  (try (do (dotimes [i 9] (when (or (= (nth (nth is_safe_grid is_safe_row) i) is_safe_n) (= (nth (nth is_safe_grid i) is_safe_column) is_safe_n)) (throw (ex-info "return" {:v false})))) (dotimes [i 3] (dotimes [j 3] (when (= (nth (nth is_safe_grid (+ (- is_safe_row (mod is_safe_row 3)) i)) (+ (- is_safe_column (mod is_safe_column 3)) j)) is_safe_n) (throw (ex-info "return" {:v false}))))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn find_empty [find_empty_grid]
  (try (do (dotimes [i 9] (dotimes [j 9] (when (= (nth (nth find_empty_grid i) j) 0) (throw (ex-info "return" {:v [i j]}))))) (throw (ex-info "return" {:v []}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn solve [solve_grid_p]
  (binding [solve_column nil solve_grid nil solve_loc nil solve_row nil] (try (do (set! solve_grid solve_grid_p) (set! solve_loc (find_empty solve_grid)) (when (= (count solve_loc) 0) (throw (ex-info "return" {:v true}))) (set! solve_row (nth solve_loc 0)) (set! solve_column (nth solve_loc 1)) (doseq [digit (range 1 10)] (when (is_safe solve_grid solve_row solve_column digit) (do (set! solve_grid (assoc-in solve_grid [solve_row solve_column] digit)) (when (solve solve_grid) (throw (ex-info "return" {:v true}))) (set! solve_grid (assoc-in solve_grid [solve_row solve_column] 0))))) (throw (ex-info "return" {:v false}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_puzzle "003020600900305001001806400008102900700000008006708200002609500800203009005010300")

(def ^:dynamic main_grid (string_to_grid main_puzzle))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println "Original grid:")
      (print_grid main_grid)
      (if (solve main_grid) (do (println "\nSolved grid:") (print_grid main_grid)) (println "\nNo solution found"))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
