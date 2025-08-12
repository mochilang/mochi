(ns main (:refer-clojure :exclude [update_area_of_max_square largest_square_area_in_matrix_top_down update_area_of_max_square_with_dp largest_square_area_in_matrix_top_down_with_dp largest_square_area_in_matrix_bottom_up largest_square_area_in_matrix_bottom_up_space_optimization]))

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

(defn _fetch [url]
  {:data [{:from "" :intensity {:actual 0 :forecast 0 :index ""} :to ""}]})

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare update_area_of_max_square largest_square_area_in_matrix_top_down update_area_of_max_square_with_dp largest_square_area_in_matrix_top_down_with_dp largest_square_area_in_matrix_bottom_up largest_square_area_in_matrix_bottom_up_space_optimization)

(def ^:dynamic largest_square_area_in_matrix_bottom_up_bottom nil)

(def ^:dynamic largest_square_area_in_matrix_bottom_up_c nil)

(def ^:dynamic largest_square_area_in_matrix_bottom_up_col nil)

(def ^:dynamic largest_square_area_in_matrix_bottom_up_diagonal nil)

(def ^:dynamic largest_square_area_in_matrix_bottom_up_dp_array nil)

(def ^:dynamic largest_square_area_in_matrix_bottom_up_largest nil)

(def ^:dynamic largest_square_area_in_matrix_bottom_up_r nil)

(def ^:dynamic largest_square_area_in_matrix_bottom_up_right nil)

(def ^:dynamic largest_square_area_in_matrix_bottom_up_row nil)

(def ^:dynamic largest_square_area_in_matrix_bottom_up_row_list nil)

(def ^:dynamic largest_square_area_in_matrix_bottom_up_space_optimization_bottom nil)

(def ^:dynamic largest_square_area_in_matrix_bottom_up_space_optimization_col nil)

(def ^:dynamic largest_square_area_in_matrix_bottom_up_space_optimization_current_row nil)

(def ^:dynamic largest_square_area_in_matrix_bottom_up_space_optimization_diagonal nil)

(def ^:dynamic largest_square_area_in_matrix_bottom_up_space_optimization_i nil)

(def ^:dynamic largest_square_area_in_matrix_bottom_up_space_optimization_j nil)

(def ^:dynamic largest_square_area_in_matrix_bottom_up_space_optimization_largest nil)

(def ^:dynamic largest_square_area_in_matrix_bottom_up_space_optimization_next_row nil)

(def ^:dynamic largest_square_area_in_matrix_bottom_up_space_optimization_right nil)

(def ^:dynamic largest_square_area_in_matrix_bottom_up_space_optimization_row nil)

(def ^:dynamic largest_square_area_in_matrix_bottom_up_space_optimization_t nil)

(def ^:dynamic largest_square_area_in_matrix_bottom_up_space_optimization_value nil)

(def ^:dynamic largest_square_area_in_matrix_bottom_up_value nil)

(def ^:dynamic largest_square_area_in_matrix_top_down_largest nil)

(def ^:dynamic largest_square_area_in_matrix_top_down_with_dp_c nil)

(def ^:dynamic largest_square_area_in_matrix_top_down_with_dp_dp_array nil)

(def ^:dynamic largest_square_area_in_matrix_top_down_with_dp_largest nil)

(def ^:dynamic largest_square_area_in_matrix_top_down_with_dp_r nil)

(def ^:dynamic largest_square_area_in_matrix_top_down_with_dp_row_list nil)

(def ^:dynamic update_area_of_max_square_diagonal nil)

(def ^:dynamic update_area_of_max_square_down nil)

(def ^:dynamic update_area_of_max_square_largest_square_area nil)

(def ^:dynamic update_area_of_max_square_right nil)

(def ^:dynamic update_area_of_max_square_sub nil)

(def ^:dynamic update_area_of_max_square_with_dp_diagonal nil)

(def ^:dynamic update_area_of_max_square_with_dp_down nil)

(def ^:dynamic update_area_of_max_square_with_dp_dp_array nil)

(def ^:dynamic update_area_of_max_square_with_dp_largest_square_area nil)

(def ^:dynamic update_area_of_max_square_with_dp_right nil)

(def ^:dynamic update_area_of_max_square_with_dp_sub nil)

(defn update_area_of_max_square [update_area_of_max_square_row update_area_of_max_square_col update_area_of_max_square_rows update_area_of_max_square_cols update_area_of_max_square_mat update_area_of_max_square_largest_square_area_p]
  (binding [update_area_of_max_square_diagonal nil update_area_of_max_square_down nil update_area_of_max_square_largest_square_area nil update_area_of_max_square_right nil update_area_of_max_square_sub nil] (try (do (set! update_area_of_max_square_largest_square_area update_area_of_max_square_largest_square_area_p) (when (or (>= update_area_of_max_square_row update_area_of_max_square_rows) (>= update_area_of_max_square_col update_area_of_max_square_cols)) (throw (ex-info "return" {:v 0}))) (set! update_area_of_max_square_right (update_area_of_max_square update_area_of_max_square_row (+ update_area_of_max_square_col 1) update_area_of_max_square_rows update_area_of_max_square_cols update_area_of_max_square_mat update_area_of_max_square_largest_square_area)) (set! update_area_of_max_square_diagonal (update_area_of_max_square (+ update_area_of_max_square_row 1) (+ update_area_of_max_square_col 1) update_area_of_max_square_rows update_area_of_max_square_cols update_area_of_max_square_mat update_area_of_max_square_largest_square_area)) (set! update_area_of_max_square_down (update_area_of_max_square (+ update_area_of_max_square_row 1) update_area_of_max_square_col update_area_of_max_square_rows update_area_of_max_square_cols update_area_of_max_square_mat update_area_of_max_square_largest_square_area)) (if (= (nth (nth update_area_of_max_square_mat update_area_of_max_square_row) update_area_of_max_square_col) 1) (do (set! update_area_of_max_square_sub (+ 1 (apply min [update_area_of_max_square_right update_area_of_max_square_diagonal update_area_of_max_square_down]))) (when (> update_area_of_max_square_sub (nth update_area_of_max_square_largest_square_area 0)) (set! update_area_of_max_square_largest_square_area (assoc update_area_of_max_square_largest_square_area 0 update_area_of_max_square_sub))) (throw (ex-info "return" {:v update_area_of_max_square_sub}))) (throw (ex-info "return" {:v 0})))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn largest_square_area_in_matrix_top_down [largest_square_area_in_matrix_top_down_rows largest_square_area_in_matrix_top_down_cols largest_square_area_in_matrix_top_down_mat]
  (binding [largest_square_area_in_matrix_top_down_largest nil] (try (do (set! largest_square_area_in_matrix_top_down_largest [0]) (update_area_of_max_square 0 0 largest_square_area_in_matrix_top_down_rows largest_square_area_in_matrix_top_down_cols largest_square_area_in_matrix_top_down_mat largest_square_area_in_matrix_top_down_largest) (throw (ex-info "return" {:v (nth largest_square_area_in_matrix_top_down_largest 0)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn update_area_of_max_square_with_dp [update_area_of_max_square_with_dp_row update_area_of_max_square_with_dp_col update_area_of_max_square_with_dp_rows update_area_of_max_square_with_dp_cols update_area_of_max_square_with_dp_mat update_area_of_max_square_with_dp_dp_array_p update_area_of_max_square_with_dp_largest_square_area_p]
  (binding [update_area_of_max_square_with_dp_diagonal nil update_area_of_max_square_with_dp_down nil update_area_of_max_square_with_dp_dp_array nil update_area_of_max_square_with_dp_largest_square_area nil update_area_of_max_square_with_dp_right nil update_area_of_max_square_with_dp_sub nil] (try (do (set! update_area_of_max_square_with_dp_dp_array update_area_of_max_square_with_dp_dp_array_p) (set! update_area_of_max_square_with_dp_largest_square_area update_area_of_max_square_with_dp_largest_square_area_p) (when (or (>= update_area_of_max_square_with_dp_row update_area_of_max_square_with_dp_rows) (>= update_area_of_max_square_with_dp_col update_area_of_max_square_with_dp_cols)) (throw (ex-info "return" {:v 0}))) (when (not= (nth (nth update_area_of_max_square_with_dp_dp_array update_area_of_max_square_with_dp_row) update_area_of_max_square_with_dp_col) (- 1)) (throw (ex-info "return" {:v (nth (nth update_area_of_max_square_with_dp_dp_array update_area_of_max_square_with_dp_row) update_area_of_max_square_with_dp_col)}))) (set! update_area_of_max_square_with_dp_right (update_area_of_max_square_with_dp update_area_of_max_square_with_dp_row (+ update_area_of_max_square_with_dp_col 1) update_area_of_max_square_with_dp_rows update_area_of_max_square_with_dp_cols update_area_of_max_square_with_dp_mat update_area_of_max_square_with_dp_dp_array update_area_of_max_square_with_dp_largest_square_area)) (set! update_area_of_max_square_with_dp_diagonal (update_area_of_max_square_with_dp (+ update_area_of_max_square_with_dp_row 1) (+ update_area_of_max_square_with_dp_col 1) update_area_of_max_square_with_dp_rows update_area_of_max_square_with_dp_cols update_area_of_max_square_with_dp_mat update_area_of_max_square_with_dp_dp_array update_area_of_max_square_with_dp_largest_square_area)) (set! update_area_of_max_square_with_dp_down (update_area_of_max_square_with_dp (+ update_area_of_max_square_with_dp_row 1) update_area_of_max_square_with_dp_col update_area_of_max_square_with_dp_rows update_area_of_max_square_with_dp_cols update_area_of_max_square_with_dp_mat update_area_of_max_square_with_dp_dp_array update_area_of_max_square_with_dp_largest_square_area)) (if (= (nth (nth update_area_of_max_square_with_dp_mat update_area_of_max_square_with_dp_row) update_area_of_max_square_with_dp_col) 1) (do (set! update_area_of_max_square_with_dp_sub (+ 1 (apply min [update_area_of_max_square_with_dp_right update_area_of_max_square_with_dp_diagonal update_area_of_max_square_with_dp_down]))) (when (> update_area_of_max_square_with_dp_sub (nth update_area_of_max_square_with_dp_largest_square_area 0)) (set! update_area_of_max_square_with_dp_largest_square_area (assoc update_area_of_max_square_with_dp_largest_square_area 0 update_area_of_max_square_with_dp_sub))) (set! update_area_of_max_square_with_dp_dp_array (assoc-in update_area_of_max_square_with_dp_dp_array [update_area_of_max_square_with_dp_row update_area_of_max_square_with_dp_col] update_area_of_max_square_with_dp_sub)) (throw (ex-info "return" {:v update_area_of_max_square_with_dp_sub}))) (do (set! update_area_of_max_square_with_dp_dp_array (assoc-in update_area_of_max_square_with_dp_dp_array [update_area_of_max_square_with_dp_row update_area_of_max_square_with_dp_col] 0)) (throw (ex-info "return" {:v 0}))))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn largest_square_area_in_matrix_top_down_with_dp [largest_square_area_in_matrix_top_down_with_dp_rows largest_square_area_in_matrix_top_down_with_dp_cols largest_square_area_in_matrix_top_down_with_dp_mat]
  (binding [largest_square_area_in_matrix_top_down_with_dp_c nil largest_square_area_in_matrix_top_down_with_dp_dp_array nil largest_square_area_in_matrix_top_down_with_dp_largest nil largest_square_area_in_matrix_top_down_with_dp_r nil largest_square_area_in_matrix_top_down_with_dp_row_list nil] (try (do (set! largest_square_area_in_matrix_top_down_with_dp_largest [0]) (set! largest_square_area_in_matrix_top_down_with_dp_dp_array []) (set! largest_square_area_in_matrix_top_down_with_dp_r 0) (while (< largest_square_area_in_matrix_top_down_with_dp_r largest_square_area_in_matrix_top_down_with_dp_rows) (do (set! largest_square_area_in_matrix_top_down_with_dp_row_list []) (set! largest_square_area_in_matrix_top_down_with_dp_c 0) (while (< largest_square_area_in_matrix_top_down_with_dp_c largest_square_area_in_matrix_top_down_with_dp_cols) (do (set! largest_square_area_in_matrix_top_down_with_dp_row_list (conj largest_square_area_in_matrix_top_down_with_dp_row_list (- 1))) (set! largest_square_area_in_matrix_top_down_with_dp_c (+ largest_square_area_in_matrix_top_down_with_dp_c 1)))) (set! largest_square_area_in_matrix_top_down_with_dp_dp_array (conj largest_square_area_in_matrix_top_down_with_dp_dp_array largest_square_area_in_matrix_top_down_with_dp_row_list)) (set! largest_square_area_in_matrix_top_down_with_dp_r (+ largest_square_area_in_matrix_top_down_with_dp_r 1)))) (update_area_of_max_square_with_dp 0 0 largest_square_area_in_matrix_top_down_with_dp_rows largest_square_area_in_matrix_top_down_with_dp_cols largest_square_area_in_matrix_top_down_with_dp_mat largest_square_area_in_matrix_top_down_with_dp_dp_array largest_square_area_in_matrix_top_down_with_dp_largest) (throw (ex-info "return" {:v (nth largest_square_area_in_matrix_top_down_with_dp_largest 0)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn largest_square_area_in_matrix_bottom_up [largest_square_area_in_matrix_bottom_up_rows largest_square_area_in_matrix_bottom_up_cols largest_square_area_in_matrix_bottom_up_mat]
  (binding [largest_square_area_in_matrix_bottom_up_bottom nil largest_square_area_in_matrix_bottom_up_c nil largest_square_area_in_matrix_bottom_up_col nil largest_square_area_in_matrix_bottom_up_diagonal nil largest_square_area_in_matrix_bottom_up_dp_array nil largest_square_area_in_matrix_bottom_up_largest nil largest_square_area_in_matrix_bottom_up_r nil largest_square_area_in_matrix_bottom_up_right nil largest_square_area_in_matrix_bottom_up_row nil largest_square_area_in_matrix_bottom_up_row_list nil largest_square_area_in_matrix_bottom_up_value nil] (try (do (set! largest_square_area_in_matrix_bottom_up_dp_array []) (set! largest_square_area_in_matrix_bottom_up_r 0) (while (<= largest_square_area_in_matrix_bottom_up_r largest_square_area_in_matrix_bottom_up_rows) (do (set! largest_square_area_in_matrix_bottom_up_row_list []) (set! largest_square_area_in_matrix_bottom_up_c 0) (while (<= largest_square_area_in_matrix_bottom_up_c largest_square_area_in_matrix_bottom_up_cols) (do (set! largest_square_area_in_matrix_bottom_up_row_list (conj largest_square_area_in_matrix_bottom_up_row_list 0)) (set! largest_square_area_in_matrix_bottom_up_c (+ largest_square_area_in_matrix_bottom_up_c 1)))) (set! largest_square_area_in_matrix_bottom_up_dp_array (conj largest_square_area_in_matrix_bottom_up_dp_array largest_square_area_in_matrix_bottom_up_row_list)) (set! largest_square_area_in_matrix_bottom_up_r (+ largest_square_area_in_matrix_bottom_up_r 1)))) (set! largest_square_area_in_matrix_bottom_up_largest 0) (set! largest_square_area_in_matrix_bottom_up_row (- largest_square_area_in_matrix_bottom_up_rows 1)) (while (>= largest_square_area_in_matrix_bottom_up_row 0) (do (set! largest_square_area_in_matrix_bottom_up_col (- largest_square_area_in_matrix_bottom_up_cols 1)) (while (>= largest_square_area_in_matrix_bottom_up_col 0) (do (set! largest_square_area_in_matrix_bottom_up_right (nth (nth largest_square_area_in_matrix_bottom_up_dp_array largest_square_area_in_matrix_bottom_up_row) (+ largest_square_area_in_matrix_bottom_up_col 1))) (set! largest_square_area_in_matrix_bottom_up_diagonal (nth (nth largest_square_area_in_matrix_bottom_up_dp_array (+ largest_square_area_in_matrix_bottom_up_row 1)) (+ largest_square_area_in_matrix_bottom_up_col 1))) (set! largest_square_area_in_matrix_bottom_up_bottom (nth (nth largest_square_area_in_matrix_bottom_up_dp_array (+ largest_square_area_in_matrix_bottom_up_row 1)) largest_square_area_in_matrix_bottom_up_col)) (if (= (nth (nth largest_square_area_in_matrix_bottom_up_mat largest_square_area_in_matrix_bottom_up_row) largest_square_area_in_matrix_bottom_up_col) 1) (do (set! largest_square_area_in_matrix_bottom_up_value (+ 1 (apply min [largest_square_area_in_matrix_bottom_up_right largest_square_area_in_matrix_bottom_up_diagonal largest_square_area_in_matrix_bottom_up_bottom]))) (set! largest_square_area_in_matrix_bottom_up_dp_array (assoc-in largest_square_area_in_matrix_bottom_up_dp_array [largest_square_area_in_matrix_bottom_up_row largest_square_area_in_matrix_bottom_up_col] largest_square_area_in_matrix_bottom_up_value)) (when (> largest_square_area_in_matrix_bottom_up_value largest_square_area_in_matrix_bottom_up_largest) (set! largest_square_area_in_matrix_bottom_up_largest largest_square_area_in_matrix_bottom_up_value))) (set! largest_square_area_in_matrix_bottom_up_dp_array (assoc-in largest_square_area_in_matrix_bottom_up_dp_array [largest_square_area_in_matrix_bottom_up_row largest_square_area_in_matrix_bottom_up_col] 0))) (set! largest_square_area_in_matrix_bottom_up_col (- largest_square_area_in_matrix_bottom_up_col 1)))) (set! largest_square_area_in_matrix_bottom_up_row (- largest_square_area_in_matrix_bottom_up_row 1)))) (throw (ex-info "return" {:v largest_square_area_in_matrix_bottom_up_largest}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn largest_square_area_in_matrix_bottom_up_space_optimization [largest_square_area_in_matrix_bottom_up_space_optimization_rows largest_square_area_in_matrix_bottom_up_space_optimization_cols largest_square_area_in_matrix_bottom_up_space_optimization_mat]
  (binding [largest_square_area_in_matrix_bottom_up_space_optimization_bottom nil largest_square_area_in_matrix_bottom_up_space_optimization_col nil largest_square_area_in_matrix_bottom_up_space_optimization_current_row nil largest_square_area_in_matrix_bottom_up_space_optimization_diagonal nil largest_square_area_in_matrix_bottom_up_space_optimization_i nil largest_square_area_in_matrix_bottom_up_space_optimization_j nil largest_square_area_in_matrix_bottom_up_space_optimization_largest nil largest_square_area_in_matrix_bottom_up_space_optimization_next_row nil largest_square_area_in_matrix_bottom_up_space_optimization_right nil largest_square_area_in_matrix_bottom_up_space_optimization_row nil largest_square_area_in_matrix_bottom_up_space_optimization_t nil largest_square_area_in_matrix_bottom_up_space_optimization_value nil] (try (do (set! largest_square_area_in_matrix_bottom_up_space_optimization_current_row []) (set! largest_square_area_in_matrix_bottom_up_space_optimization_i 0) (while (<= largest_square_area_in_matrix_bottom_up_space_optimization_i largest_square_area_in_matrix_bottom_up_space_optimization_cols) (do (set! largest_square_area_in_matrix_bottom_up_space_optimization_current_row (conj largest_square_area_in_matrix_bottom_up_space_optimization_current_row 0)) (set! largest_square_area_in_matrix_bottom_up_space_optimization_i (+ largest_square_area_in_matrix_bottom_up_space_optimization_i 1)))) (set! largest_square_area_in_matrix_bottom_up_space_optimization_next_row []) (set! largest_square_area_in_matrix_bottom_up_space_optimization_j 0) (while (<= largest_square_area_in_matrix_bottom_up_space_optimization_j largest_square_area_in_matrix_bottom_up_space_optimization_cols) (do (set! largest_square_area_in_matrix_bottom_up_space_optimization_next_row (conj largest_square_area_in_matrix_bottom_up_space_optimization_next_row 0)) (set! largest_square_area_in_matrix_bottom_up_space_optimization_j (+ largest_square_area_in_matrix_bottom_up_space_optimization_j 1)))) (set! largest_square_area_in_matrix_bottom_up_space_optimization_largest 0) (set! largest_square_area_in_matrix_bottom_up_space_optimization_row (- largest_square_area_in_matrix_bottom_up_space_optimization_rows 1)) (while (>= largest_square_area_in_matrix_bottom_up_space_optimization_row 0) (do (set! largest_square_area_in_matrix_bottom_up_space_optimization_col (- largest_square_area_in_matrix_bottom_up_space_optimization_cols 1)) (while (>= largest_square_area_in_matrix_bottom_up_space_optimization_col 0) (do (set! largest_square_area_in_matrix_bottom_up_space_optimization_right (nth largest_square_area_in_matrix_bottom_up_space_optimization_current_row (+ largest_square_area_in_matrix_bottom_up_space_optimization_col 1))) (set! largest_square_area_in_matrix_bottom_up_space_optimization_diagonal (nth largest_square_area_in_matrix_bottom_up_space_optimization_next_row (+ largest_square_area_in_matrix_bottom_up_space_optimization_col 1))) (set! largest_square_area_in_matrix_bottom_up_space_optimization_bottom (nth largest_square_area_in_matrix_bottom_up_space_optimization_next_row largest_square_area_in_matrix_bottom_up_space_optimization_col)) (if (= (nth (nth largest_square_area_in_matrix_bottom_up_space_optimization_mat largest_square_area_in_matrix_bottom_up_space_optimization_row) largest_square_area_in_matrix_bottom_up_space_optimization_col) 1) (do (set! largest_square_area_in_matrix_bottom_up_space_optimization_value (+ 1 (apply min [largest_square_area_in_matrix_bottom_up_space_optimization_right largest_square_area_in_matrix_bottom_up_space_optimization_diagonal largest_square_area_in_matrix_bottom_up_space_optimization_bottom]))) (set! largest_square_area_in_matrix_bottom_up_space_optimization_current_row (assoc largest_square_area_in_matrix_bottom_up_space_optimization_current_row largest_square_area_in_matrix_bottom_up_space_optimization_col largest_square_area_in_matrix_bottom_up_space_optimization_value)) (when (> largest_square_area_in_matrix_bottom_up_space_optimization_value largest_square_area_in_matrix_bottom_up_space_optimization_largest) (set! largest_square_area_in_matrix_bottom_up_space_optimization_largest largest_square_area_in_matrix_bottom_up_space_optimization_value))) (set! largest_square_area_in_matrix_bottom_up_space_optimization_current_row (assoc largest_square_area_in_matrix_bottom_up_space_optimization_current_row largest_square_area_in_matrix_bottom_up_space_optimization_col 0))) (set! largest_square_area_in_matrix_bottom_up_space_optimization_col (- largest_square_area_in_matrix_bottom_up_space_optimization_col 1)))) (set! largest_square_area_in_matrix_bottom_up_space_optimization_next_row largest_square_area_in_matrix_bottom_up_space_optimization_current_row) (set! largest_square_area_in_matrix_bottom_up_space_optimization_current_row []) (set! largest_square_area_in_matrix_bottom_up_space_optimization_t 0) (while (<= largest_square_area_in_matrix_bottom_up_space_optimization_t largest_square_area_in_matrix_bottom_up_space_optimization_cols) (do (set! largest_square_area_in_matrix_bottom_up_space_optimization_current_row (conj largest_square_area_in_matrix_bottom_up_space_optimization_current_row 0)) (set! largest_square_area_in_matrix_bottom_up_space_optimization_t (+ largest_square_area_in_matrix_bottom_up_space_optimization_t 1)))) (set! largest_square_area_in_matrix_bottom_up_space_optimization_row (- largest_square_area_in_matrix_bottom_up_space_optimization_row 1)))) (throw (ex-info "return" {:v largest_square_area_in_matrix_bottom_up_space_optimization_largest}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_sample [[1 1] [1 1]])

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (largest_square_area_in_matrix_top_down 2 2 main_sample))
      (println (largest_square_area_in_matrix_top_down_with_dp 2 2 main_sample))
      (println (largest_square_area_in_matrix_bottom_up 2 2 main_sample))
      (println (largest_square_area_in_matrix_bottom_up_space_optimization 2 2 main_sample))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
