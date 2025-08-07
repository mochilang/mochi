(ns main (:refer-clojure :exclude [contains repeat build_board depth_first_search n_queens_solution]))

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

(declare contains repeat build_board depth_first_search n_queens_solution)

(def ^:dynamic build_board_board nil)

(def ^:dynamic build_board_col nil)

(def ^:dynamic build_board_i nil)

(def ^:dynamic build_board_line nil)

(def ^:dynamic contains_i nil)

(def ^:dynamic depth_first_search_boards nil)

(def ^:dynamic depth_first_search_col nil)

(def ^:dynamic depth_first_search_result nil)

(def ^:dynamic depth_first_search_row nil)

(def ^:dynamic depth_first_search_single nil)

(def ^:dynamic n_queens_solution_boards nil)

(def ^:dynamic n_queens_solution_i nil)

(def ^:dynamic n_queens_solution_j nil)

(def ^:dynamic repeat_i nil)

(def ^:dynamic repeat_result nil)

(defn contains [contains_xs contains_x]
  (binding [contains_i nil] (try (do (set! contains_i 0) (while (< contains_i (count contains_xs)) (do (when (= (nth contains_xs contains_i) contains_x) (throw (ex-info "return" {:v true}))) (set! contains_i (+ contains_i 1)))) (throw (ex-info "return" {:v false}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn repeat [repeat_s repeat_times]
  (binding [repeat_i nil repeat_result nil] (try (do (set! repeat_result "") (set! repeat_i 0) (while (< repeat_i repeat_times) (do (set! repeat_result (str repeat_result repeat_s)) (set! repeat_i (+ repeat_i 1)))) (throw (ex-info "return" {:v repeat_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn build_board [build_board_pos build_board_n]
  (binding [build_board_board nil build_board_col nil build_board_i nil build_board_line nil] (try (do (set! build_board_board []) (set! build_board_i 0) (while (< build_board_i (count build_board_pos)) (do (set! build_board_col (nth build_board_pos build_board_i)) (set! build_board_line (str (str (repeat ". " build_board_col) "Q ") (repeat ". " (- (- build_board_n 1) build_board_col)))) (set! build_board_board (conj build_board_board build_board_line)) (set! build_board_i (+ build_board_i 1)))) (throw (ex-info "return" {:v build_board_board}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn depth_first_search [depth_first_search_pos depth_first_search_dr depth_first_search_dl depth_first_search_n]
  (binding [depth_first_search_boards nil depth_first_search_col nil depth_first_search_result nil depth_first_search_row nil depth_first_search_single nil] (try (do (set! depth_first_search_row (count depth_first_search_pos)) (when (= depth_first_search_row depth_first_search_n) (do (set! depth_first_search_single []) (set! depth_first_search_single (conj depth_first_search_single (build_board depth_first_search_pos depth_first_search_n))) (throw (ex-info "return" {:v depth_first_search_single})))) (set! depth_first_search_boards []) (set! depth_first_search_col 0) (loop [while_flag_1 true] (when (and while_flag_1 (< depth_first_search_col depth_first_search_n)) (cond (or (or (contains depth_first_search_pos depth_first_search_col) (contains depth_first_search_dr (- depth_first_search_row depth_first_search_col))) (contains depth_first_search_dl (+ depth_first_search_row depth_first_search_col))) (do (set! depth_first_search_col (+ depth_first_search_col 1)) (recur true)) :else (do (set! depth_first_search_result (depth_first_search (conj depth_first_search_pos depth_first_search_col) (conj depth_first_search_dr (- depth_first_search_row depth_first_search_col)) (conj depth_first_search_dl (+ depth_first_search_row depth_first_search_col)) depth_first_search_n)) (set! depth_first_search_boards (concat depth_first_search_boards depth_first_search_result)) (set! depth_first_search_col (+ depth_first_search_col 1)) (recur while_flag_1))))) (throw (ex-info "return" {:v depth_first_search_boards}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn n_queens_solution [n_queens_solution_n]
  (binding [n_queens_solution_boards nil n_queens_solution_i nil n_queens_solution_j nil] (try (do (set! n_queens_solution_boards (depth_first_search [] [] [] n_queens_solution_n)) (set! n_queens_solution_i 0) (while (< n_queens_solution_i (count n_queens_solution_boards)) (do (set! n_queens_solution_j 0) (while (< n_queens_solution_j (count (nth n_queens_solution_boards n_queens_solution_i))) (do (println (nth (nth n_queens_solution_boards n_queens_solution_i) n_queens_solution_j)) (set! n_queens_solution_j (+ n_queens_solution_j 1)))) (println "") (set! n_queens_solution_i (+ n_queens_solution_i 1)))) (println (count n_queens_solution_boards) "solutions were found.") (throw (ex-info "return" {:v (count n_queens_solution_boards)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (n_queens_solution 4)
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
