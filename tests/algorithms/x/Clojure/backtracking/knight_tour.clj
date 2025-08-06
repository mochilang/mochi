(ns main (:refer-clojure :exclude [get_valid_pos is_complete open_knight_tour_helper open_knight_tour]))

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

(declare get_valid_pos is_complete open_knight_tour_helper open_knight_tour)

(def ^:dynamic get_valid_pos_inner nil)

(def ^:dynamic get_valid_pos_permissible nil)

(def ^:dynamic get_valid_pos_positions nil)

(def ^:dynamic get_valid_pos_x nil)

(def ^:dynamic get_valid_pos_x_test nil)

(def ^:dynamic get_valid_pos_y nil)

(def ^:dynamic get_valid_pos_y_test nil)

(def ^:dynamic is_complete_row nil)

(def ^:dynamic open_knight_tour_board nil)

(def ^:dynamic open_knight_tour_helper_board nil)

(def ^:dynamic open_knight_tour_helper_moves nil)

(def ^:dynamic open_knight_tour_helper_position nil)

(def ^:dynamic open_knight_tour_helper_x nil)

(def ^:dynamic open_knight_tour_helper_y nil)

(def ^:dynamic open_knight_tour_row nil)

(defn get_valid_pos [get_valid_pos_position get_valid_pos_n]
  (binding [get_valid_pos_inner nil get_valid_pos_permissible nil get_valid_pos_positions nil get_valid_pos_x nil get_valid_pos_x_test nil get_valid_pos_y nil get_valid_pos_y_test nil] (try (do (set! get_valid_pos_y (nth get_valid_pos_position 0)) (set! get_valid_pos_x (nth get_valid_pos_position 1)) (set! get_valid_pos_positions [[(+ get_valid_pos_y 1) (+ get_valid_pos_x 2)] [(- get_valid_pos_y 1) (+ get_valid_pos_x 2)] [(+ get_valid_pos_y 1) (- get_valid_pos_x 2)] [(- get_valid_pos_y 1) (- get_valid_pos_x 2)] [(+ get_valid_pos_y 2) (+ get_valid_pos_x 1)] [(+ get_valid_pos_y 2) (- get_valid_pos_x 1)] [(- get_valid_pos_y 2) (+ get_valid_pos_x 1)] [(- get_valid_pos_y 2) (- get_valid_pos_x 1)]]) (set! get_valid_pos_permissible []) (dotimes [idx (count get_valid_pos_positions)] (do (set! get_valid_pos_inner (nth get_valid_pos_positions idx)) (set! get_valid_pos_y_test (nth get_valid_pos_inner 0)) (set! get_valid_pos_x_test (nth get_valid_pos_inner 1)) (when (and (and (and (>= get_valid_pos_y_test 0) (< get_valid_pos_y_test get_valid_pos_n)) (>= get_valid_pos_x_test 0)) (< get_valid_pos_x_test get_valid_pos_n)) (set! get_valid_pos_permissible (conj get_valid_pos_permissible get_valid_pos_inner))))) (throw (ex-info "return" {:v get_valid_pos_permissible}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn is_complete [is_complete_board]
  (binding [is_complete_row nil] (try (do (dotimes [i (count is_complete_board)] (do (set! is_complete_row (nth is_complete_board i)) (dotimes [j (count is_complete_row)] (when (= (nth is_complete_row j) 0) (throw (ex-info "return" {:v false})))))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn open_knight_tour_helper [open_knight_tour_helper_board_p open_knight_tour_helper_pos open_knight_tour_helper_curr]
  (binding [open_knight_tour_helper_board nil open_knight_tour_helper_moves nil open_knight_tour_helper_position nil open_knight_tour_helper_x nil open_knight_tour_helper_y nil] (try (do (set! open_knight_tour_helper_board open_knight_tour_helper_board_p) (when (is_complete open_knight_tour_helper_board) (throw (ex-info "return" {:v true}))) (set! open_knight_tour_helper_moves (get_valid_pos open_knight_tour_helper_pos (count open_knight_tour_helper_board))) (dotimes [i (count open_knight_tour_helper_moves)] (do (set! open_knight_tour_helper_position (nth open_knight_tour_helper_moves i)) (set! open_knight_tour_helper_y (nth open_knight_tour_helper_position 0)) (set! open_knight_tour_helper_x (nth open_knight_tour_helper_position 1)) (when (= (nth (nth open_knight_tour_helper_board open_knight_tour_helper_y) open_knight_tour_helper_x) 0) (do (set! open_knight_tour_helper_board (assoc-in open_knight_tour_helper_board [open_knight_tour_helper_y open_knight_tour_helper_x] (+ open_knight_tour_helper_curr 1))) (when (open_knight_tour_helper open_knight_tour_helper_board open_knight_tour_helper_position (+ open_knight_tour_helper_curr 1)) (throw (ex-info "return" {:v true}))) (set! open_knight_tour_helper_board (assoc-in open_knight_tour_helper_board [open_knight_tour_helper_y open_knight_tour_helper_x] 0)))))) (throw (ex-info "return" {:v false}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn open_knight_tour [open_knight_tour_n]
  (binding [open_knight_tour_board nil open_knight_tour_row nil] (try (do (set! open_knight_tour_board []) (dotimes [i open_knight_tour_n] (do (set! open_knight_tour_row []) (dotimes [j open_knight_tour_n] (set! open_knight_tour_row (conj open_knight_tour_row 0))) (set! open_knight_tour_board (conj open_knight_tour_board open_knight_tour_row)))) (dotimes [i open_knight_tour_n] (dotimes [j open_knight_tour_n] (do (set! open_knight_tour_board (assoc-in open_knight_tour_board [i j] 1)) (when (open_knight_tour_helper open_knight_tour_board [i j] 1) (throw (ex-info "return" {:v open_knight_tour_board}))) (set! open_knight_tour_board (assoc-in open_knight_tour_board [i j] 0))))) (println (str "Open Knight Tour cannot be performed on a board of size " (str open_knight_tour_n))) (throw (ex-info "return" {:v open_knight_tour_board}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_board (open_knight_tour 1))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (nth (nth main_board 0) 0))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
