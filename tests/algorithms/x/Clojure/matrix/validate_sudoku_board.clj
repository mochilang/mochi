(ns main (:refer-clojure :exclude [is_valid_sudoku_board]))

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

(declare is_valid_sudoku_board)

(def ^:dynamic is_valid_sudoku_board_box nil)

(def ^:dynamic is_valid_sudoku_board_boxes nil)

(def ^:dynamic is_valid_sudoku_board_c nil)

(def ^:dynamic is_valid_sudoku_board_cols nil)

(def ^:dynamic is_valid_sudoku_board_i nil)

(def ^:dynamic is_valid_sudoku_board_r nil)

(def ^:dynamic is_valid_sudoku_board_rows nil)

(def ^:dynamic is_valid_sudoku_board_value nil)

(def ^:dynamic main_NUM_SQUARES nil)

(def ^:dynamic main_EMPTY_CELL nil)

(defn is_valid_sudoku_board [is_valid_sudoku_board_board]
  (binding [is_valid_sudoku_board_box nil is_valid_sudoku_board_boxes nil is_valid_sudoku_board_c nil is_valid_sudoku_board_cols nil is_valid_sudoku_board_i nil is_valid_sudoku_board_r nil is_valid_sudoku_board_rows nil is_valid_sudoku_board_value nil] (try (do (when (not= (count is_valid_sudoku_board_board) main_NUM_SQUARES) (throw (ex-info "return" {:v false}))) (set! is_valid_sudoku_board_i 0) (while (< is_valid_sudoku_board_i main_NUM_SQUARES) (do (when (not= (count (nth is_valid_sudoku_board_board is_valid_sudoku_board_i)) main_NUM_SQUARES) (throw (ex-info "return" {:v false}))) (set! is_valid_sudoku_board_i (+ is_valid_sudoku_board_i 1)))) (set! is_valid_sudoku_board_rows []) (set! is_valid_sudoku_board_cols []) (set! is_valid_sudoku_board_boxes []) (set! is_valid_sudoku_board_i 0) (while (< is_valid_sudoku_board_i main_NUM_SQUARES) (do (set! is_valid_sudoku_board_rows (conj is_valid_sudoku_board_rows [])) (set! is_valid_sudoku_board_cols (conj is_valid_sudoku_board_cols [])) (set! is_valid_sudoku_board_boxes (conj is_valid_sudoku_board_boxes [])) (set! is_valid_sudoku_board_i (+ is_valid_sudoku_board_i 1)))) (loop [is_valid_sudoku_board_r_seq main_NUM_SQUARES] (when (seq is_valid_sudoku_board_r_seq) (let [is_valid_sudoku_board_r (first is_valid_sudoku_board_r_seq)] (do (loop [is_valid_sudoku_board_c_seq main_NUM_SQUARES] (when (seq is_valid_sudoku_board_c_seq) (let [is_valid_sudoku_board_c (first is_valid_sudoku_board_c_seq)] (do (set! is_valid_sudoku_board_value (nth (nth is_valid_sudoku_board_board is_valid_sudoku_board_r) is_valid_sudoku_board_c)) (cond (= is_valid_sudoku_board_value main_EMPTY_CELL) (recur (rest is_valid_sudoku_board_c_seq)) :else (do (set! is_valid_sudoku_board_box (+ (* (toi (quot is_valid_sudoku_board_r 3)) 3) (toi (quot is_valid_sudoku_board_c 3)))) (when (or (or (in is_valid_sudoku_board_value (nth is_valid_sudoku_board_rows is_valid_sudoku_board_r)) (in is_valid_sudoku_board_value (nth is_valid_sudoku_board_cols is_valid_sudoku_board_c))) (in is_valid_sudoku_board_value (nth is_valid_sudoku_board_boxes is_valid_sudoku_board_box))) (throw (ex-info "return" {:v false}))) (set! is_valid_sudoku_board_rows (assoc is_valid_sudoku_board_rows is_valid_sudoku_board_r (conj (nth is_valid_sudoku_board_rows is_valid_sudoku_board_r) is_valid_sudoku_board_value))) (set! is_valid_sudoku_board_cols (assoc is_valid_sudoku_board_cols is_valid_sudoku_board_c (conj (nth is_valid_sudoku_board_cols is_valid_sudoku_board_c) is_valid_sudoku_board_value))) (set! is_valid_sudoku_board_boxes (assoc is_valid_sudoku_board_boxes is_valid_sudoku_board_box (conj (nth is_valid_sudoku_board_boxes is_valid_sudoku_board_box) is_valid_sudoku_board_value))) (recur (rest is_valid_sudoku_board_c_seq)))))))) (cond :else (recur (rest is_valid_sudoku_board_r_seq))))))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_valid_board nil)

(def ^:dynamic main_invalid_board nil)

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (alter-var-root (var main_NUM_SQUARES) (constantly 9))
      (alter-var-root (var main_EMPTY_CELL) (constantly "."))
      (alter-var-root (var main_valid_board) (constantly [["5" "3" "." "." "7" "." "." "." "."] ["6" "." "." "1" "9" "5" "." "." "."] ["." "9" "8" "." "." "." "." "6" "."] ["8" "." "." "." "6" "." "." "." "3"] ["4" "." "." "8" "." "3" "." "." "1"] ["7" "." "." "." "2" "." "." "." "6"] ["." "6" "." "." "." "." "2" "8" "."] ["." "." "." "4" "1" "9" "." "." "5"] ["." "." "." "." "8" "." "." "7" "9"]]))
      (alter-var-root (var main_invalid_board) (constantly [["8" "3" "." "." "7" "." "." "." "."] ["6" "." "." "1" "9" "5" "." "." "."] ["." "9" "8" "." "." "." "." "6" "."] ["8" "." "." "." "6" "." "." "." "3"] ["4" "." "." "8" "." "3" "." "." "1"] ["7" "." "." "." "2" "." "." "." "6"] ["." "6" "." "." "." "." "2" "8" "."] ["." "." "." "4" "1" "9" "." "." "5"] ["." "." "." "." "8" "." "." "7" "9"]]))
      (println (is_valid_sudoku_board main_valid_board))
      (println (is_valid_sudoku_board main_invalid_board))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
