(ns main (:refer-clojure :exclude [is_alnum to_int split parse_moves validate_matrix_size validate_matrix_content validate_moves contains find_repeat increment_score move_x move_y play build_matrix process_game main]))

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

(declare is_alnum to_int split parse_moves validate_matrix_size validate_matrix_content validate_moves contains find_repeat increment_score move_x move_y play build_matrix process_game main)

(def ^:dynamic build_matrix_i nil)

(def ^:dynamic build_matrix_j nil)

(def ^:dynamic build_matrix_res nil)

(def ^:dynamic build_matrix_row nil)

(def ^:dynamic build_matrix_row_list nil)

(def ^:dynamic contains_i nil)

(def ^:dynamic contains_p nil)

(def ^:dynamic find_repeat_color nil)

(def ^:dynamic find_repeat_column nil)

(def ^:dynamic find_repeat_idx nil)

(def ^:dynamic find_repeat_pos nil)

(def ^:dynamic find_repeat_repeated nil)

(def ^:dynamic find_repeat_stack nil)

(def ^:dynamic find_repeat_visited nil)

(def ^:dynamic main_matrix nil)

(def ^:dynamic main_moves nil)

(def ^:dynamic main_score nil)

(def ^:dynamic main_size nil)

(def ^:dynamic move_x_matrix_g nil)

(def ^:dynamic move_x_new_list nil)

(def ^:dynamic move_x_row nil)

(def ^:dynamic move_x_val nil)

(def ^:dynamic move_y_all_empty nil)

(def ^:dynamic move_y_c nil)

(def ^:dynamic move_y_col nil)

(def ^:dynamic move_y_column nil)

(def ^:dynamic move_y_empty_cols nil)

(def ^:dynamic move_y_i nil)

(def ^:dynamic move_y_matrix_g nil)

(def ^:dynamic move_y_r nil)

(def ^:dynamic move_y_row nil)

(def ^:dynamic parse_moves_ch nil)

(def ^:dynamic parse_moves_i nil)

(def ^:dynamic parse_moves_j nil)

(def ^:dynamic parse_moves_moves nil)

(def ^:dynamic parse_moves_num nil)

(def ^:dynamic parse_moves_numbers nil)

(def ^:dynamic parse_moves_pair nil)

(def ^:dynamic parse_moves_pairs nil)

(def ^:dynamic parse_moves_x nil)

(def ^:dynamic parse_moves_y nil)

(def ^:dynamic play_column nil)

(def ^:dynamic play_i nil)

(def ^:dynamic play_matrix_g nil)

(def ^:dynamic play_p nil)

(def ^:dynamic play_same_colors nil)

(def ^:dynamic play_sc nil)

(def ^:dynamic process_game_game_matrix nil)

(def ^:dynamic process_game_i nil)

(def ^:dynamic process_game_mv nil)

(def ^:dynamic process_game_res nil)

(def ^:dynamic process_game_total nil)

(def ^:dynamic split_ch nil)

(def ^:dynamic split_current nil)

(def ^:dynamic split_i nil)

(def ^:dynamic split_res nil)

(def ^:dynamic to_int_i nil)

(def ^:dynamic to_int_res nil)

(def ^:dynamic validate_matrix_content_ch nil)

(def ^:dynamic validate_matrix_content_i nil)

(def ^:dynamic validate_matrix_content_j nil)

(def ^:dynamic validate_matrix_content_row nil)

(def ^:dynamic validate_moves_i nil)

(def ^:dynamic validate_moves_mv nil)

(defn is_alnum [is_alnum_ch]
  (try (throw (ex-info "return" {:v (or (or (and (>= (compare is_alnum_ch "0") 0) (<= (compare is_alnum_ch "9") 0)) (and (>= (compare is_alnum_ch "A") 0) (<= (compare is_alnum_ch "Z") 0))) (and (>= (compare is_alnum_ch "a") 0) (<= (compare is_alnum_ch "z") 0)))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn to_int [to_int_token]
  (binding [to_int_i nil to_int_res nil] (try (do (set! to_int_res 0) (set! to_int_i 0) (while (< to_int_i (count to_int_token)) (do (set! to_int_res (+ (* to_int_res 10) (Long/parseLong (subs to_int_token to_int_i (min (+ to_int_i 1) (count to_int_token)))))) (set! to_int_i (+ to_int_i 1)))) (throw (ex-info "return" {:v to_int_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn split [split_s split_sep]
  (binding [split_ch nil split_current nil split_i nil split_res nil] (try (do (set! split_res []) (set! split_current "") (set! split_i 0) (while (< split_i (count split_s)) (do (set! split_ch (subs split_s split_i (min (+ split_i 1) (count split_s)))) (if (= split_ch split_sep) (do (set! split_res (conj split_res split_current)) (set! split_current "")) (set! split_current (str split_current split_ch))) (set! split_i (+ split_i 1)))) (set! split_res (conj split_res split_current)) (throw (ex-info "return" {:v split_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn parse_moves [parse_moves_input_str]
  (binding [parse_moves_ch nil parse_moves_i nil parse_moves_j nil parse_moves_moves nil parse_moves_num nil parse_moves_numbers nil parse_moves_pair nil parse_moves_pairs nil parse_moves_x nil parse_moves_y nil] (try (do (set! parse_moves_pairs (split parse_moves_input_str ",")) (set! parse_moves_moves []) (set! parse_moves_i 0) (while (< parse_moves_i (count parse_moves_pairs)) (do (set! parse_moves_pair (nth parse_moves_pairs parse_moves_i)) (set! parse_moves_numbers []) (set! parse_moves_num "") (set! parse_moves_j 0) (while (< parse_moves_j (count parse_moves_pair)) (do (set! parse_moves_ch (subs parse_moves_pair parse_moves_j (min (+ parse_moves_j 1) (count parse_moves_pair)))) (if (= parse_moves_ch " ") (when (not= parse_moves_num "") (do (set! parse_moves_numbers (conj parse_moves_numbers parse_moves_num)) (set! parse_moves_num ""))) (set! parse_moves_num (str parse_moves_num parse_moves_ch))) (set! parse_moves_j (+ parse_moves_j 1)))) (when (not= parse_moves_num "") (set! parse_moves_numbers (conj parse_moves_numbers parse_moves_num))) (when (not= (count parse_moves_numbers) 2) (throw (Exception. "Each move must have exactly two numbers."))) (set! parse_moves_x (to_int (nth parse_moves_numbers 0))) (set! parse_moves_y (to_int (nth parse_moves_numbers 1))) (set! parse_moves_moves (conj parse_moves_moves {:x parse_moves_x :y parse_moves_y})) (set! parse_moves_i (+ parse_moves_i 1)))) (throw (ex-info "return" {:v parse_moves_moves}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn validate_matrix_size [validate_matrix_size_size]
  (do (when (<= validate_matrix_size_size 0) (throw (Exception. "Matrix size must be a positive integer."))) validate_matrix_size_size))

(defn validate_matrix_content [validate_matrix_content_matrix validate_matrix_content_size]
  (binding [validate_matrix_content_ch nil validate_matrix_content_i nil validate_matrix_content_j nil validate_matrix_content_row nil] (do (when (not= (count validate_matrix_content_matrix) validate_matrix_content_size) (throw (Exception. "The matrix dont match with size."))) (set! validate_matrix_content_i 0) (while (< validate_matrix_content_i validate_matrix_content_size) (do (set! validate_matrix_content_row (nth validate_matrix_content_matrix validate_matrix_content_i)) (when (not= (count validate_matrix_content_row) validate_matrix_content_size) (throw (Exception. (str (str "Each row in the matrix must have exactly " (mochi_str validate_matrix_content_size)) " characters.")))) (set! validate_matrix_content_j 0) (while (< validate_matrix_content_j validate_matrix_content_size) (do (set! validate_matrix_content_ch (subs validate_matrix_content_row validate_matrix_content_j (min (+ validate_matrix_content_j 1) (count validate_matrix_content_row)))) (when (not (is_alnum validate_matrix_content_ch)) (throw (Exception. "Matrix rows can only contain letters and numbers."))) (set! validate_matrix_content_j (+ validate_matrix_content_j 1)))) (set! validate_matrix_content_i (+ validate_matrix_content_i 1)))) validate_matrix_content_matrix)))

(defn validate_moves [validate_moves_moves validate_moves_size]
  (binding [validate_moves_i nil validate_moves_mv nil] (do (set! validate_moves_i 0) (while (< validate_moves_i (count validate_moves_moves)) (do (set! validate_moves_mv (nth validate_moves_moves validate_moves_i)) (when (or (or (or (< (:x validate_moves_mv) 0) (>= (:x validate_moves_mv) validate_moves_size)) (< (:y validate_moves_mv) 0)) (>= (:y validate_moves_mv) validate_moves_size)) (throw (Exception. "Move is out of bounds for a matrix."))) (set! validate_moves_i (+ validate_moves_i 1)))) validate_moves_moves)))

(defn contains [contains_pos contains_r contains_c]
  (binding [contains_i nil contains_p nil] (try (do (set! contains_i 0) (while (< contains_i (count contains_pos)) (do (set! contains_p (nth contains_pos contains_i)) (when (and (= (:x contains_p) contains_r) (= (:y contains_p) contains_c)) (throw (ex-info "return" {:v true}))) (set! contains_i (+ contains_i 1)))) (throw (ex-info "return" {:v false}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn find_repeat [find_repeat_matrix_g find_repeat_row find_repeat_column_p find_repeat_size]
  (binding [find_repeat_column find_repeat_column_p find_repeat_color nil find_repeat_idx nil find_repeat_pos nil find_repeat_repeated nil find_repeat_stack nil find_repeat_visited nil] (try (do (set! find_repeat_column (- (- find_repeat_size 1) find_repeat_column)) (set! find_repeat_visited []) (set! find_repeat_repeated []) (set! find_repeat_color (nth (nth find_repeat_matrix_g find_repeat_column) find_repeat_row)) (when (= find_repeat_color "-") (throw (ex-info "return" {:v find_repeat_repeated}))) (set! find_repeat_stack [{:x find_repeat_column :y find_repeat_row}]) (loop [while_flag_1 true] (when (and while_flag_1 (> (count find_repeat_stack) 0)) (do (set! find_repeat_idx (- (count find_repeat_stack) 1)) (set! find_repeat_pos (nth find_repeat_stack find_repeat_idx)) (set! find_repeat_stack (subvec find_repeat_stack 0 find_repeat_idx)) (cond (or (or (or (< (:x find_repeat_pos) 0) (>= (:x find_repeat_pos) find_repeat_size)) (< (:y find_repeat_pos) 0)) (>= (:y find_repeat_pos) find_repeat_size)) (recur true) (contains find_repeat_visited (:x find_repeat_pos) (:y find_repeat_pos)) (recur true) :else (do (set! find_repeat_visited (conj find_repeat_visited find_repeat_pos)) (when (= (nth (nth find_repeat_matrix_g (:x find_repeat_pos)) (:y find_repeat_pos)) find_repeat_color) (do (set! find_repeat_repeated (conj find_repeat_repeated find_repeat_pos)) (set! find_repeat_stack (conj find_repeat_stack {:x (- (:x find_repeat_pos) 1) :y (:y find_repeat_pos)})) (set! find_repeat_stack (conj find_repeat_stack {:x (+ (:x find_repeat_pos) 1) :y (:y find_repeat_pos)})) (set! find_repeat_stack (conj find_repeat_stack {:x (:x find_repeat_pos) :y (- (:y find_repeat_pos) 1)})) (set! find_repeat_stack (conj find_repeat_stack {:x (:x find_repeat_pos) :y (+ (:y find_repeat_pos) 1)})))) (recur while_flag_1)))))) (throw (ex-info "return" {:v find_repeat_repeated}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))) (finally (alter-var-root (var find_repeat_column) (constantly find_repeat_column))))))

(defn increment_score [count_v]
  (try (throw (ex-info "return" {:v (quot (* count_v (+ count_v 1)) 2)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn move_x [move_x_matrix_g_p move_x_column move_x_size]
  (binding [move_x_matrix_g move_x_matrix_g_p move_x_new_list nil move_x_row nil move_x_val nil] (try (do (set! move_x_new_list []) (set! move_x_row 0) (while (< move_x_row move_x_size) (do (set! move_x_val (nth (nth move_x_matrix_g move_x_row) move_x_column)) (if (not= move_x_val "-") (set! move_x_new_list (conj move_x_new_list move_x_val)) (set! move_x_new_list (concat [move_x_val] move_x_new_list))) (set! move_x_row (+ move_x_row 1)))) (set! move_x_row 0) (while (< move_x_row move_x_size) (do (set! move_x_matrix_g (assoc-in move_x_matrix_g [move_x_row move_x_column] (nth move_x_new_list move_x_row))) (set! move_x_row (+ move_x_row 1)))) (throw (ex-info "return" {:v move_x_matrix_g}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))) (finally (alter-var-root (var move_x_matrix_g) (constantly move_x_matrix_g))))))

(defn move_y [move_y_matrix_g_p move_y_size]
  (binding [move_y_matrix_g move_y_matrix_g_p move_y_all_empty nil move_y_c nil move_y_col nil move_y_column nil move_y_empty_cols nil move_y_i nil move_y_r nil move_y_row nil] (try (do (set! move_y_empty_cols []) (set! move_y_column (- move_y_size 1)) (loop [while_flag_2 true] (when (and while_flag_2 (>= move_y_column 0)) (do (set! move_y_row 0) (set! move_y_all_empty true) (loop [while_flag_3 true] (when (and while_flag_3 (< move_y_row move_y_size)) (cond (not= (nth (nth move_y_matrix_g move_y_row) move_y_column) "-") (do (set! move_y_all_empty false) (recur false)) :else (do (set! move_y_row (+ move_y_row 1)) (recur while_flag_3))))) (when move_y_all_empty (set! move_y_empty_cols (conj move_y_empty_cols move_y_column))) (set! move_y_column (- move_y_column 1)) (cond :else (recur while_flag_2))))) (set! move_y_i 0) (while (< move_y_i (count move_y_empty_cols)) (do (set! move_y_col (nth move_y_empty_cols move_y_i)) (set! move_y_c (+ move_y_col 1)) (while (< move_y_c move_y_size) (do (set! move_y_r 0) (while (< move_y_r move_y_size) (do (set! move_y_matrix_g (assoc-in move_y_matrix_g [move_y_r (- move_y_c 1)] (nth (nth move_y_matrix_g move_y_r) move_y_c))) (set! move_y_r (+ move_y_r 1)))) (set! move_y_c (+ move_y_c 1)))) (set! move_y_r 0) (while (< move_y_r move_y_size) (do (set! move_y_matrix_g (assoc-in move_y_matrix_g [move_y_r (- move_y_size 1)] "-")) (set! move_y_r (+ move_y_r 1)))) (set! move_y_i (+ move_y_i 1)))) (throw (ex-info "return" {:v move_y_matrix_g}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))) (finally (alter-var-root (var move_y_matrix_g) (constantly move_y_matrix_g))))))

(defn play [play_matrix_g_p play_pos_x play_pos_y play_size]
  (binding [play_matrix_g play_matrix_g_p play_column nil play_i nil play_p nil play_same_colors nil play_sc nil] (try (do (set! play_same_colors (let [__res (find_repeat play_matrix_g play_pos_x play_pos_y play_size)] (do (set! play_pos_y find_repeat_column) __res))) (when (not= (count play_same_colors) 0) (do (set! play_i 0) (while (< play_i (count play_same_colors)) (do (set! play_p (nth play_same_colors play_i)) (set! play_matrix_g (assoc-in play_matrix_g [(:x play_p) (:y play_p)] "-")) (set! play_i (+ play_i 1)))) (set! play_column 0) (while (< play_column play_size) (do (set! play_matrix_g (let [__res (move_x play_matrix_g play_column play_size)] (do (set! play_matrix_g move_x_matrix_g) __res))) (set! play_column (+ play_column 1)))) (set! play_matrix_g (let [__res (move_y play_matrix_g play_size)] (do (set! play_matrix_g move_y_matrix_g) __res))))) (set! play_sc (increment_score (count play_same_colors))) (throw (ex-info "return" {:v {:matrix play_matrix_g :score play_sc}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))) (finally (alter-var-root (var play_matrix_g) (constantly play_matrix_g))))))

(defn build_matrix [build_matrix_matrix]
  (binding [build_matrix_i nil build_matrix_j nil build_matrix_res nil build_matrix_row nil build_matrix_row_list nil] (try (do (set! build_matrix_res []) (set! build_matrix_i 0) (while (< build_matrix_i (count build_matrix_matrix)) (do (set! build_matrix_row (nth build_matrix_matrix build_matrix_i)) (set! build_matrix_row_list []) (set! build_matrix_j 0) (while (< build_matrix_j (count build_matrix_row)) (do (set! build_matrix_row_list (conj build_matrix_row_list (subs build_matrix_row build_matrix_j (min (+ build_matrix_j 1) (count build_matrix_row))))) (set! build_matrix_j (+ build_matrix_j 1)))) (set! build_matrix_res (conj build_matrix_res build_matrix_row_list)) (set! build_matrix_i (+ build_matrix_i 1)))) (throw (ex-info "return" {:v build_matrix_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn process_game [process_game_size process_game_matrix process_game_moves]
  (binding [process_game_game_matrix nil process_game_i nil process_game_mv nil process_game_res nil process_game_total nil] (try (do (set! process_game_game_matrix (build_matrix process_game_matrix)) (set! process_game_total 0) (set! process_game_i 0) (while (< process_game_i (count process_game_moves)) (do (set! process_game_mv (nth process_game_moves process_game_i)) (set! process_game_res (let [__res (play process_game_game_matrix (:x process_game_mv) (:y process_game_mv) process_game_size)] (do (set! process_game_game_matrix play_matrix_g) __res))) (set! process_game_game_matrix (:matrix process_game_res)) (set! process_game_total (+ process_game_total (:score process_game_res))) (set! process_game_i (+ process_game_i 1)))) (throw (ex-info "return" {:v process_game_total}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_matrix nil main_moves nil main_score nil main_size nil] (do (set! main_size 4) (set! main_matrix ["RRBG" "RBBG" "YYGG" "XYGG"]) (set! main_moves (parse_moves "0 1,1 1")) (validate_matrix_size main_size) (validate_matrix_content main_matrix main_size) (validate_moves main_moves main_size) (set! main_score (process_game main_size main_matrix main_moves)) (println (mochi_str main_score)))))

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
