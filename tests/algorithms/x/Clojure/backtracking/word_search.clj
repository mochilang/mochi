(ns main (:refer-clojure :exclude [contains get_point_key search_from word_exists main]))

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

(declare contains get_point_key search_from word_exists main)

(def ^:dynamic contains_i nil)

(def ^:dynamic main_board nil)

(def ^:dynamic search_from_dir_i nil)

(def ^:dynamic search_from_dir_j nil)

(def ^:dynamic search_from_k nil)

(def ^:dynamic search_from_key nil)

(def ^:dynamic search_from_len_board nil)

(def ^:dynamic search_from_len_board_column nil)

(def ^:dynamic search_from_new_visited nil)

(def ^:dynamic search_from_next_i nil)

(def ^:dynamic search_from_next_j nil)

(def ^:dynamic word_exists_i nil)

(def ^:dynamic word_exists_j nil)

(def ^:dynamic word_exists_key nil)

(def ^:dynamic word_exists_len_board nil)

(def ^:dynamic word_exists_len_board_column nil)

(def ^:dynamic word_exists_visited nil)

(defn contains [contains_xs contains_x]
  (binding [contains_i nil] (try (do (set! contains_i 0) (while (< contains_i (count contains_xs)) (do (when (= (nth contains_xs contains_i) contains_x) (throw (ex-info "return" {:v true}))) (set! contains_i (+ contains_i 1)))) (throw (ex-info "return" {:v false}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn get_point_key [get_point_key_len_board get_point_key_len_board_column get_point_key_row get_point_key_column]
  (try (throw (ex-info "return" {:v (+ (* (* get_point_key_len_board get_point_key_len_board_column) get_point_key_row) get_point_key_column)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn search_from [search_from_board search_from_word search_from_row search_from_column search_from_word_index search_from_visited]
  (binding [search_from_dir_i nil search_from_dir_j nil search_from_k nil search_from_key nil search_from_len_board nil search_from_len_board_column nil search_from_new_visited nil search_from_next_i nil search_from_next_j nil] (try (do (when (not= (nth (nth search_from_board search_from_row) search_from_column) (subs search_from_word search_from_word_index (min (+ search_from_word_index 1) (count search_from_word)))) (throw (ex-info "return" {:v false}))) (when (= search_from_word_index (- (count search_from_word) 1)) (throw (ex-info "return" {:v true}))) (set! search_from_len_board (count search_from_board)) (set! search_from_len_board_column (count (nth search_from_board 0))) (set! search_from_dir_i [0 0 (- 1) 1]) (set! search_from_dir_j [1 (- 1) 0 0]) (set! search_from_k 0) (loop [while_flag_1 true] (when (and while_flag_1 (< search_from_k 4)) (do (set! search_from_next_i (+ search_from_row (nth search_from_dir_i search_from_k))) (set! search_from_next_j (+ search_from_column (nth search_from_dir_j search_from_k))) (cond (not (and (and (and (<= 0 search_from_next_i) (< search_from_next_i search_from_len_board)) (<= 0 search_from_next_j)) (< search_from_next_j search_from_len_board_column))) (do (set! search_from_k (+ search_from_k 1)) (recur true)) (contains search_from_visited search_from_key) (do (set! search_from_k (+ search_from_k 1)) (recur true)) :else (do (set! search_from_key (get_point_key search_from_len_board search_from_len_board_column search_from_next_i search_from_next_j)) (set! search_from_new_visited (conj search_from_visited search_from_key)) (when (search_from search_from_board search_from_word search_from_next_i search_from_next_j (+ search_from_word_index 1) search_from_new_visited) (throw (ex-info "return" {:v true}))) (set! search_from_k (+ search_from_k 1)) (recur while_flag_1)))))) (throw (ex-info "return" {:v false}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn word_exists [word_exists_board word_exists_word]
  (binding [word_exists_i nil word_exists_j nil word_exists_key nil word_exists_len_board nil word_exists_len_board_column nil word_exists_visited nil] (try (do (set! word_exists_len_board (count word_exists_board)) (set! word_exists_len_board_column (count (nth word_exists_board 0))) (set! word_exists_i 0) (while (< word_exists_i word_exists_len_board) (do (set! word_exists_j 0) (while (< word_exists_j word_exists_len_board_column) (do (set! word_exists_key (get_point_key word_exists_len_board word_exists_len_board_column word_exists_i word_exists_j)) (set! word_exists_visited (conj [] word_exists_key)) (when (search_from word_exists_board word_exists_word word_exists_i word_exists_j 0 word_exists_visited) (throw (ex-info "return" {:v true}))) (set! word_exists_j (+ word_exists_j 1)))) (set! word_exists_i (+ word_exists_i 1)))) (throw (ex-info "return" {:v false}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_board nil] (do (set! main_board [["A" "B" "C" "E"] ["S" "F" "C" "S"] ["A" "D" "E" "E"]]) (println (word_exists main_board "ABCCED")) (println (word_exists main_board "SEE")) (println (word_exists main_board "ABCB")))))

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
