(ns main (:refer-clojure :exclude [rand rand_range shuffle rand_letter make_word_search insert_dir generate_board visualise main]))

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

(declare rand rand_range shuffle rand_letter make_word_search insert_dir generate_board visualise main)

(def ^:dynamic generate_board_c nil)

(def ^:dynamic generate_board_cols nil)

(def ^:dynamic generate_board_d nil)

(def ^:dynamic generate_board_dirs_c nil)

(def ^:dynamic generate_board_dirs_r nil)

(def ^:dynamic generate_board_i nil)

(def ^:dynamic generate_board_r nil)

(def ^:dynamic generate_board_rows nil)

(def ^:dynamic generate_board_word nil)

(def ^:dynamic insert_dir_cc nil)

(def ^:dynamic insert_dir_cc2 nil)

(def ^:dynamic insert_dir_ci nil)

(def ^:dynamic insert_dir_col nil)

(def ^:dynamic insert_dir_end_c nil)

(def ^:dynamic insert_dir_end_r nil)

(def ^:dynamic insert_dir_k nil)

(def ^:dynamic insert_dir_ok nil)

(def ^:dynamic insert_dir_ri nil)

(def ^:dynamic insert_dir_row nil)

(def ^:dynamic insert_dir_row_list nil)

(def ^:dynamic insert_dir_rr nil)

(def ^:dynamic insert_dir_rr2 nil)

(def ^:dynamic insert_dir_word_len nil)

(def ^:dynamic main_words nil)

(def ^:dynamic main_ws nil)

(def ^:dynamic make_word_search_board nil)

(def ^:dynamic make_word_search_c nil)

(def ^:dynamic make_word_search_r nil)

(def ^:dynamic make_word_search_row nil)

(def ^:dynamic rand_letter_i nil)

(def ^:dynamic rand_letter_letters nil)

(def ^:dynamic shuffle_i nil)

(def ^:dynamic shuffle_j nil)

(def ^:dynamic shuffle_list_int nil)

(def ^:dynamic shuffle_tmp nil)

(def ^:dynamic visualise_c nil)

(def ^:dynamic visualise_ch nil)

(def ^:dynamic visualise_r nil)

(def ^:dynamic visualise_result nil)

(def ^:dynamic main_seed 123456789)

(defn rand []
  (try (do (alter-var-root (var main_seed) (fn [_] (mod (+ (* main_seed 1103515245) 12345) 2147483648))) (throw (ex-info "return" {:v main_seed}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn rand_range [rand_range_max]
  (try (throw (ex-info "return" {:v (mod (rand) rand_range_max)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn shuffle [shuffle_list_int_p]
  (binding [shuffle_list_int shuffle_list_int_p shuffle_i nil shuffle_j nil shuffle_tmp nil] (try (do (set! shuffle_i (- (count shuffle_list_int) 1)) (while (> shuffle_i 0) (do (set! shuffle_j (rand_range (+ shuffle_i 1))) (set! shuffle_tmp (nth shuffle_list_int shuffle_i)) (set! shuffle_list_int (assoc shuffle_list_int shuffle_i (nth shuffle_list_int shuffle_j))) (set! shuffle_list_int (assoc shuffle_list_int shuffle_j shuffle_tmp)) (set! shuffle_i (- shuffle_i 1)))) (throw (ex-info "return" {:v shuffle_list_int}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))) (finally (alter-var-root (var shuffle_list_int) (constantly shuffle_list_int))))))

(defn rand_letter []
  (binding [rand_letter_i nil rand_letter_letters nil] (try (do (set! rand_letter_letters "abcdefghijklmnopqrstuvwxyz") (set! rand_letter_i (rand_range 26)) (throw (ex-info "return" {:v (subs rand_letter_letters rand_letter_i (min (+ rand_letter_i 1) (count rand_letter_letters)))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn make_word_search [make_word_search_words make_word_search_width make_word_search_height]
  (binding [make_word_search_board nil make_word_search_c nil make_word_search_r nil make_word_search_row nil] (try (do (set! make_word_search_board []) (set! make_word_search_r 0) (while (< make_word_search_r make_word_search_height) (do (set! make_word_search_row []) (set! make_word_search_c 0) (while (< make_word_search_c make_word_search_width) (do (set! make_word_search_row (conj make_word_search_row "")) (set! make_word_search_c (+ make_word_search_c 1)))) (set! make_word_search_board (conj make_word_search_board make_word_search_row)) (set! make_word_search_r (+ make_word_search_r 1)))) (throw (ex-info "return" {:v {:board make_word_search_board :height make_word_search_height :width make_word_search_width :words make_word_search_words}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn insert_dir [insert_dir_ws insert_dir_word insert_dir_dr insert_dir_dc insert_dir_rows insert_dir_cols]
  (binding [insert_dir_cc nil insert_dir_cc2 nil insert_dir_ci nil insert_dir_col nil insert_dir_end_c nil insert_dir_end_r nil insert_dir_k nil insert_dir_ok nil insert_dir_ri nil insert_dir_row nil insert_dir_row_list nil insert_dir_rr nil insert_dir_rr2 nil insert_dir_word_len nil] (try (do (set! insert_dir_word_len (count insert_dir_word)) (set! insert_dir_ri 0) (loop [while_flag_1 true] (when (and while_flag_1 (< insert_dir_ri (count insert_dir_rows))) (do (set! insert_dir_row (nth insert_dir_rows insert_dir_ri)) (set! insert_dir_ci 0) (loop [while_flag_2 true] (when (and while_flag_2 (< insert_dir_ci (count insert_dir_cols))) (do (set! insert_dir_col (nth insert_dir_cols insert_dir_ci)) (set! insert_dir_end_r (+ insert_dir_row (* insert_dir_dr (- insert_dir_word_len 1)))) (set! insert_dir_end_c (+ insert_dir_col (* insert_dir_dc (- insert_dir_word_len 1)))) (cond (or (or (or (< insert_dir_end_r 0) (>= insert_dir_end_r (:height insert_dir_ws))) (< insert_dir_end_c 0)) (>= insert_dir_end_c (:width insert_dir_ws))) (do (set! insert_dir_ci (+ insert_dir_ci 1)) (recur true)) :else (do (set! insert_dir_k 0) (set! insert_dir_ok true) (loop [while_flag_3 true] (when (and while_flag_3 (< insert_dir_k insert_dir_word_len)) (do (set! insert_dir_rr (+ insert_dir_row (* insert_dir_dr insert_dir_k))) (set! insert_dir_cc (+ insert_dir_col (* insert_dir_dc insert_dir_k))) (cond (not= (get (get (:board insert_dir_ws) insert_dir_rr) insert_dir_cc) "") (do (set! insert_dir_ok false) (recur false)) :else (do (set! insert_dir_k (+ insert_dir_k 1)) (recur while_flag_3)))))) (when insert_dir_ok (do (set! insert_dir_k 0) (while (< insert_dir_k insert_dir_word_len) (do (set! insert_dir_rr2 (+ insert_dir_row (* insert_dir_dr insert_dir_k))) (set! insert_dir_cc2 (+ insert_dir_col (* insert_dir_dc insert_dir_k))) (set! insert_dir_row_list (get (:board insert_dir_ws) insert_dir_rr2)) (set! insert_dir_row_list (assoc insert_dir_row_list insert_dir_cc2 (subs insert_dir_word insert_dir_k (min (+ insert_dir_k 1) (count insert_dir_word))))) (set! insert_dir_k (+ insert_dir_k 1)))) (throw (ex-info "return" {:v true})))) (set! insert_dir_ci (+ insert_dir_ci 1)) (recur while_flag_2)))))) (set! insert_dir_ri (+ insert_dir_ri 1)) (cond :else (recur while_flag_1))))) (throw (ex-info "return" {:v false}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn generate_board [generate_board_ws]
  (binding [generate_board_c nil generate_board_cols nil generate_board_d nil generate_board_dirs_c nil generate_board_dirs_r nil generate_board_i nil generate_board_r nil generate_board_rows nil generate_board_word nil] (do (set! generate_board_dirs_r [(- 1) (- 1) 0 1 1 1 0 (- 1)]) (set! generate_board_dirs_c [0 1 1 1 0 (- 1) (- 1) (- 1)]) (set! generate_board_i 0) (while (< generate_board_i (count (:words generate_board_ws))) (do (set! generate_board_word (get (:words generate_board_ws) generate_board_i)) (set! generate_board_rows []) (set! generate_board_r 0) (while (< generate_board_r (:height generate_board_ws)) (do (set! generate_board_rows (conj generate_board_rows generate_board_r)) (set! generate_board_r (+ generate_board_r 1)))) (set! generate_board_cols []) (set! generate_board_c 0) (while (< generate_board_c (:width generate_board_ws)) (do (set! generate_board_cols (conj generate_board_cols generate_board_c)) (set! generate_board_c (+ generate_board_c 1)))) (set! generate_board_rows (let [__res (shuffle generate_board_rows)] (do (set! generate_board_rows shuffle_list_int) __res))) (set! generate_board_cols (let [__res (shuffle generate_board_cols)] (do (set! generate_board_cols shuffle_list_int) __res))) (set! generate_board_d (rand_range 8)) (insert_dir generate_board_ws generate_board_word (nth generate_board_dirs_r generate_board_d) (nth generate_board_dirs_c generate_board_d) generate_board_rows generate_board_cols) (set! generate_board_i (+ generate_board_i 1)))) generate_board_ws)))

(defn visualise [visualise_ws visualise_add_fake_chars]
  (binding [visualise_c nil visualise_ch nil visualise_r nil visualise_result nil] (try (do (set! visualise_result "") (set! visualise_r 0) (while (< visualise_r (:height visualise_ws)) (do (set! visualise_c 0) (while (< visualise_c (:width visualise_ws)) (do (set! visualise_ch (get (get (:board visualise_ws) visualise_r) visualise_c)) (when (= visualise_ch "") (if visualise_add_fake_chars (set! visualise_ch (rand_letter)) (set! visualise_ch "#"))) (set! visualise_result (str (str visualise_result visualise_ch) " ")) (set! visualise_c (+ visualise_c 1)))) (set! visualise_result (str visualise_result "\n")) (set! visualise_r (+ visualise_r 1)))) (throw (ex-info "return" {:v visualise_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_words nil main_ws nil] (do (set! main_words ["cat" "dog" "snake" "fish"]) (set! main_ws (make_word_search main_words 10 10)) (generate_board main_ws) (println (visualise main_ws true)))))

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
