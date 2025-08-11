(ns main (:refer-clojure :exclude [repeat_str split_words justify_line text_justification]))

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

(declare repeat_str split_words justify_line text_justification)

(def ^:dynamic justify_line_aligned nil)

(def ^:dynamic justify_line_base nil)

(def ^:dynamic justify_line_extra nil)

(def ^:dynamic justify_line_i nil)

(def ^:dynamic justify_line_num_spaces_between_words_list nil)

(def ^:dynamic justify_line_overall_spaces_count nil)

(def ^:dynamic justify_line_spaces nil)

(def ^:dynamic justify_line_spaces_to_insert_between_words nil)

(def ^:dynamic justify_line_words_count nil)

(def ^:dynamic repeat_str_i nil)

(def ^:dynamic repeat_str_res nil)

(def ^:dynamic split_words_ch nil)

(def ^:dynamic split_words_current nil)

(def ^:dynamic split_words_i nil)

(def ^:dynamic split_words_res nil)

(def ^:dynamic text_justification_answer nil)

(def ^:dynamic text_justification_idx nil)

(def ^:dynamic text_justification_j nil)

(def ^:dynamic text_justification_last_line nil)

(def ^:dynamic text_justification_line nil)

(def ^:dynamic text_justification_remaining_spaces nil)

(def ^:dynamic text_justification_w nil)

(def ^:dynamic text_justification_width nil)

(def ^:dynamic text_justification_words nil)

(defn repeat_str [repeat_str_s count_v]
  (binding [repeat_str_i nil repeat_str_res nil] (try (do (set! repeat_str_res "") (set! repeat_str_i 0) (while (< repeat_str_i count_v) (do (set! repeat_str_res (str repeat_str_res repeat_str_s)) (set! repeat_str_i (+ repeat_str_i 1)))) (throw (ex-info "return" {:v repeat_str_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn split_words [split_words_s]
  (binding [split_words_ch nil split_words_current nil split_words_i nil split_words_res nil] (try (do (set! split_words_res []) (set! split_words_current "") (set! split_words_i 0) (while (< split_words_i (count split_words_s)) (do (set! split_words_ch (subs split_words_s split_words_i (min (+ split_words_i 1) (count split_words_s)))) (if (= split_words_ch " ") (when (not= split_words_current "") (do (set! split_words_res (conj split_words_res split_words_current)) (set! split_words_current ""))) (set! split_words_current (str split_words_current split_words_ch))) (set! split_words_i (+ split_words_i 1)))) (when (not= split_words_current "") (set! split_words_res (conj split_words_res split_words_current))) (throw (ex-info "return" {:v split_words_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn justify_line [justify_line_line justify_line_width justify_line_max_width]
  (binding [justify_line_aligned nil justify_line_base nil justify_line_extra nil justify_line_i nil justify_line_num_spaces_between_words_list nil justify_line_overall_spaces_count nil justify_line_spaces nil justify_line_spaces_to_insert_between_words nil justify_line_words_count nil] (try (do (set! justify_line_overall_spaces_count (- justify_line_max_width justify_line_width)) (set! justify_line_words_count (count justify_line_line)) (when (= justify_line_words_count 1) (throw (ex-info "return" {:v (str (nth justify_line_line 0) (repeat_str " " justify_line_overall_spaces_count))}))) (set! justify_line_spaces_to_insert_between_words (- justify_line_words_count 1)) (set! justify_line_num_spaces_between_words_list []) (set! justify_line_base (/ justify_line_overall_spaces_count justify_line_spaces_to_insert_between_words)) (set! justify_line_extra (mod justify_line_overall_spaces_count justify_line_spaces_to_insert_between_words)) (set! justify_line_i 0) (while (< justify_line_i justify_line_spaces_to_insert_between_words) (do (set! justify_line_spaces justify_line_base) (when (< justify_line_i justify_line_extra) (set! justify_line_spaces (+ justify_line_spaces 1))) (set! justify_line_num_spaces_between_words_list (conj justify_line_num_spaces_between_words_list justify_line_spaces)) (set! justify_line_i (+ justify_line_i 1)))) (set! justify_line_aligned "") (set! justify_line_i 0) (while (< justify_line_i justify_line_spaces_to_insert_between_words) (do (set! justify_line_aligned (str (str justify_line_aligned (nth justify_line_line justify_line_i)) (repeat_str " " (nth justify_line_num_spaces_between_words_list justify_line_i)))) (set! justify_line_i (+ justify_line_i 1)))) (set! justify_line_aligned (str justify_line_aligned (nth justify_line_line justify_line_spaces_to_insert_between_words))) (throw (ex-info "return" {:v justify_line_aligned}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn text_justification [text_justification_word text_justification_max_width]
  (binding [text_justification_answer nil text_justification_idx nil text_justification_j nil text_justification_last_line nil text_justification_line nil text_justification_remaining_spaces nil text_justification_w nil text_justification_width nil text_justification_words nil] (try (do (set! text_justification_words (split_words text_justification_word)) (set! text_justification_answer []) (set! text_justification_line []) (set! text_justification_width 0) (set! text_justification_idx 0) (while (< text_justification_idx (count text_justification_words)) (do (set! text_justification_w (nth text_justification_words text_justification_idx)) (if (<= (+ (+ text_justification_width (count text_justification_w)) (count text_justification_line)) text_justification_max_width) (do (set! text_justification_line (conj text_justification_line text_justification_w)) (set! text_justification_width (+ text_justification_width (count text_justification_w)))) (do (set! text_justification_answer (conj text_justification_answer (justify_line text_justification_line text_justification_width text_justification_max_width))) (set! text_justification_line [text_justification_w]) (set! text_justification_width (count text_justification_w)))) (set! text_justification_idx (+ text_justification_idx 1)))) (set! text_justification_remaining_spaces (- (- text_justification_max_width text_justification_width) (count text_justification_line))) (set! text_justification_last_line "") (set! text_justification_j 0) (while (< text_justification_j (count text_justification_line)) (do (when (> text_justification_j 0) (set! text_justification_last_line (str text_justification_last_line " "))) (set! text_justification_last_line (str text_justification_last_line (nth text_justification_line text_justification_j))) (set! text_justification_j (+ text_justification_j 1)))) (set! text_justification_last_line (str text_justification_last_line (repeat_str " " (+ text_justification_remaining_spaces 1)))) (set! text_justification_answer (conj text_justification_answer text_justification_last_line)) (throw (ex-info "return" {:v text_justification_answer}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (text_justification "This is an example of text justification." 16)))
      (println (str (text_justification "Two roads diverged in a yellow wood" 16)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
