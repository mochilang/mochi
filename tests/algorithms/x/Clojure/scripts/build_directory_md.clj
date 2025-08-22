(ns main (:refer-clojure :exclude [split join repeat replace_char contains file_extension remove_extension title_case count_char md_prefix print_path sort_strings good_file_paths print_directory_md]))

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
  (int (Double/valueOf (str s))))

(defn _ord [s]
  (int (first s)))

(defn mochi_str [v]
  (cond (float? v) (let [s (str v)] (if (clojure.string/ends-with? s ".0") (subs s 0 (- (count s) 2)) s)) :else (str v)))

(defn _fetch [url]
  {:data [{:from "" :intensity {:actual 0 :forecast 0 :index ""} :to ""}]})

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare split join repeat replace_char contains file_extension remove_extension title_case count_char md_prefix print_path sort_strings good_file_paths print_directory_md)

(declare _read_file)

(def ^:dynamic contains_i nil)

(def ^:dynamic count_char_cnt nil)

(def ^:dynamic count_char_i nil)

(def ^:dynamic file_extension_i nil)

(def ^:dynamic good_file_paths_ext nil)

(def ^:dynamic good_file_paths_filename nil)

(def ^:dynamic good_file_paths_k nil)

(def ^:dynamic good_file_paths_p nil)

(def ^:dynamic good_file_paths_part nil)

(def ^:dynamic good_file_paths_parts nil)

(def ^:dynamic good_file_paths_res nil)

(def ^:dynamic good_file_paths_skip nil)

(def ^:dynamic join_i nil)

(def ^:dynamic join_res nil)

(def ^:dynamic print_directory_md_filename nil)

(def ^:dynamic print_directory_md_filepath nil)

(def ^:dynamic print_directory_md_files nil)

(def ^:dynamic print_directory_md_fp nil)

(def ^:dynamic print_directory_md_i nil)

(def ^:dynamic print_directory_md_indent nil)

(def ^:dynamic print_directory_md_name nil)

(def ^:dynamic print_directory_md_old_path nil)

(def ^:dynamic print_directory_md_parts nil)

(def ^:dynamic print_directory_md_url nil)

(def ^:dynamic print_path_i nil)

(def ^:dynamic print_path_new_parts nil)

(def ^:dynamic print_path_old_parts nil)

(def ^:dynamic print_path_title nil)

(def ^:dynamic remove_extension_i nil)

(def ^:dynamic repeat_i nil)

(def ^:dynamic repeat_out nil)

(def ^:dynamic replace_char_c nil)

(def ^:dynamic replace_char_i nil)

(def ^:dynamic replace_char_out nil)

(def ^:dynamic sort_strings_arr nil)

(def ^:dynamic sort_strings_i nil)

(def ^:dynamic sort_strings_j nil)

(def ^:dynamic sort_strings_min_idx nil)

(def ^:dynamic sort_strings_tmp nil)

(def ^:dynamic split_cur nil)

(def ^:dynamic split_i nil)

(def ^:dynamic split_parts nil)

(def ^:dynamic title_case_c nil)

(def ^:dynamic title_case_cap nil)

(def ^:dynamic title_case_i nil)

(def ^:dynamic title_case_out nil)

(defn split [split_s split_sep]
  (binding [split_cur nil split_i nil split_parts nil] (try (do (set! split_parts []) (set! split_cur "") (set! split_i 0) (while (< split_i (count split_s)) (if (and (and (> (count split_sep) 0) (<= (+ split_i (count split_sep)) (count split_s))) (= (subs split_s split_i (min (+ split_i (count split_sep)) (count split_s))) split_sep)) (do (set! split_parts (conj split_parts split_cur)) (set! split_cur "") (set! split_i (+ split_i (count split_sep)))) (do (set! split_cur (str split_cur (subs split_s split_i (min (+ split_i 1) (count split_s))))) (set! split_i (+ split_i 1))))) (set! split_parts (conj split_parts split_cur)) (throw (ex-info "return" {:v split_parts}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn join [join_xs join_sep]
  (binding [join_i nil join_res nil] (try (do (set! join_res "") (set! join_i 0) (while (< join_i (count join_xs)) (do (when (> join_i 0) (set! join_res (str join_res join_sep))) (set! join_res (str join_res (nth join_xs join_i))) (set! join_i (+ join_i 1)))) (throw (ex-info "return" {:v join_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn repeat [repeat_s repeat_n]
  (binding [repeat_i nil repeat_out nil] (try (do (set! repeat_out "") (set! repeat_i 0) (while (< repeat_i repeat_n) (do (set! repeat_out (str repeat_out repeat_s)) (set! repeat_i (+ repeat_i 1)))) (throw (ex-info "return" {:v repeat_out}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn replace_char [replace_char_s replace_char_old replace_char_new]
  (binding [replace_char_c nil replace_char_i nil replace_char_out nil] (try (do (set! replace_char_out "") (set! replace_char_i 0) (while (< replace_char_i (count replace_char_s)) (do (set! replace_char_c (subs replace_char_s replace_char_i (min (+ replace_char_i 1) (count replace_char_s)))) (if (= replace_char_c replace_char_old) (set! replace_char_out (str replace_char_out replace_char_new)) (set! replace_char_out (str replace_char_out replace_char_c))) (set! replace_char_i (+ replace_char_i 1)))) (throw (ex-info "return" {:v replace_char_out}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn contains [contains_s contains_sub]
  (binding [contains_i nil] (try (do (when (= (count contains_sub) 0) (throw (ex-info "return" {:v true}))) (set! contains_i 0) (while (<= (+ contains_i (count contains_sub)) (count contains_s)) (do (when (= (subs contains_s contains_i (min (+ contains_i (count contains_sub)) (count contains_s))) contains_sub) (throw (ex-info "return" {:v true}))) (set! contains_i (+ contains_i 1)))) (throw (ex-info "return" {:v false}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn file_extension [file_extension_name]
  (binding [file_extension_i nil] (try (do (set! file_extension_i (- (count file_extension_name) 1)) (while (>= file_extension_i 0) (do (when (= (subs file_extension_name file_extension_i (min (+ file_extension_i 1) (count file_extension_name))) ".") (throw (ex-info "return" {:v (subs file_extension_name file_extension_i (min (count file_extension_name) (count file_extension_name)))}))) (set! file_extension_i (- file_extension_i 1)))) (throw (ex-info "return" {:v ""}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn remove_extension [remove_extension_name]
  (binding [remove_extension_i nil] (try (do (set! remove_extension_i (- (count remove_extension_name) 1)) (while (>= remove_extension_i 0) (do (when (= (subs remove_extension_name remove_extension_i (min (+ remove_extension_i 1) (count remove_extension_name))) ".") (throw (ex-info "return" {:v (subs remove_extension_name 0 (min remove_extension_i (count remove_extension_name)))}))) (set! remove_extension_i (- remove_extension_i 1)))) (throw (ex-info "return" {:v remove_extension_name}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn title_case [title_case_s]
  (binding [title_case_c nil title_case_cap nil title_case_i nil title_case_out nil] (try (do (set! title_case_out "") (set! title_case_cap true) (set! title_case_i 0) (while (< title_case_i (count title_case_s)) (do (set! title_case_c (subs title_case_s title_case_i (min (+ title_case_i 1) (count title_case_s)))) (if (= title_case_c " ") (do (set! title_case_out (str title_case_out title_case_c)) (set! title_case_cap true)) (if title_case_cap (do (set! title_case_out (str title_case_out (clojure.string/upper-case title_case_c))) (set! title_case_cap false)) (set! title_case_out (str title_case_out (clojure.string/lower-case title_case_c))))) (set! title_case_i (+ title_case_i 1)))) (throw (ex-info "return" {:v title_case_out}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn count_char [count_char_s count_char_ch]
  (binding [count_char_cnt nil count_char_i nil] (try (do (set! count_char_cnt 0) (set! count_char_i 0) (while (< count_char_i (count count_char_s)) (do (when (= (subs count_char_s count_char_i (min (+ count_char_i 1) (count count_char_s))) count_char_ch) (set! count_char_cnt (+ count_char_cnt 1))) (set! count_char_i (+ count_char_i 1)))) (throw (ex-info "return" {:v count_char_cnt}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn md_prefix [md_prefix_level]
  (try (if (= md_prefix_level 0) "\n##" (str (repeat "  " md_prefix_level) "*")) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn print_path [print_path_old_path print_path_new_path]
  (binding [print_path_i nil print_path_new_parts nil print_path_old_parts nil print_path_title nil] (try (do (set! print_path_old_parts (split print_path_old_path "/")) (set! print_path_new_parts (split print_path_new_path "/")) (set! print_path_i 0) (while (< print_path_i (count print_path_new_parts)) (do (when (and (or (>= print_path_i (count print_path_old_parts)) (not= (nth print_path_old_parts print_path_i) (nth print_path_new_parts print_path_i))) (not= (nth print_path_new_parts print_path_i) "")) (do (set! print_path_title (title_case (replace_char (nth print_path_new_parts print_path_i) "_" " "))) (println (str (str (md_prefix print_path_i) " ") print_path_title)))) (set! print_path_i (+ print_path_i 1)))) (throw (ex-info "return" {:v print_path_new_path}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn sort_strings [sort_strings_xs]
  (binding [sort_strings_arr nil sort_strings_i nil sort_strings_j nil sort_strings_min_idx nil sort_strings_tmp nil] (try (do (set! sort_strings_arr sort_strings_xs) (set! sort_strings_i 0) (while (< sort_strings_i (count sort_strings_arr)) (do (set! sort_strings_min_idx sort_strings_i) (set! sort_strings_j (+ sort_strings_i 1)) (while (< sort_strings_j (count sort_strings_arr)) (do (when (< (compare (nth sort_strings_arr sort_strings_j) (nth sort_strings_arr sort_strings_min_idx)) 0) (set! sort_strings_min_idx sort_strings_j)) (set! sort_strings_j (+ sort_strings_j 1)))) (set! sort_strings_tmp (nth sort_strings_arr sort_strings_i)) (set! sort_strings_arr (assoc sort_strings_arr sort_strings_i (nth sort_strings_arr sort_strings_min_idx))) (set! sort_strings_arr (assoc sort_strings_arr sort_strings_min_idx sort_strings_tmp)) (set! sort_strings_i (+ sort_strings_i 1)))) (throw (ex-info "return" {:v sort_strings_arr}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn good_file_paths [good_file_paths_paths]
  (binding [good_file_paths_ext nil good_file_paths_filename nil good_file_paths_k nil good_file_paths_p nil good_file_paths_part nil good_file_paths_parts nil good_file_paths_res nil good_file_paths_skip nil] (try (do (set! good_file_paths_res []) (loop [good_file_paths_p_seq good_file_paths_paths] (when (seq good_file_paths_p_seq) (let [good_file_paths_p (first good_file_paths_p_seq)] (do (set! good_file_paths_parts (split good_file_paths_p "/")) (set! good_file_paths_skip false) (set! good_file_paths_k 0) (while (< good_file_paths_k (- (count good_file_paths_parts) 1)) (do (set! good_file_paths_part (nth good_file_paths_parts good_file_paths_k)) (when (or (or (or (= good_file_paths_part "scripts") (= (subvec good_file_paths_part 0 1) ".")) (= (subvec good_file_paths_part 0 1) "_")) (contains good_file_paths_part "venv")) (set! good_file_paths_skip true)) (set! good_file_paths_k (+ good_file_paths_k 1)))) (cond good_file_paths_skip (recur (rest good_file_paths_p_seq)) (= good_file_paths_filename "__init__.py") (recur (rest good_file_paths_p_seq)) :else (do (set! good_file_paths_filename (nth good_file_paths_parts (- (count good_file_paths_parts) 1))) (set! good_file_paths_ext (file_extension good_file_paths_filename)) (when (or (= good_file_paths_ext ".py") (= good_file_paths_ext ".ipynb")) (set! good_file_paths_res (conj good_file_paths_res good_file_paths_p))) (recur (rest good_file_paths_p_seq)))))))) (throw (ex-info "return" {:v good_file_paths_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn print_directory_md [print_directory_md_paths]
  (binding [print_directory_md_filename nil print_directory_md_filepath nil print_directory_md_files nil print_directory_md_fp nil print_directory_md_i nil print_directory_md_indent nil print_directory_md_name nil print_directory_md_old_path nil print_directory_md_parts nil print_directory_md_url nil] (do (set! print_directory_md_files (sort_strings (good_file_paths print_directory_md_paths))) (set! print_directory_md_old_path "") (set! print_directory_md_i 0) (while (< print_directory_md_i (count print_directory_md_files)) (do (set! print_directory_md_fp (nth print_directory_md_files print_directory_md_i)) (set! print_directory_md_parts (split print_directory_md_fp "/")) (set! print_directory_md_filename (nth print_directory_md_parts (- (count print_directory_md_parts) 1))) (set! print_directory_md_filepath "") (when (> (count print_directory_md_parts) 1) (set! print_directory_md_filepath (join (subvec print_directory_md_parts 0 (- (count print_directory_md_parts) 1)) "/"))) (when (not= print_directory_md_filepath print_directory_md_old_path) (set! print_directory_md_old_path (print_path print_directory_md_old_path print_directory_md_filepath))) (set! print_directory_md_indent 0) (when (> (count print_directory_md_filepath) 0) (set! print_directory_md_indent (+ (count_char print_directory_md_filepath "/") 1))) (set! print_directory_md_url (replace_char print_directory_md_fp " " "%20")) (set! print_directory_md_name (title_case (replace_char (remove_extension print_directory_md_filename) "_" " "))) (println (str (str (str (str (str (md_prefix print_directory_md_indent) " [") print_directory_md_name) "](") print_directory_md_url) ")")) (set! print_directory_md_i (+ print_directory_md_i 1)))) print_directory_md_paths)))

(def ^:dynamic main_sample nil)

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (alter-var-root (var main_sample) (constantly ["data_structures/linked_list.py" "data_structures/binary_tree.py" "math/number_theory/prime_check.py" "math/number_theory/greatest_common_divisor.ipynb"]))
      (print_directory_md main_sample)
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
