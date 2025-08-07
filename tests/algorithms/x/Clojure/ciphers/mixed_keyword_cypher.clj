(ns main (:refer-clojure :exclude [to_upper contains contains_char get_value print_mapping mixed_keyword]))

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

(declare to_upper contains contains_char get_value print_mapping mixed_keyword)

(def ^:dynamic contains_char_i nil)

(def ^:dynamic contains_i nil)

(def ^:dynamic get_value_i nil)

(def ^:dynamic mixed_keyword_alphabet nil)

(def ^:dynamic mixed_keyword_ch nil)

(def ^:dynamic mixed_keyword_column nil)

(def ^:dynamic mixed_keyword_i nil)

(def ^:dynamic mixed_keyword_k nil)

(def ^:dynamic mixed_keyword_keys nil)

(def ^:dynamic mixed_keyword_keyword_u nil)

(def ^:dynamic mixed_keyword_letter_index nil)

(def ^:dynamic mixed_keyword_mapped nil)

(def ^:dynamic mixed_keyword_modified nil)

(def ^:dynamic mixed_keyword_num_unique nil)

(def ^:dynamic mixed_keyword_plaintext_u nil)

(def ^:dynamic mixed_keyword_r nil)

(def ^:dynamic mixed_keyword_result nil)

(def ^:dynamic mixed_keyword_row nil)

(def ^:dynamic mixed_keyword_row_idx nil)

(def ^:dynamic mixed_keyword_shifted nil)

(def ^:dynamic mixed_keyword_unique nil)

(def ^:dynamic mixed_keyword_values nil)

(def ^:dynamic print_mapping_i nil)

(def ^:dynamic print_mapping_s nil)

(def ^:dynamic to_upper_ch nil)

(def ^:dynamic to_upper_found nil)

(def ^:dynamic to_upper_i nil)

(def ^:dynamic to_upper_j nil)

(def ^:dynamic to_upper_res nil)

(def ^:dynamic main_UPPER "ABCDEFGHIJKLMNOPQRSTUVWXYZ")

(def ^:dynamic main_LOWER "abcdefghijklmnopqrstuvwxyz")

(defn to_upper [to_upper_s]
  (binding [to_upper_ch nil to_upper_found nil to_upper_i nil to_upper_j nil to_upper_res nil] (try (do (set! to_upper_res "") (set! to_upper_i 0) (while (< to_upper_i (count to_upper_s)) (do (set! to_upper_ch (nth to_upper_s to_upper_i)) (set! to_upper_j 0) (set! to_upper_found false) (loop [while_flag_1 true] (when (and while_flag_1 (< to_upper_j 26)) (cond (= to_upper_ch (nth main_LOWER to_upper_j)) (do (set! to_upper_res (str to_upper_res (nth main_UPPER to_upper_j))) (set! to_upper_found true) (recur false)) :else (do (set! to_upper_j (+ to_upper_j 1)) (recur while_flag_1))))) (when (= to_upper_found false) (set! to_upper_res (str to_upper_res to_upper_ch))) (set! to_upper_i (+ to_upper_i 1)))) (throw (ex-info "return" {:v to_upper_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn contains [contains_xs contains_x]
  (binding [contains_i nil] (try (do (set! contains_i 0) (while (< contains_i (count contains_xs)) (do (when (= (nth contains_xs contains_i) contains_x) (throw (ex-info "return" {:v true}))) (set! contains_i (+ contains_i 1)))) (throw (ex-info "return" {:v false}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn contains_char [contains_char_s contains_char_ch]
  (binding [contains_char_i nil] (try (do (set! contains_char_i 0) (while (< contains_char_i (count contains_char_s)) (do (when (= (nth contains_char_s contains_char_i) contains_char_ch) (throw (ex-info "return" {:v true}))) (set! contains_char_i (+ contains_char_i 1)))) (throw (ex-info "return" {:v false}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn get_value [get_value_keys get_value_values get_value_key]
  (binding [get_value_i nil] (try (do (set! get_value_i 0) (while (< get_value_i (count get_value_keys)) (do (when (= (nth get_value_keys get_value_i) get_value_key) (throw (ex-info "return" {:v (nth get_value_values get_value_i)}))) (set! get_value_i (+ get_value_i 1)))) (throw (ex-info "return" {:v nil}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn print_mapping [print_mapping_keys print_mapping_values]
  (binding [print_mapping_i nil print_mapping_s nil] (do (set! print_mapping_s "{") (set! print_mapping_i 0) (while (< print_mapping_i (count print_mapping_keys)) (do (set! print_mapping_s (str (str (str (str (str print_mapping_s "'") (nth print_mapping_keys print_mapping_i)) "': '") (nth print_mapping_values print_mapping_i)) "'")) (when (< (+ print_mapping_i 1) (count print_mapping_keys)) (set! print_mapping_s (str print_mapping_s ", "))) (set! print_mapping_i (+ print_mapping_i 1)))) (set! print_mapping_s (str print_mapping_s "}")) (println print_mapping_s))))

(defn mixed_keyword [mixed_keyword_keyword mixed_keyword_plaintext mixed_keyword_verbose]
  (binding [mixed_keyword_alphabet nil mixed_keyword_ch nil mixed_keyword_column nil mixed_keyword_i nil mixed_keyword_k nil mixed_keyword_keys nil mixed_keyword_keyword_u nil mixed_keyword_letter_index nil mixed_keyword_mapped nil mixed_keyword_modified nil mixed_keyword_num_unique nil mixed_keyword_plaintext_u nil mixed_keyword_r nil mixed_keyword_result nil mixed_keyword_row nil mixed_keyword_row_idx nil mixed_keyword_shifted nil mixed_keyword_unique nil mixed_keyword_values nil] (try (do (set! mixed_keyword_alphabet main_UPPER) (set! mixed_keyword_keyword_u (to_upper mixed_keyword_keyword)) (set! mixed_keyword_plaintext_u (to_upper mixed_keyword_plaintext)) (set! mixed_keyword_unique []) (set! mixed_keyword_i 0) (while (< mixed_keyword_i (count mixed_keyword_keyword_u)) (do (set! mixed_keyword_ch (nth mixed_keyword_keyword_u mixed_keyword_i)) (when (and (contains_char mixed_keyword_alphabet mixed_keyword_ch) (= (contains mixed_keyword_unique mixed_keyword_ch) false)) (set! mixed_keyword_unique (conj mixed_keyword_unique mixed_keyword_ch))) (set! mixed_keyword_i (+ mixed_keyword_i 1)))) (set! mixed_keyword_num_unique (count mixed_keyword_unique)) (set! mixed_keyword_shifted []) (set! mixed_keyword_i 0) (while (< mixed_keyword_i (count mixed_keyword_unique)) (do (set! mixed_keyword_shifted (conj mixed_keyword_shifted (nth mixed_keyword_unique mixed_keyword_i))) (set! mixed_keyword_i (+ mixed_keyword_i 1)))) (set! mixed_keyword_i 0) (while (< mixed_keyword_i (count mixed_keyword_alphabet)) (do (set! mixed_keyword_ch (nth mixed_keyword_alphabet mixed_keyword_i)) (when (= (contains mixed_keyword_unique mixed_keyword_ch) false) (set! mixed_keyword_shifted (conj mixed_keyword_shifted mixed_keyword_ch))) (set! mixed_keyword_i (+ mixed_keyword_i 1)))) (set! mixed_keyword_modified []) (set! mixed_keyword_k 0) (while (< mixed_keyword_k (count mixed_keyword_shifted)) (do (set! mixed_keyword_row []) (set! mixed_keyword_r 0) (while (and (< mixed_keyword_r mixed_keyword_num_unique) (< (+ mixed_keyword_k mixed_keyword_r) (count mixed_keyword_shifted))) (do (set! mixed_keyword_row (conj mixed_keyword_row (nth mixed_keyword_shifted (+ mixed_keyword_k mixed_keyword_r)))) (set! mixed_keyword_r (+ mixed_keyword_r 1)))) (set! mixed_keyword_modified (conj mixed_keyword_modified mixed_keyword_row)) (set! mixed_keyword_k (+ mixed_keyword_k mixed_keyword_num_unique)))) (set! mixed_keyword_keys []) (set! mixed_keyword_values []) (set! mixed_keyword_column 0) (set! mixed_keyword_letter_index 0) (while (< mixed_keyword_column mixed_keyword_num_unique) (do (set! mixed_keyword_row_idx 0) (loop [while_flag_2 true] (when (and while_flag_2 (< mixed_keyword_row_idx (count mixed_keyword_modified))) (do (set! mixed_keyword_row (nth mixed_keyword_modified mixed_keyword_row_idx)) (cond (<= (count mixed_keyword_row) mixed_keyword_column) (recur false) :else (do (set! mixed_keyword_keys (conj mixed_keyword_keys (nth mixed_keyword_alphabet mixed_keyword_letter_index))) (set! mixed_keyword_values (conj mixed_keyword_values (nth mixed_keyword_row mixed_keyword_column))) (set! mixed_keyword_letter_index (+ mixed_keyword_letter_index 1)) (set! mixed_keyword_row_idx (+ mixed_keyword_row_idx 1)) (recur while_flag_2)))))) (set! mixed_keyword_column (+ mixed_keyword_column 1)))) (when mixed_keyword_verbose (print_mapping mixed_keyword_keys mixed_keyword_values)) (set! mixed_keyword_result "") (set! mixed_keyword_i 0) (while (< mixed_keyword_i (count mixed_keyword_plaintext_u)) (do (set! mixed_keyword_ch (nth mixed_keyword_plaintext_u mixed_keyword_i)) (set! mixed_keyword_mapped (get_value mixed_keyword_keys mixed_keyword_values mixed_keyword_ch)) (if (= mixed_keyword_mapped nil) (set! mixed_keyword_result (str mixed_keyword_result mixed_keyword_ch)) (set! mixed_keyword_result (str mixed_keyword_result mixed_keyword_mapped))) (set! mixed_keyword_i (+ mixed_keyword_i 1)))) (throw (ex-info "return" {:v mixed_keyword_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (mixed_keyword "college" "UNIVERSITY" true))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
