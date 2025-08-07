(ns main (:refer-clojure :exclude [compare_string contains_string unique_strings check decimal_to_binary is_for_table count_ones selection count_char prime_implicant_chart main]))

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

(declare compare_string contains_string unique_strings check decimal_to_binary is_for_table count_ones selection count_char prime_implicant_chart main)

(def ^:dynamic check_check1 nil)

(def ^:dynamic check_current nil)

(def ^:dynamic check_i nil)

(def ^:dynamic check_j nil)

(def ^:dynamic check_k nil)

(def ^:dynamic check_pi nil)

(def ^:dynamic check_temp nil)

(def ^:dynamic compare_string_c1 nil)

(def ^:dynamic compare_string_c2 nil)

(def ^:dynamic compare_string_i nil)

(def ^:dynamic compare_string_result nil)

(def ^:dynamic contains_string_i nil)

(def ^:dynamic count_char_cnt nil)

(def ^:dynamic count_char_i nil)

(def ^:dynamic count_ones_c nil)

(def ^:dynamic count_ones_j nil)

(def ^:dynamic count_v nil)

(def ^:dynamic decimal_to_binary_i nil)

(def ^:dynamic decimal_to_binary_idx nil)

(def ^:dynamic decimal_to_binary_minterm nil)

(def ^:dynamic decimal_to_binary_string nil)

(def ^:dynamic decimal_to_binary_temp nil)

(def ^:dynamic is_for_table_c1 nil)

(def ^:dynamic is_for_table_c2 nil)

(def ^:dynamic is_for_table_count_n nil)

(def ^:dynamic is_for_table_i nil)

(def ^:dynamic main_binary nil)

(def ^:dynamic main_chart nil)

(def ^:dynamic main_essential_prime_implicants nil)

(def ^:dynamic main_minterms nil)

(def ^:dynamic main_no_of_variable nil)

(def ^:dynamic main_prime_implicants nil)

(def ^:dynamic prime_implicant_chart_chart nil)

(def ^:dynamic prime_implicant_chart_i nil)

(def ^:dynamic prime_implicant_chart_j nil)

(def ^:dynamic prime_implicant_chart_row nil)

(def ^:dynamic selection_chart nil)

(def ^:dynamic selection_col nil)

(def ^:dynamic selection_counts nil)

(def ^:dynamic selection_i nil)

(def ^:dynamic selection_j nil)

(def ^:dynamic selection_k nil)

(def ^:dynamic selection_max_n nil)

(def ^:dynamic selection_r nil)

(def ^:dynamic selection_r2 nil)

(def ^:dynamic selection_rem nil)

(def ^:dynamic selection_row nil)

(def ^:dynamic selection_select nil)

(def ^:dynamic selection_temp nil)

(def ^:dynamic unique_strings_i nil)

(def ^:dynamic unique_strings_res nil)

(defn compare_string [compare_string_string1 compare_string_string2]
  (binding [compare_string_c1 nil compare_string_c2 nil compare_string_i nil compare_string_result nil count_v nil] (try (do (set! compare_string_result "") (set! count_v 0) (set! compare_string_i 0) (while (< compare_string_i (count compare_string_string1)) (do (set! compare_string_c1 (subs compare_string_string1 compare_string_i (min (+ compare_string_i 1) (count compare_string_string1)))) (set! compare_string_c2 (subs compare_string_string2 compare_string_i (min (+ compare_string_i 1) (count compare_string_string2)))) (if (not= compare_string_c1 compare_string_c2) (do (set! count_v (+ count_v 1)) (set! compare_string_result (str compare_string_result "_"))) (set! compare_string_result (str compare_string_result compare_string_c1))) (set! compare_string_i (+ compare_string_i 1)))) (if (> count_v 1) "" compare_string_result)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn contains_string [contains_string_arr contains_string_value]
  (binding [contains_string_i nil] (try (do (set! contains_string_i 0) (while (< contains_string_i (count contains_string_arr)) (do (when (= (nth contains_string_arr contains_string_i) contains_string_value) (throw (ex-info "return" {:v true}))) (set! contains_string_i (+ contains_string_i 1)))) (throw (ex-info "return" {:v false}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn unique_strings [unique_strings_arr]
  (binding [unique_strings_i nil unique_strings_res nil] (try (do (set! unique_strings_res []) (set! unique_strings_i 0) (while (< unique_strings_i (count unique_strings_arr)) (do (when (not (contains_string unique_strings_res (nth unique_strings_arr unique_strings_i))) (set! unique_strings_res (conj unique_strings_res (nth unique_strings_arr unique_strings_i)))) (set! unique_strings_i (+ unique_strings_i 1)))) (throw (ex-info "return" {:v unique_strings_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn check [check_binary]
  (binding [check_check1 nil check_current nil check_i nil check_j nil check_k nil check_pi nil check_temp nil] (try (do (set! check_pi []) (set! check_current check_binary) (while true (do (set! check_check1 []) (set! check_i 0) (while (< check_i (count check_current)) (do (set! check_check1 (conj check_check1 "$")) (set! check_i (+ check_i 1)))) (set! check_temp []) (set! check_i 0) (while (< check_i (count check_current)) (do (set! check_j (+ check_i 1)) (while (< check_j (count check_current)) (do (set! check_k (compare_string (nth check_current check_i) (nth check_current check_j))) (when (= check_k "") (do (set! check_check1 (assoc check_check1 check_i "*")) (set! check_check1 (assoc check_check1 check_j "*")) (set! check_temp (conj check_temp "X")))) (set! check_j (+ check_j 1)))) (set! check_i (+ check_i 1)))) (set! check_i 0) (while (< check_i (count check_current)) (do (when (= (nth check_check1 check_i) "$") (set! check_pi (conj check_pi (nth check_current check_i)))) (set! check_i (+ check_i 1)))) (when (= (count check_temp) 0) (throw (ex-info "return" {:v check_pi}))) (set! check_current (unique_strings check_temp))))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn decimal_to_binary [decimal_to_binary_no_of_variable decimal_to_binary_minterms]
  (binding [decimal_to_binary_i nil decimal_to_binary_idx nil decimal_to_binary_minterm nil decimal_to_binary_string nil decimal_to_binary_temp nil] (try (do (set! decimal_to_binary_temp []) (set! decimal_to_binary_idx 0) (while (< decimal_to_binary_idx (count decimal_to_binary_minterms)) (do (set! decimal_to_binary_minterm (nth decimal_to_binary_minterms decimal_to_binary_idx)) (set! decimal_to_binary_string "") (set! decimal_to_binary_i 0) (while (< decimal_to_binary_i decimal_to_binary_no_of_variable) (do (set! decimal_to_binary_string (str (str (mod decimal_to_binary_minterm 2)) decimal_to_binary_string)) (set! decimal_to_binary_minterm (quot decimal_to_binary_minterm 2)) (set! decimal_to_binary_i (+ decimal_to_binary_i 1)))) (set! decimal_to_binary_temp (conj decimal_to_binary_temp decimal_to_binary_string)) (set! decimal_to_binary_idx (+ decimal_to_binary_idx 1)))) (throw (ex-info "return" {:v decimal_to_binary_temp}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn is_for_table [is_for_table_string1 is_for_table_string2 count_v]
  (binding [is_for_table_c1 nil is_for_table_c2 nil is_for_table_count_n nil is_for_table_i nil] (try (do (set! is_for_table_count_n 0) (set! is_for_table_i 0) (while (< is_for_table_i (count is_for_table_string1)) (do (set! is_for_table_c1 (subs is_for_table_string1 is_for_table_i (min (+ is_for_table_i 1) (count is_for_table_string1)))) (set! is_for_table_c2 (subs is_for_table_string2 is_for_table_i (min (+ is_for_table_i 1) (count is_for_table_string2)))) (when (not= is_for_table_c1 is_for_table_c2) (set! is_for_table_count_n (+ is_for_table_count_n 1))) (set! is_for_table_i (+ is_for_table_i 1)))) (throw (ex-info "return" {:v (= is_for_table_count_n count_v)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn count_ones [count_ones_row]
  (binding [count_ones_c nil count_ones_j nil] (try (do (set! count_ones_c 0) (set! count_ones_j 0) (while (< count_ones_j (count count_ones_row)) (do (when (= (nth count_ones_row count_ones_j) 1) (set! count_ones_c (+ count_ones_c 1))) (set! count_ones_j (+ count_ones_j 1)))) (throw (ex-info "return" {:v count_ones_c}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn selection [selection_chart_p selection_prime_implicants]
  (binding [count_v nil selection_chart nil selection_col nil selection_counts nil selection_i nil selection_j nil selection_k nil selection_max_n nil selection_r nil selection_r2 nil selection_rem nil selection_row nil selection_select nil selection_temp nil] (try (do (set! selection_chart selection_chart_p) (set! selection_temp []) (set! selection_select []) (set! selection_i 0) (while (< selection_i (count selection_chart)) (do (set! selection_select (conj selection_select 0)) (set! selection_i (+ selection_i 1)))) (set! selection_col 0) (while (< selection_col (count (nth selection_chart 0))) (do (set! count_v 0) (set! selection_row 0) (while (< selection_row (count selection_chart)) (do (when (= (nth (nth selection_chart selection_row) selection_col) 1) (set! count_v (+ count_v 1))) (set! selection_row (+ selection_row 1)))) (when (= count_v 1) (do (set! selection_rem 0) (set! selection_row 0) (while (< selection_row (count selection_chart)) (do (when (= (nth (nth selection_chart selection_row) selection_col) 1) (set! selection_rem selection_row)) (set! selection_row (+ selection_row 1)))) (set! selection_select (assoc selection_select selection_rem 1)))) (set! selection_col (+ selection_col 1)))) (set! selection_i 0) (while (< selection_i (count selection_select)) (do (when (= (nth selection_select selection_i) 1) (do (set! selection_j 0) (while (< selection_j (count (nth selection_chart 0))) (do (when (= (nth (nth selection_chart selection_i) selection_j) 1) (do (set! selection_r 0) (while (< selection_r (count selection_chart)) (do (set! selection_chart (assoc-in selection_chart [selection_r selection_j] 0)) (set! selection_r (+ selection_r 1)))))) (set! selection_j (+ selection_j 1)))) (set! selection_temp (conj selection_temp (nth selection_prime_implicants selection_i))))) (set! selection_i (+ selection_i 1)))) (while true (do (set! selection_counts []) (set! selection_r 0) (while (< selection_r (count selection_chart)) (do (set! selection_counts (conj selection_counts (count_ones (nth selection_chart selection_r)))) (set! selection_r (+ selection_r 1)))) (set! selection_max_n (nth selection_counts 0)) (set! selection_rem 0) (set! selection_k 1) (while (< selection_k (count selection_counts)) (do (when (> (nth selection_counts selection_k) selection_max_n) (do (set! selection_max_n (nth selection_counts selection_k)) (set! selection_rem selection_k))) (set! selection_k (+ selection_k 1)))) (when (= selection_max_n 0) (throw (ex-info "return" {:v selection_temp}))) (set! selection_temp (conj selection_temp (nth selection_prime_implicants selection_rem))) (set! selection_j 0) (while (< selection_j (count (nth selection_chart 0))) (do (when (= (nth (nth selection_chart selection_rem) selection_j) 1) (do (set! selection_r2 0) (while (< selection_r2 (count selection_chart)) (do (set! selection_chart (assoc-in selection_chart [selection_r2 selection_j] 0)) (set! selection_r2 (+ selection_r2 1)))))) (set! selection_j (+ selection_j 1))))))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn count_char [count_char_s count_char_ch]
  (binding [count_char_cnt nil count_char_i nil] (try (do (set! count_char_cnt 0) (set! count_char_i 0) (while (< count_char_i (count count_char_s)) (do (when (= (subs count_char_s count_char_i (min (+ count_char_i 1) (count count_char_s))) count_char_ch) (set! count_char_cnt (+ count_char_cnt 1))) (set! count_char_i (+ count_char_i 1)))) (throw (ex-info "return" {:v count_char_cnt}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn prime_implicant_chart [prime_implicant_chart_prime_implicants prime_implicant_chart_binary]
  (binding [count_v nil prime_implicant_chart_chart nil prime_implicant_chart_i nil prime_implicant_chart_j nil prime_implicant_chart_row nil] (try (do (set! prime_implicant_chart_chart []) (set! prime_implicant_chart_i 0) (while (< prime_implicant_chart_i (count prime_implicant_chart_prime_implicants)) (do (set! prime_implicant_chart_row []) (set! prime_implicant_chart_j 0) (while (< prime_implicant_chart_j (count prime_implicant_chart_binary)) (do (set! prime_implicant_chart_row (conj prime_implicant_chart_row 0)) (set! prime_implicant_chart_j (+ prime_implicant_chart_j 1)))) (set! prime_implicant_chart_chart (conj prime_implicant_chart_chart prime_implicant_chart_row)) (set! prime_implicant_chart_i (+ prime_implicant_chart_i 1)))) (set! prime_implicant_chart_i 0) (while (< prime_implicant_chart_i (count prime_implicant_chart_prime_implicants)) (do (set! count_v (count_char (nth prime_implicant_chart_prime_implicants prime_implicant_chart_i) "_")) (set! prime_implicant_chart_j 0) (while (< prime_implicant_chart_j (count prime_implicant_chart_binary)) (do (when (is_for_table (nth prime_implicant_chart_prime_implicants prime_implicant_chart_i) (nth prime_implicant_chart_binary prime_implicant_chart_j) count_v) (set! prime_implicant_chart_chart (assoc-in prime_implicant_chart_chart [prime_implicant_chart_i prime_implicant_chart_j] 1))) (set! prime_implicant_chart_j (+ prime_implicant_chart_j 1)))) (set! prime_implicant_chart_i (+ prime_implicant_chart_i 1)))) (throw (ex-info "return" {:v prime_implicant_chart_chart}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_binary nil main_chart nil main_essential_prime_implicants nil main_minterms nil main_no_of_variable nil main_prime_implicants nil] (do (set! main_no_of_variable 3) (set! main_minterms [1 5 7]) (set! main_binary (decimal_to_binary main_no_of_variable main_minterms)) (set! main_prime_implicants (check main_binary)) (println "Prime Implicants are:") (println (str main_prime_implicants)) (set! main_chart (prime_implicant_chart main_prime_implicants main_binary)) (set! main_essential_prime_implicants (selection main_chart main_prime_implicants)) (println "Essential Prime Implicants are:") (println (str main_essential_prime_implicants)))))

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
