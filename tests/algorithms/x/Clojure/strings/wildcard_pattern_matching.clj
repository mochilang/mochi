(ns main (:refer-clojure :exclude [make_matrix_bool match_pattern main]))

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

(declare make_matrix_bool match_pattern main)

(def ^:dynamic make_matrix_bool_matrix nil)

(def ^:dynamic make_matrix_bool_row nil)

(def ^:dynamic match_pattern_dp nil)

(def ^:dynamic match_pattern_i nil)

(def ^:dynamic match_pattern_j nil)

(def ^:dynamic match_pattern_j2 nil)

(def ^:dynamic match_pattern_len_pattern nil)

(def ^:dynamic match_pattern_len_string nil)

(def ^:dynamic match_pattern_p_char nil)

(def ^:dynamic match_pattern_prev_p nil)

(def ^:dynamic match_pattern_row nil)

(def ^:dynamic match_pattern_row0 nil)

(def ^:dynamic match_pattern_s_char nil)

(def ^:dynamic match_pattern_val nil)

(defn make_matrix_bool [make_matrix_bool_rows make_matrix_bool_cols make_matrix_bool_init]
  (binding [make_matrix_bool_matrix nil make_matrix_bool_row nil] (try (do (set! make_matrix_bool_matrix []) (dotimes [_ make_matrix_bool_rows] (do (set! make_matrix_bool_row []) (dotimes [_2 make_matrix_bool_cols] (set! make_matrix_bool_row (conj make_matrix_bool_row make_matrix_bool_init))) (set! make_matrix_bool_matrix (conj make_matrix_bool_matrix make_matrix_bool_row)))) (throw (ex-info "return" {:v make_matrix_bool_matrix}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn match_pattern [match_pattern_input_string match_pattern_pattern]
  (binding [match_pattern_dp nil match_pattern_i nil match_pattern_j nil match_pattern_j2 nil match_pattern_len_pattern nil match_pattern_len_string nil match_pattern_p_char nil match_pattern_prev_p nil match_pattern_row nil match_pattern_row0 nil match_pattern_s_char nil match_pattern_val nil] (try (do (set! match_pattern_len_string (+ (count match_pattern_input_string) 1)) (set! match_pattern_len_pattern (+ (count match_pattern_pattern) 1)) (set! match_pattern_dp (make_matrix_bool match_pattern_len_string match_pattern_len_pattern false)) (set! match_pattern_row0 (nth match_pattern_dp 0)) (set! match_pattern_row0 (assoc match_pattern_row0 0 true)) (set! match_pattern_dp (assoc match_pattern_dp 0 match_pattern_row0)) (set! match_pattern_j 1) (while (< match_pattern_j match_pattern_len_pattern) (do (set! match_pattern_row0 (nth match_pattern_dp 0)) (if (= (subs match_pattern_pattern (- match_pattern_j 1) (min match_pattern_j (count match_pattern_pattern))) "*") (set! match_pattern_row0 (assoc match_pattern_row0 match_pattern_j (nth match_pattern_row0 (- match_pattern_j 2)))) (set! match_pattern_row0 (assoc match_pattern_row0 match_pattern_j false))) (set! match_pattern_dp (assoc match_pattern_dp 0 match_pattern_row0)) (set! match_pattern_j (+ match_pattern_j 1)))) (set! match_pattern_i 1) (while (< match_pattern_i match_pattern_len_string) (do (set! match_pattern_row (nth match_pattern_dp match_pattern_i)) (set! match_pattern_j2 1) (while (< match_pattern_j2 match_pattern_len_pattern) (do (set! match_pattern_s_char (subs match_pattern_input_string (- match_pattern_i 1) (min match_pattern_i (count match_pattern_input_string)))) (set! match_pattern_p_char (subs match_pattern_pattern (- match_pattern_j2 1) (min match_pattern_j2 (count match_pattern_pattern)))) (if (or (= match_pattern_s_char match_pattern_p_char) (= match_pattern_p_char ".")) (set! match_pattern_row (assoc match_pattern_row match_pattern_j2 (nth (nth match_pattern_dp (- match_pattern_i 1)) (- match_pattern_j2 1)))) (if (= match_pattern_p_char "*") (do (set! match_pattern_val (nth (nth match_pattern_dp match_pattern_i) (- match_pattern_j2 2))) (set! match_pattern_prev_p (subs match_pattern_pattern (- match_pattern_j2 2) (min (- match_pattern_j2 1) (count match_pattern_pattern)))) (when (and (not match_pattern_val) (or (= match_pattern_prev_p match_pattern_s_char) (= match_pattern_prev_p "."))) (set! match_pattern_val (nth (nth match_pattern_dp (- match_pattern_i 1)) match_pattern_j2))) (set! match_pattern_row (assoc match_pattern_row match_pattern_j2 match_pattern_val))) (set! match_pattern_row (assoc match_pattern_row match_pattern_j2 false)))) (set! match_pattern_j2 (+ match_pattern_j2 1)))) (set! match_pattern_dp (assoc match_pattern_dp match_pattern_i match_pattern_row)) (set! match_pattern_i (+ match_pattern_i 1)))) (throw (ex-info "return" {:v (nth (nth match_pattern_dp (- match_pattern_len_string 1)) (- match_pattern_len_pattern 1))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (do (when (not (match_pattern "aab" "c*a*b")) (throw (Exception. "case1 failed"))) (when (match_pattern "dabc" "*abc") (throw (Exception. "case2 failed"))) (when (match_pattern "aaa" "aa") (throw (Exception. "case3 failed"))) (when (not (match_pattern "aaa" "a.a")) (throw (Exception. "case4 failed"))) (when (match_pattern "aaab" "aa*") (throw (Exception. "case5 failed"))) (when (not (match_pattern "aaab" ".*")) (throw (Exception. "case6 failed"))) (when (match_pattern "a" "bbbb") (throw (Exception. "case7 failed"))) (when (match_pattern "" "bbbb") (throw (Exception. "case8 failed"))) (when (match_pattern "a" "") (throw (Exception. "case9 failed"))) (when (not (match_pattern "" "")) (throw (Exception. "case10 failed"))) (println (str (match_pattern "aab" "c*a*b"))) (println (str (match_pattern "dabc" "*abc"))) (println (str (match_pattern "aaa" "aa"))) (println (str (match_pattern "aaa" "a.a"))) (println (str (match_pattern "aaab" "aa*"))) (println (str (match_pattern "aaab" ".*"))) (println (str (match_pattern "a" "bbbb"))) (println (str (match_pattern "" "bbbb"))) (println (str (match_pattern "a" ""))) (println (str (match_pattern "" "")))))

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
