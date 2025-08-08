(ns main (:refer-clojure :exclude [score_function smith_waterman traceback]))

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

(declare score_function smith_waterman traceback)

(def ^:dynamic smith_waterman_delete nil)

(def ^:dynamic smith_waterman_diag nil)

(def ^:dynamic smith_waterman_insert nil)

(def ^:dynamic smith_waterman_m nil)

(def ^:dynamic smith_waterman_max_val nil)

(def ^:dynamic smith_waterman_n nil)

(def ^:dynamic smith_waterman_q nil)

(def ^:dynamic smith_waterman_qc nil)

(def ^:dynamic smith_waterman_row nil)

(def ^:dynamic smith_waterman_s nil)

(def ^:dynamic smith_waterman_sc nil)

(def ^:dynamic smith_waterman_score nil)

(def ^:dynamic traceback_align1 nil)

(def ^:dynamic traceback_align2 nil)

(def ^:dynamic traceback_gap_penalty nil)

(def ^:dynamic traceback_i nil)

(def ^:dynamic traceback_i_max nil)

(def ^:dynamic traceback_j nil)

(def ^:dynamic traceback_j_max nil)

(def ^:dynamic traceback_max_value nil)

(def ^:dynamic traceback_q nil)

(def ^:dynamic traceback_qc nil)

(def ^:dynamic traceback_s nil)

(def ^:dynamic traceback_sc nil)

(defn score_function [score_function_source_char score_function_target_char score_function_match_score score_function_mismatch_score score_function_gap_score]
  (try (do (when (or (= score_function_source_char "-") (= score_function_target_char "-")) (throw (ex-info "return" {:v score_function_gap_score}))) (if (= score_function_source_char score_function_target_char) score_function_match_score score_function_mismatch_score)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn smith_waterman [smith_waterman_query smith_waterman_subject smith_waterman_match_score smith_waterman_mismatch_score smith_waterman_gap_score]
  (binding [smith_waterman_delete nil smith_waterman_diag nil smith_waterman_insert nil smith_waterman_m nil smith_waterman_max_val nil smith_waterman_n nil smith_waterman_q nil smith_waterman_qc nil smith_waterman_row nil smith_waterman_s nil smith_waterman_sc nil smith_waterman_score nil] (try (do (set! smith_waterman_q (clojure.string/upper-case smith_waterman_query)) (set! smith_waterman_s (clojure.string/upper-case smith_waterman_subject)) (set! smith_waterman_m (count smith_waterman_q)) (set! smith_waterman_n (count smith_waterman_s)) (set! smith_waterman_score []) (dotimes [_ (+ smith_waterman_m 1)] (do (set! smith_waterman_row []) (dotimes [_2 (+ smith_waterman_n 1)] (set! smith_waterman_row (conj smith_waterman_row 0))) (set! smith_waterman_score (conj smith_waterman_score smith_waterman_row)))) (doseq [i (range 1 (+ smith_waterman_m 1))] (doseq [j (range 1 (+ smith_waterman_n 1))] (do (set! smith_waterman_qc (subs smith_waterman_q (- i 1) (min i (count smith_waterman_q)))) (set! smith_waterman_sc (subs smith_waterman_s (- j 1) (min j (count smith_waterman_s)))) (set! smith_waterman_diag (+ (nth (nth smith_waterman_score (- i 1)) (- j 1)) (score_function smith_waterman_qc smith_waterman_sc smith_waterman_match_score smith_waterman_mismatch_score smith_waterman_gap_score))) (set! smith_waterman_delete (+ (nth (nth smith_waterman_score (- i 1)) j) smith_waterman_gap_score)) (set! smith_waterman_insert (+ (nth (nth smith_waterman_score i) (- j 1)) smith_waterman_gap_score)) (set! smith_waterman_max_val 0) (when (> smith_waterman_diag smith_waterman_max_val) (set! smith_waterman_max_val smith_waterman_diag)) (when (> smith_waterman_delete smith_waterman_max_val) (set! smith_waterman_max_val smith_waterman_delete)) (when (> smith_waterman_insert smith_waterman_max_val) (set! smith_waterman_max_val smith_waterman_insert)) (set! smith_waterman_score (assoc-in smith_waterman_score [i j] smith_waterman_max_val))))) (throw (ex-info "return" {:v smith_waterman_score}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn traceback [traceback_score traceback_query traceback_subject traceback_match_score traceback_mismatch_score traceback_gap_score]
  (binding [traceback_align1 nil traceback_align2 nil traceback_gap_penalty nil traceback_i nil traceback_i_max nil traceback_j nil traceback_j_max nil traceback_max_value nil traceback_q nil traceback_qc nil traceback_s nil traceback_sc nil] (try (do (set! traceback_q (clojure.string/upper-case traceback_query)) (set! traceback_s (clojure.string/upper-case traceback_subject)) (set! traceback_max_value 0) (set! traceback_i_max 0) (set! traceback_j_max 0) (dotimes [i (count traceback_score)] (dotimes [j (count (nth traceback_score i))] (when (> (nth (nth traceback_score i) j) traceback_max_value) (do (set! traceback_max_value (nth (nth traceback_score i) j)) (set! traceback_i_max i) (set! traceback_j_max j))))) (set! traceback_i traceback_i_max) (set! traceback_j traceback_j_max) (set! traceback_align1 "") (set! traceback_align2 "") (set! traceback_gap_penalty (score_function "-" "-" traceback_match_score traceback_mismatch_score traceback_gap_score)) (when (or (= traceback_i 0) (= traceback_j 0)) (throw (ex-info "return" {:v ""}))) (while (and (> traceback_i 0) (> traceback_j 0)) (do (set! traceback_qc (subs traceback_q (- traceback_i 1) (min traceback_i (count traceback_q)))) (set! traceback_sc (subs traceback_s (- traceback_j 1) (min traceback_j (count traceback_s)))) (if (= (nth (nth traceback_score traceback_i) traceback_j) (+ (nth (nth traceback_score (- traceback_i 1)) (- traceback_j 1)) (score_function traceback_qc traceback_sc traceback_match_score traceback_mismatch_score traceback_gap_score))) (do (set! traceback_align1 (str traceback_qc traceback_align1)) (set! traceback_align2 (str traceback_sc traceback_align2)) (set! traceback_i (- traceback_i 1)) (set! traceback_j (- traceback_j 1))) (if (= (nth (nth traceback_score traceback_i) traceback_j) (+ (nth (nth traceback_score (- traceback_i 1)) traceback_j) traceback_gap_penalty)) (do (set! traceback_align1 (str traceback_qc traceback_align1)) (set! traceback_align2 (str "-" traceback_align2)) (set! traceback_i (- traceback_i 1))) (do (set! traceback_align1 (str "-" traceback_align1)) (set! traceback_align2 (str traceback_sc traceback_align2)) (set! traceback_j (- traceback_j 1))))))) (throw (ex-info "return" {:v (str (str traceback_align1 "\n") traceback_align2)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_query "HEAGAWGHEE")

(def ^:dynamic main_subject "PAWHEAE")

(def ^:dynamic main_score (smith_waterman main_query main_subject 1 (- 1) (- 2)))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (traceback main_score main_query main_subject 1 (- 1) (- 2)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
