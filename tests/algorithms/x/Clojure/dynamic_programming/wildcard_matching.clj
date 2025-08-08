(ns main (:refer-clojure :exclude [make_bool_list make_bool_matrix is_match print_bool]))

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

(declare make_bool_list make_bool_matrix is_match print_bool)

(def ^:dynamic is_match_dp nil)

(def ^:dynamic is_match_i nil)

(def ^:dynamic is_match_j nil)

(def ^:dynamic is_match_j2 nil)

(def ^:dynamic is_match_m nil)

(def ^:dynamic is_match_n nil)

(def ^:dynamic is_match_pc nil)

(def ^:dynamic is_match_sc nil)

(def ^:dynamic make_bool_list_i nil)

(def ^:dynamic make_bool_list_row nil)

(def ^:dynamic make_bool_matrix_i nil)

(def ^:dynamic make_bool_matrix_matrix nil)

(defn make_bool_list [make_bool_list_n]
  (binding [make_bool_list_i nil make_bool_list_row nil] (try (do (set! make_bool_list_row []) (set! make_bool_list_i 0) (while (< make_bool_list_i make_bool_list_n) (do (set! make_bool_list_row (conj make_bool_list_row false)) (set! make_bool_list_i (+ make_bool_list_i 1)))) (throw (ex-info "return" {:v make_bool_list_row}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn make_bool_matrix [make_bool_matrix_rows make_bool_matrix_cols]
  (binding [make_bool_matrix_i nil make_bool_matrix_matrix nil] (try (do (set! make_bool_matrix_matrix []) (set! make_bool_matrix_i 0) (while (< make_bool_matrix_i make_bool_matrix_rows) (do (set! make_bool_matrix_matrix (conj make_bool_matrix_matrix (make_bool_list make_bool_matrix_cols))) (set! make_bool_matrix_i (+ make_bool_matrix_i 1)))) (throw (ex-info "return" {:v make_bool_matrix_matrix}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn is_match [is_match_s is_match_p]
  (binding [is_match_dp nil is_match_i nil is_match_j nil is_match_j2 nil is_match_m nil is_match_n nil is_match_pc nil is_match_sc nil] (try (do (set! is_match_n (count is_match_s)) (set! is_match_m (count is_match_p)) (set! is_match_dp (make_bool_matrix (+ is_match_n 1) (+ is_match_m 1))) (set! is_match_dp (assoc-in is_match_dp [0 0] true)) (set! is_match_j 1) (while (<= is_match_j is_match_m) (do (when (= (subs is_match_p (- is_match_j 1) (min is_match_j (count is_match_p))) "*") (set! is_match_dp (assoc-in is_match_dp [0 is_match_j] (nth (nth is_match_dp 0) (- is_match_j 1))))) (set! is_match_j (+ is_match_j 1)))) (set! is_match_i 1) (while (<= is_match_i is_match_n) (do (set! is_match_j2 1) (while (<= is_match_j2 is_match_m) (do (set! is_match_pc (subs is_match_p (- is_match_j2 1) (min is_match_j2 (count is_match_p)))) (set! is_match_sc (subs is_match_s (- is_match_i 1) (min is_match_i (count is_match_s)))) (if (or (= is_match_pc is_match_sc) (= is_match_pc "?")) (set! is_match_dp (assoc-in is_match_dp [is_match_i is_match_j2] (nth (nth is_match_dp (- is_match_i 1)) (- is_match_j2 1)))) (when (= is_match_pc "*") (when (or (nth (nth is_match_dp (- is_match_i 1)) is_match_j2) (nth (nth is_match_dp is_match_i) (- is_match_j2 1))) (set! is_match_dp (assoc-in is_match_dp [is_match_i is_match_j2] true))))) (set! is_match_j2 (+ is_match_j2 1)))) (set! is_match_i (+ is_match_i 1)))) (throw (ex-info "return" {:v (nth (nth is_match_dp is_match_n) is_match_m)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn print_bool [print_bool_b]
  (if print_bool_b (println true) (println false)))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (print_bool (is_match "abc" "a*c"))
      (print_bool (is_match "abc" "a*d"))
      (print_bool (is_match "baaabab" "*****ba*****ab"))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
