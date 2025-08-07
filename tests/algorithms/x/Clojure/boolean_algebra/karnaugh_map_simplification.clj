(ns main (:refer-clojure :exclude [row_string print_kmap join_terms simplify_kmap]))

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

(declare row_string print_kmap join_terms simplify_kmap)

(def ^:dynamic join_terms_i nil)

(def ^:dynamic join_terms_res nil)

(def ^:dynamic print_kmap_i nil)

(def ^:dynamic row_string_i nil)

(def ^:dynamic row_string_s nil)

(def ^:dynamic simplify_kmap_a nil)

(def ^:dynamic simplify_kmap_b nil)

(def ^:dynamic simplify_kmap_expr nil)

(def ^:dynamic simplify_kmap_item nil)

(def ^:dynamic simplify_kmap_row nil)

(def ^:dynamic simplify_kmap_term nil)

(def ^:dynamic simplify_kmap_terms nil)

(defn row_string [row_string_row]
  (binding [row_string_i nil row_string_s nil] (try (do (set! row_string_s "[") (set! row_string_i 0) (while (< row_string_i (count row_string_row)) (do (set! row_string_s (str row_string_s (str (nth row_string_row row_string_i)))) (when (< row_string_i (- (count row_string_row) 1)) (set! row_string_s (str row_string_s ", "))) (set! row_string_i (+ row_string_i 1)))) (set! row_string_s (str row_string_s "]")) (throw (ex-info "return" {:v row_string_s}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn print_kmap [print_kmap_kmap]
  (binding [print_kmap_i nil] (do (set! print_kmap_i 0) (while (< print_kmap_i (count print_kmap_kmap)) (do (println (row_string (nth print_kmap_kmap print_kmap_i))) (set! print_kmap_i (+ print_kmap_i 1)))))))

(defn join_terms [join_terms_terms]
  (binding [join_terms_i nil join_terms_res nil] (try (do (when (= (count join_terms_terms) 0) (throw (ex-info "return" {:v ""}))) (set! join_terms_res (nth join_terms_terms 0)) (set! join_terms_i 1) (while (< join_terms_i (count join_terms_terms)) (do (set! join_terms_res (str (str join_terms_res " + ") (nth join_terms_terms join_terms_i))) (set! join_terms_i (+ join_terms_i 1)))) (throw (ex-info "return" {:v join_terms_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn simplify_kmap [simplify_kmap_board]
  (binding [simplify_kmap_a nil simplify_kmap_b nil simplify_kmap_expr nil simplify_kmap_item nil simplify_kmap_row nil simplify_kmap_term nil simplify_kmap_terms nil] (try (do (set! simplify_kmap_terms []) (set! simplify_kmap_a 0) (while (< simplify_kmap_a (count simplify_kmap_board)) (do (set! simplify_kmap_row (nth simplify_kmap_board simplify_kmap_a)) (set! simplify_kmap_b 0) (while (< simplify_kmap_b (count simplify_kmap_row)) (do (set! simplify_kmap_item (nth simplify_kmap_row simplify_kmap_b)) (when (not= simplify_kmap_item 0) (do (set! simplify_kmap_term (+ (if (not= simplify_kmap_a 0) "A" "A'") (if (not= simplify_kmap_b 0) "B" "B'"))) (set! simplify_kmap_terms (conj simplify_kmap_terms simplify_kmap_term)))) (set! simplify_kmap_b (+ simplify_kmap_b 1)))) (set! simplify_kmap_a (+ simplify_kmap_a 1)))) (set! simplify_kmap_expr (join_terms simplify_kmap_terms)) (throw (ex-info "return" {:v simplify_kmap_expr}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_kmap [[0 1] [1 1]])

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (print_kmap main_kmap)
      (println "Simplified Expression:")
      (println (simplify_kmap main_kmap))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
