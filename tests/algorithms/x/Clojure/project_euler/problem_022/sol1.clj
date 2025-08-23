(ns main (:refer-clojure :exclude [parse_names insertion_sort letter_value name_score main]))

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

(declare parse_names insertion_sort letter_value name_score main)

(declare _read_file)

(def ^:dynamic insertion_sort_a nil)

(def ^:dynamic insertion_sort_i nil)

(def ^:dynamic insertion_sort_j nil)

(def ^:dynamic insertion_sort_key nil)

(def ^:dynamic letter_value_alphabet nil)

(def ^:dynamic letter_value_idx nil)

(def ^:dynamic main_i nil)

(def ^:dynamic main_line nil)

(def ^:dynamic main_names nil)

(def ^:dynamic main_total nil)

(def ^:dynamic name_score_i nil)

(def ^:dynamic name_score_score nil)

(def ^:dynamic parse_names_ch nil)

(def ^:dynamic parse_names_current nil)

(def ^:dynamic parse_names_i nil)

(def ^:dynamic parse_names_names nil)

(defn parse_names [parse_names_line]
  (binding [parse_names_ch nil parse_names_current nil parse_names_i nil parse_names_names nil] (try (do (set! parse_names_names []) (set! parse_names_current "") (set! parse_names_i 0) (while (< parse_names_i (count parse_names_line)) (do (set! parse_names_ch (subs parse_names_line parse_names_i (min (+ parse_names_i 1) (count parse_names_line)))) (if (= parse_names_ch ",") (do (set! parse_names_names (conj parse_names_names parse_names_current)) (set! parse_names_current "")) (when (not= parse_names_ch "\"") (set! parse_names_current (str parse_names_current parse_names_ch)))) (set! parse_names_i (+ parse_names_i 1)))) (set! parse_names_names (conj parse_names_names parse_names_current)) (throw (ex-info "return" {:v parse_names_names}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn insertion_sort [insertion_sort_arr]
  (binding [insertion_sort_a nil insertion_sort_i nil insertion_sort_j nil insertion_sort_key nil] (try (do (set! insertion_sort_a insertion_sort_arr) (set! insertion_sort_i 1) (while (< insertion_sort_i (count insertion_sort_a)) (do (set! insertion_sort_key (nth insertion_sort_a insertion_sort_i)) (set! insertion_sort_j (- insertion_sort_i 1)) (while (and (>= insertion_sort_j 0) (> (compare (nth insertion_sort_a insertion_sort_j) insertion_sort_key) 0)) (do (set! insertion_sort_a (assoc insertion_sort_a (+ insertion_sort_j 1) (nth insertion_sort_a insertion_sort_j))) (set! insertion_sort_j (- insertion_sort_j 1)))) (set! insertion_sort_a (assoc insertion_sort_a (+ insertion_sort_j 1) insertion_sort_key)) (set! insertion_sort_i (+ insertion_sort_i 1)))) (throw (ex-info "return" {:v insertion_sort_a}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn letter_value [letter_value_ch]
  (binding [letter_value_alphabet nil letter_value_idx nil] (try (do (set! letter_value_alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZ") (set! letter_value_idx 0) (while (< letter_value_idx (count letter_value_alphabet)) (do (when (= (subs letter_value_alphabet letter_value_idx (min (+ letter_value_idx 1) (count letter_value_alphabet))) letter_value_ch) (throw (ex-info "return" {:v (+ letter_value_idx 1)}))) (set! letter_value_idx (+ letter_value_idx 1)))) (throw (ex-info "return" {:v 0}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn name_score [name_score_name]
  (binding [name_score_i nil name_score_score nil] (try (do (set! name_score_score 0) (set! name_score_i 0) (while (< name_score_i (count name_score_name)) (do (set! name_score_score (+ name_score_score (letter_value (subs name_score_name name_score_i (min (+ name_score_i 1) (count name_score_name)))))) (set! name_score_i (+ name_score_i 1)))) (throw (ex-info "return" {:v name_score_score}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_i nil main_line nil main_names nil main_total nil] (do (set! main_line (read-line)) (set! main_names (insertion_sort (parse_names main_line))) (set! main_total 0) (set! main_i 0) (while (< main_i (count main_names)) (do (set! main_total (+ main_total (* (+ main_i 1) (name_score (nth main_names main_i))))) (set! main_i (+ main_i 1)))) (println (mochi_str main_total)))))

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
