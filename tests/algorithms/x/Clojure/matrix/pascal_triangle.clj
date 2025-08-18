(ns main (:refer-clojure :exclude [populate_current_row generate_pascal_triangle row_to_string print_pascal_triangle main]))

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

(declare populate_current_row generate_pascal_triangle row_to_string print_pascal_triangle main)

(def ^:dynamic generate_pascal_triangle_row nil)

(def ^:dynamic generate_pascal_triangle_row_idx nil)

(def ^:dynamic generate_pascal_triangle_triangle nil)

(def ^:dynamic populate_current_row_i nil)

(def ^:dynamic populate_current_row_left nil)

(def ^:dynamic populate_current_row_right nil)

(def ^:dynamic populate_current_row_row nil)

(def ^:dynamic print_pascal_triangle_line nil)

(def ^:dynamic print_pascal_triangle_r nil)

(def ^:dynamic print_pascal_triangle_triangle nil)

(def ^:dynamic row_to_string_c nil)

(def ^:dynamic row_to_string_line nil)

(def ^:dynamic row_to_string_s nil)

(def ^:dynamic row_to_string_spaces nil)

(defn populate_current_row [populate_current_row_triangle populate_current_row_current_row_idx]
  (binding [populate_current_row_i nil populate_current_row_left nil populate_current_row_right nil populate_current_row_row nil] (try (do (set! populate_current_row_row []) (set! populate_current_row_i 0) (while (<= populate_current_row_i populate_current_row_current_row_idx) (do (if (or (= populate_current_row_i 0) (= populate_current_row_i populate_current_row_current_row_idx)) (set! populate_current_row_row (conj populate_current_row_row 1)) (do (set! populate_current_row_left (nth (nth populate_current_row_triangle (- populate_current_row_current_row_idx 1)) (- populate_current_row_i 1))) (set! populate_current_row_right (nth (nth populate_current_row_triangle (- populate_current_row_current_row_idx 1)) populate_current_row_i)) (set! populate_current_row_row (conj populate_current_row_row (+ populate_current_row_left populate_current_row_right))))) (set! populate_current_row_i (+ populate_current_row_i 1)))) (throw (ex-info "return" {:v populate_current_row_row}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn generate_pascal_triangle [generate_pascal_triangle_num_rows]
  (binding [generate_pascal_triangle_row nil generate_pascal_triangle_row_idx nil generate_pascal_triangle_triangle nil] (try (do (when (<= generate_pascal_triangle_num_rows 0) (throw (ex-info "return" {:v []}))) (set! generate_pascal_triangle_triangle []) (set! generate_pascal_triangle_row_idx 0) (while (< generate_pascal_triangle_row_idx generate_pascal_triangle_num_rows) (do (set! generate_pascal_triangle_row (populate_current_row generate_pascal_triangle_triangle generate_pascal_triangle_row_idx)) (set! generate_pascal_triangle_triangle (conj generate_pascal_triangle_triangle generate_pascal_triangle_row)) (set! generate_pascal_triangle_row_idx (+ generate_pascal_triangle_row_idx 1)))) (throw (ex-info "return" {:v generate_pascal_triangle_triangle}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn row_to_string [row_to_string_row row_to_string_total_rows row_to_string_row_idx]
  (binding [row_to_string_c nil row_to_string_line nil row_to_string_s nil row_to_string_spaces nil] (try (do (set! row_to_string_line "") (set! row_to_string_spaces (- (- row_to_string_total_rows row_to_string_row_idx) 1)) (set! row_to_string_s 0) (while (< row_to_string_s row_to_string_spaces) (do (set! row_to_string_line (str row_to_string_line " ")) (set! row_to_string_s (+ row_to_string_s 1)))) (set! row_to_string_c 0) (while (<= row_to_string_c row_to_string_row_idx) (do (set! row_to_string_line (str row_to_string_line (mochi_str (nth row_to_string_row row_to_string_c)))) (when (not= row_to_string_c row_to_string_row_idx) (set! row_to_string_line (str row_to_string_line " "))) (set! row_to_string_c (+ row_to_string_c 1)))) (throw (ex-info "return" {:v row_to_string_line}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn print_pascal_triangle [print_pascal_triangle_num_rows]
  (binding [print_pascal_triangle_line nil print_pascal_triangle_r nil print_pascal_triangle_triangle nil] (do (set! print_pascal_triangle_triangle (generate_pascal_triangle print_pascal_triangle_num_rows)) (set! print_pascal_triangle_r 0) (while (< print_pascal_triangle_r print_pascal_triangle_num_rows) (do (set! print_pascal_triangle_line (row_to_string (nth print_pascal_triangle_triangle print_pascal_triangle_r) print_pascal_triangle_num_rows print_pascal_triangle_r)) (println print_pascal_triangle_line) (set! print_pascal_triangle_r (+ print_pascal_triangle_r 1)))) print_pascal_triangle_num_rows)))

(defn main []
  (do (print_pascal_triangle 5) (println (mochi_str (generate_pascal_triangle 5)))))

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
