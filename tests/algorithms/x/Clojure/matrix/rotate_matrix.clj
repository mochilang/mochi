(ns main (:refer-clojure :exclude [abs_int make_matrix transpose reverse_row reverse_column rotate_90 rotate_180 rotate_270 row_to_string print_matrix]))

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

(declare abs_int make_matrix transpose reverse_row reverse_column rotate_90 rotate_180 rotate_270 row_to_string print_matrix)

(def ^:dynamic main_mat nil)

(def ^:dynamic make_matrix_mat nil)

(def ^:dynamic make_matrix_row nil)

(def ^:dynamic make_matrix_size nil)

(def ^:dynamic make_matrix_x nil)

(def ^:dynamic make_matrix_y nil)

(def ^:dynamic print_matrix_i nil)

(def ^:dynamic reverse_column_i nil)

(def ^:dynamic reverse_column_j nil)

(def ^:dynamic reverse_column_result nil)

(def ^:dynamic reverse_column_row nil)

(def ^:dynamic reverse_row_i nil)

(def ^:dynamic reverse_row_result nil)

(def ^:dynamic rotate_180_rc nil)

(def ^:dynamic rotate_180_rr nil)

(def ^:dynamic rotate_270_rc nil)

(def ^:dynamic rotate_270_t nil)

(def ^:dynamic rotate_90_rr nil)

(def ^:dynamic rotate_90_t nil)

(def ^:dynamic row_to_string_i nil)

(def ^:dynamic row_to_string_line nil)

(def ^:dynamic transpose_i nil)

(def ^:dynamic transpose_j nil)

(def ^:dynamic transpose_n nil)

(def ^:dynamic transpose_result nil)

(def ^:dynamic transpose_row nil)

(defn abs_int [abs_int_n]
  (try (if (< abs_int_n 0) (- abs_int_n) abs_int_n) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn make_matrix [make_matrix_row_size]
  (binding [make_matrix_mat nil make_matrix_row nil make_matrix_size nil make_matrix_x nil make_matrix_y nil] (try (do (set! make_matrix_size (abs_int make_matrix_row_size)) (when (= make_matrix_size 0) (set! make_matrix_size 4)) (set! make_matrix_mat []) (set! make_matrix_y 0) (while (< make_matrix_y make_matrix_size) (do (set! make_matrix_row []) (set! make_matrix_x 0) (while (< make_matrix_x make_matrix_size) (do (set! make_matrix_row (conj make_matrix_row (+ (+ 1 make_matrix_x) (* make_matrix_y make_matrix_size)))) (set! make_matrix_x (+ make_matrix_x 1)))) (set! make_matrix_mat (conj make_matrix_mat make_matrix_row)) (set! make_matrix_y (+ make_matrix_y 1)))) (throw (ex-info "return" {:v make_matrix_mat}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn transpose [transpose_mat]
  (binding [transpose_i nil transpose_j nil transpose_n nil transpose_result nil transpose_row nil] (try (do (set! transpose_n (count transpose_mat)) (set! transpose_result []) (set! transpose_i 0) (while (< transpose_i transpose_n) (do (set! transpose_row []) (set! transpose_j 0) (while (< transpose_j transpose_n) (do (set! transpose_row (conj transpose_row (nth (nth transpose_mat transpose_j) transpose_i))) (set! transpose_j (+ transpose_j 1)))) (set! transpose_result (conj transpose_result transpose_row)) (set! transpose_i (+ transpose_i 1)))) (throw (ex-info "return" {:v transpose_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn reverse_row [reverse_row_mat]
  (binding [reverse_row_i nil reverse_row_result nil] (try (do (set! reverse_row_result []) (set! reverse_row_i (- (count reverse_row_mat) 1)) (while (>= reverse_row_i 0) (do (set! reverse_row_result (conj reverse_row_result (nth reverse_row_mat reverse_row_i))) (set! reverse_row_i (- reverse_row_i 1)))) (throw (ex-info "return" {:v reverse_row_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn reverse_column [reverse_column_mat]
  (binding [reverse_column_i nil reverse_column_j nil reverse_column_result nil reverse_column_row nil] (try (do (set! reverse_column_result []) (set! reverse_column_i 0) (while (< reverse_column_i (count reverse_column_mat)) (do (set! reverse_column_row []) (set! reverse_column_j (- (count (nth reverse_column_mat reverse_column_i)) 1)) (while (>= reverse_column_j 0) (do (set! reverse_column_row (conj reverse_column_row (nth (nth reverse_column_mat reverse_column_i) reverse_column_j))) (set! reverse_column_j (- reverse_column_j 1)))) (set! reverse_column_result (conj reverse_column_result reverse_column_row)) (set! reverse_column_i (+ reverse_column_i 1)))) (throw (ex-info "return" {:v reverse_column_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn rotate_90 [rotate_90_mat]
  (binding [rotate_90_rr nil rotate_90_t nil] (try (do (set! rotate_90_t (transpose rotate_90_mat)) (set! rotate_90_rr (reverse_row rotate_90_t)) (throw (ex-info "return" {:v rotate_90_rr}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn rotate_180 [rotate_180_mat]
  (binding [rotate_180_rc nil rotate_180_rr nil] (try (do (set! rotate_180_rc (reverse_column rotate_180_mat)) (set! rotate_180_rr (reverse_row rotate_180_rc)) (throw (ex-info "return" {:v rotate_180_rr}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn rotate_270 [rotate_270_mat]
  (binding [rotate_270_rc nil rotate_270_t nil] (try (do (set! rotate_270_t (transpose rotate_270_mat)) (set! rotate_270_rc (reverse_column rotate_270_t)) (throw (ex-info "return" {:v rotate_270_rc}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn row_to_string [row_to_string_row]
  (binding [row_to_string_i nil row_to_string_line nil] (try (do (set! row_to_string_line "") (set! row_to_string_i 0) (while (< row_to_string_i (count row_to_string_row)) (do (if (= row_to_string_i 0) (set! row_to_string_line (mochi_str (nth row_to_string_row row_to_string_i))) (set! row_to_string_line (str (str row_to_string_line " ") (mochi_str (nth row_to_string_row row_to_string_i))))) (set! row_to_string_i (+ row_to_string_i 1)))) (throw (ex-info "return" {:v row_to_string_line}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn print_matrix [print_matrix_mat]
  (binding [print_matrix_i nil] (do (set! print_matrix_i 0) (while (< print_matrix_i (count print_matrix_mat)) (do (println (row_to_string (nth print_matrix_mat print_matrix_i))) (set! print_matrix_i (+ print_matrix_i 1)))) print_matrix_mat)))

(def ^:dynamic main_mat (make_matrix 4))

(def ^:dynamic main_r90 nil)

(def ^:dynamic main_r180 nil)

(def ^:dynamic main_r270 nil)

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println "\norigin:\n")
      (print_matrix main_mat)
      (println "\nrotate 90 counterclockwise:\n")
      (alter-var-root (var main_r90) (constantly (rotate_90 main_mat)))
      (print_matrix main_r90)
      (alter-var-root (var main_mat) (constantly (make_matrix 4)))
      (println "\norigin:\n")
      (print_matrix main_mat)
      (println "\nrotate 180:\n")
      (alter-var-root (var main_r180) (constantly (rotate_180 main_mat)))
      (print_matrix main_r180)
      (alter-var-root (var main_mat) (constantly (make_matrix 4)))
      (println "\norigin:\n")
      (print_matrix main_mat)
      (println "\nrotate 270 counterclockwise:\n")
      (alter-var-root (var main_r270) (constantly (rotate_270 main_mat)))
      (print_matrix main_r270)
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
