(ns main (:refer-clojure :exclude [min_int minimum_cost_path]))

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

(declare min_int minimum_cost_path)

(def ^:dynamic minimum_cost_path_best nil)

(def ^:dynamic minimum_cost_path_cols nil)

(def ^:dynamic minimum_cost_path_i nil)

(def ^:dynamic minimum_cost_path_j nil)

(def ^:dynamic minimum_cost_path_left nil)

(def ^:dynamic minimum_cost_path_matrix nil)

(def ^:dynamic minimum_cost_path_row nil)

(def ^:dynamic minimum_cost_path_row0 nil)

(def ^:dynamic minimum_cost_path_rows nil)

(def ^:dynamic minimum_cost_path_up nil)

(defn min_int [min_int_a min_int_b]
  (try (if (< min_int_a min_int_b) min_int_a min_int_b) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn minimum_cost_path [minimum_cost_path_matrix_p]
  (binding [minimum_cost_path_best nil minimum_cost_path_cols nil minimum_cost_path_i nil minimum_cost_path_j nil minimum_cost_path_left nil minimum_cost_path_matrix nil minimum_cost_path_row nil minimum_cost_path_row0 nil minimum_cost_path_rows nil minimum_cost_path_up nil] (try (do (set! minimum_cost_path_matrix minimum_cost_path_matrix_p) (set! minimum_cost_path_rows (count minimum_cost_path_matrix)) (set! minimum_cost_path_cols (count (nth minimum_cost_path_matrix 0))) (set! minimum_cost_path_j 1) (while (< minimum_cost_path_j minimum_cost_path_cols) (do (set! minimum_cost_path_row0 (nth minimum_cost_path_matrix 0)) (set! minimum_cost_path_row0 (assoc minimum_cost_path_row0 minimum_cost_path_j (+ (nth minimum_cost_path_row0 minimum_cost_path_j) (nth minimum_cost_path_row0 (- minimum_cost_path_j 1))))) (set! minimum_cost_path_matrix (assoc minimum_cost_path_matrix 0 minimum_cost_path_row0)) (set! minimum_cost_path_j (+ minimum_cost_path_j 1)))) (set! minimum_cost_path_i 1) (while (< minimum_cost_path_i minimum_cost_path_rows) (do (set! minimum_cost_path_row (nth minimum_cost_path_matrix minimum_cost_path_i)) (set! minimum_cost_path_row (assoc minimum_cost_path_row 0 (+ (nth minimum_cost_path_row 0) (nth (nth minimum_cost_path_matrix (- minimum_cost_path_i 1)) 0)))) (set! minimum_cost_path_matrix (assoc minimum_cost_path_matrix minimum_cost_path_i minimum_cost_path_row)) (set! minimum_cost_path_i (+ minimum_cost_path_i 1)))) (set! minimum_cost_path_i 1) (while (< minimum_cost_path_i minimum_cost_path_rows) (do (set! minimum_cost_path_row (nth minimum_cost_path_matrix minimum_cost_path_i)) (set! minimum_cost_path_j 1) (while (< minimum_cost_path_j minimum_cost_path_cols) (do (set! minimum_cost_path_up (nth (nth minimum_cost_path_matrix (- minimum_cost_path_i 1)) minimum_cost_path_j)) (set! minimum_cost_path_left (nth minimum_cost_path_row (- minimum_cost_path_j 1))) (set! minimum_cost_path_best (min_int minimum_cost_path_up minimum_cost_path_left)) (set! minimum_cost_path_row (assoc minimum_cost_path_row minimum_cost_path_j (+ (nth minimum_cost_path_row minimum_cost_path_j) minimum_cost_path_best))) (set! minimum_cost_path_j (+ minimum_cost_path_j 1)))) (set! minimum_cost_path_matrix (assoc minimum_cost_path_matrix minimum_cost_path_i minimum_cost_path_row)) (set! minimum_cost_path_i (+ minimum_cost_path_i 1)))) (throw (ex-info "return" {:v (nth (nth minimum_cost_path_matrix (- minimum_cost_path_rows 1)) (- minimum_cost_path_cols 1))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_m1 [[2 1] [3 1] [4 2]])

(def ^:dynamic main_m2 [[2 1 4] [2 1 3] [3 2 1]])

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (minimum_cost_path main_m1)))
      (println (str (minimum_cost_path main_m2)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
