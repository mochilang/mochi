(ns main (:refer-clojure :exclude [run_maze solve_maze]))

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

(declare run_maze solve_maze)

(def ^:dynamic run_maze_block_flag nil)

(def ^:dynamic run_maze_lower_flag nil)

(def ^:dynamic run_maze_size nil)

(def ^:dynamic run_maze_sol nil)

(def ^:dynamic run_maze_upper_flag nil)

(def ^:dynamic solve_maze_i nil)

(def ^:dynamic solve_maze_j nil)

(def ^:dynamic solve_maze_row nil)

(def ^:dynamic solve_maze_size nil)

(def ^:dynamic solve_maze_sol nil)

(def ^:dynamic solve_maze_solved nil)

(defn run_maze [run_maze_maze run_maze_i run_maze_j run_maze_dr run_maze_dc run_maze_sol_p]
  (binding [run_maze_block_flag nil run_maze_lower_flag nil run_maze_size nil run_maze_sol nil run_maze_upper_flag nil] (try (do (set! run_maze_sol run_maze_sol_p) (set! run_maze_size (count run_maze_maze)) (when (and (and (= run_maze_i run_maze_dr) (= run_maze_j run_maze_dc)) (= (nth (nth run_maze_maze run_maze_i) run_maze_j) 0)) (do (set! run_maze_sol (assoc-in run_maze_sol [run_maze_i run_maze_j] 0)) (throw (ex-info "return" {:v true})))) (set! run_maze_lower_flag (and (>= run_maze_i 0) (>= run_maze_j 0))) (set! run_maze_upper_flag (and (< run_maze_i run_maze_size) (< run_maze_j run_maze_size))) (when (and run_maze_lower_flag run_maze_upper_flag) (do (set! run_maze_block_flag (and (= (nth (nth run_maze_sol run_maze_i) run_maze_j) 1) (= (nth (nth run_maze_maze run_maze_i) run_maze_j) 0))) (when run_maze_block_flag (do (set! run_maze_sol (assoc-in run_maze_sol [run_maze_i run_maze_j] 0)) (when (or (or (or (run_maze run_maze_maze (+ run_maze_i 1) run_maze_j run_maze_dr run_maze_dc run_maze_sol) (run_maze run_maze_maze run_maze_i (+ run_maze_j 1) run_maze_dr run_maze_dc run_maze_sol)) (run_maze run_maze_maze (- run_maze_i 1) run_maze_j run_maze_dr run_maze_dc run_maze_sol)) (run_maze run_maze_maze run_maze_i (- run_maze_j 1) run_maze_dr run_maze_dc run_maze_sol)) (throw (ex-info "return" {:v true}))) (set! run_maze_sol (assoc-in run_maze_sol [run_maze_i run_maze_j] 1)) (throw (ex-info "return" {:v false})))))) (throw (ex-info "return" {:v false}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn solve_maze [solve_maze_maze solve_maze_sr solve_maze_sc solve_maze_dr solve_maze_dc]
  (binding [solve_maze_i nil solve_maze_j nil solve_maze_row nil solve_maze_size nil solve_maze_sol nil solve_maze_solved nil] (try (do (set! solve_maze_size (count solve_maze_maze)) (when (not (and (and (and (and (and (and (and (<= 0 solve_maze_sr) (< solve_maze_sr solve_maze_size)) (<= 0 solve_maze_sc)) (< solve_maze_sc solve_maze_size)) (<= 0 solve_maze_dr)) (< solve_maze_dr solve_maze_size)) (<= 0 solve_maze_dc)) (< solve_maze_dc solve_maze_size))) (throw (Exception. "Invalid source or destination coordinates"))) (set! solve_maze_sol []) (set! solve_maze_i 0) (while (< solve_maze_i solve_maze_size) (do (set! solve_maze_row []) (set! solve_maze_j 0) (while (< solve_maze_j solve_maze_size) (do (set! solve_maze_row (conj solve_maze_row 1)) (set! solve_maze_j (+ solve_maze_j 1)))) (set! solve_maze_sol (conj solve_maze_sol solve_maze_row)) (set! solve_maze_i (+ solve_maze_i 1)))) (set! solve_maze_solved (run_maze solve_maze_maze solve_maze_sr solve_maze_sc solve_maze_dr solve_maze_dc solve_maze_sol)) (if solve_maze_solved (throw (ex-info "return" {:v solve_maze_sol})) (throw (Exception. "No solution exists!")))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_maze [[0 1 0 1 1] [0 0 0 0 0] [1 0 1 0 1] [0 0 1 0 0] [1 0 0 1 0]])

(def ^:dynamic main_n (- (count main_maze) 1))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (solve_maze main_maze 0 0 main_n main_n)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
