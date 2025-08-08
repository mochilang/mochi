(ns main (:refer-clojure :exclude [iabs search main]))

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

(declare iabs search main)

(def ^:dynamic main_cost nil)

(def ^:dynamic main_goal nil)

(def ^:dynamic main_grid nil)

(def ^:dynamic main_h nil)

(def ^:dynamic main_heuristic nil)

(def ^:dynamic main_i nil)

(def ^:dynamic main_init nil)

(def ^:dynamic main_j nil)

(def ^:dynamic main_p nil)

(def ^:dynamic main_result nil)

(def ^:dynamic main_row nil)

(def ^:dynamic main_rr nil)

(def ^:dynamic search_action nil)

(def ^:dynamic search_best_f nil)

(def ^:dynamic search_best_i nil)

(def ^:dynamic search_c nil)

(def ^:dynamic search_cell nil)

(def ^:dynamic search_closed nil)

(def ^:dynamic search_d nil)

(def ^:dynamic search_dir nil)

(def ^:dynamic search_f nil)

(def ^:dynamic search_f2 nil)

(def ^:dynamic search_found nil)

(def ^:dynamic search_g nil)

(def ^:dynamic search_g2 nil)

(def ^:dynamic search_i nil)

(def ^:dynamic search_idx nil)

(def ^:dynamic search_invpath nil)

(def ^:dynamic search_new_cell nil)

(def ^:dynamic search_next_cell nil)

(def ^:dynamic search_path nil)

(def ^:dynamic search_r nil)

(def ^:dynamic search_resign nil)

(def ^:dynamic search_row nil)

(def ^:dynamic search_x nil)

(def ^:dynamic search_x2 nil)

(def ^:dynamic search_y nil)

(def ^:dynamic search_y2 nil)

(def ^:dynamic main_DIRECTIONS [[(- 1) 0] [0 (- 1)] [1 0] [0 1]])

(defn iabs [iabs_x]
  (try (if (< iabs_x 0) (- iabs_x) iabs_x) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn search [search_grid search_init search_goal search_cost search_heuristic]
  (binding [search_action nil search_best_f nil search_best_i nil search_c nil search_cell nil search_closed nil search_d nil search_dir nil search_f nil search_f2 nil search_found nil search_g nil search_g2 nil search_i nil search_idx nil search_invpath nil search_new_cell nil search_next_cell nil search_path nil search_r nil search_resign nil search_row nil search_x nil search_x2 nil search_y nil search_y2 nil] (try (do (set! search_closed []) (set! search_r 0) (while (< search_r (count search_grid)) (do (set! search_row []) (set! search_c 0) (while (< search_c (count (nth search_grid 0))) (do (set! search_row (conj search_row 0)) (set! search_c (+ search_c 1)))) (set! search_closed (conj search_closed search_row)) (set! search_r (+ search_r 1)))) (set! search_closed (assoc-in search_closed [(nth search_init 0) (nth search_init 1)] 1)) (set! search_action []) (set! search_r 0) (while (< search_r (count search_grid)) (do (set! search_row []) (set! search_c 0) (while (< search_c (count (nth search_grid 0))) (do (set! search_row (conj search_row 0)) (set! search_c (+ search_c 1)))) (set! search_action (conj search_action search_row)) (set! search_r (+ search_r 1)))) (set! search_x (nth search_init 0)) (set! search_y (nth search_init 1)) (set! search_g 0) (set! search_f (+ search_g (nth (nth search_heuristic search_x) search_y))) (set! search_cell [[search_f search_g search_x search_y]]) (set! search_found false) (set! search_resign false) (while (and (not search_found) (not search_resign)) (if (= (count search_cell) 0) (throw (Exception. "Algorithm is unable to find solution")) (do (set! search_best_i 0) (set! search_best_f (nth (nth search_cell 0) 0)) (set! search_i 1) (while (< search_i (count search_cell)) (do (when (< (nth (nth search_cell search_i) 0) search_best_f) (do (set! search_best_f (nth (nth search_cell search_i) 0)) (set! search_best_i search_i))) (set! search_i (+ search_i 1)))) (set! search_next_cell (nth search_cell search_best_i)) (set! search_new_cell []) (set! search_i 0) (while (< search_i (count search_cell)) (do (when (not= search_i search_best_i) (set! search_new_cell (conj search_new_cell (nth search_cell search_i)))) (set! search_i (+ search_i 1)))) (set! search_cell search_new_cell) (set! search_x (nth search_next_cell 2)) (set! search_y (nth search_next_cell 3)) (set! search_g (nth search_next_cell 1)) (if (and (= search_x (nth search_goal 0)) (= search_y (nth search_goal 1))) (set! search_found true) (do (set! search_d 0) (while (< search_d (count main_DIRECTIONS)) (do (set! search_x2 (+ search_x (nth (nth main_DIRECTIONS search_d) 0))) (set! search_y2 (+ search_y (nth (nth main_DIRECTIONS search_d) 1))) (when (and (and (and (and (and (>= search_x2 0) (< search_x2 (count search_grid))) (>= search_y2 0)) (< search_y2 (count (nth search_grid 0)))) (= (nth (nth search_closed search_x2) search_y2) 0)) (= (nth (nth search_grid search_x2) search_y2) 0)) (do (set! search_g2 (+ search_g search_cost)) (set! search_f2 (+ search_g2 (nth (nth search_heuristic search_x2) search_y2))) (set! search_cell (conj search_cell [search_f2 search_g2 search_x2 search_y2])) (set! search_closed (assoc-in search_closed [search_x2 search_y2] 1)) (set! search_action (assoc-in search_action [search_x2 search_y2] search_d)))) (set! search_d (+ search_d 1))))))))) (set! search_invpath []) (set! search_x (nth search_goal 0)) (set! search_y (nth search_goal 1)) (set! search_invpath (conj search_invpath [search_x search_y])) (while (or (not= search_x (nth search_init 0)) (not= search_y (nth search_init 1))) (do (set! search_dir (nth (nth search_action search_x) search_y)) (set! search_x2 (- search_x (nth (nth main_DIRECTIONS search_dir) 0))) (set! search_y2 (- search_y (nth (nth main_DIRECTIONS search_dir) 1))) (set! search_x search_x2) (set! search_y search_y2) (set! search_invpath (conj search_invpath [search_x search_y])))) (set! search_path []) (set! search_idx (- (count search_invpath) 1)) (while (>= search_idx 0) (do (set! search_path (conj search_path (nth search_invpath search_idx))) (set! search_idx (- search_idx 1)))) (throw (ex-info "return" {:v {:action search_action :path search_path}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_cost nil main_goal nil main_grid nil main_h nil main_heuristic nil main_i nil main_init nil main_j nil main_p nil main_result nil main_row nil main_rr nil] (do (set! main_grid [[0 1 0 0 0 0] [0 1 0 0 0 0] [0 1 0 0 0 0] [0 1 0 0 1 0] [0 0 0 0 1 0]]) (set! main_init [0 0]) (set! main_goal [(- (count main_grid) 1) (- (count (nth main_grid 0)) 1)]) (set! main_cost 1) (set! main_heuristic []) (set! main_i 0) (while (< main_i (count main_grid)) (do (set! main_row []) (set! main_j 0) (while (< main_j (count (nth main_grid 0))) (do (set! main_h (+ (iabs (- main_i (nth main_goal 0))) (iabs (- main_j (nth main_goal 1))))) (if (= (nth (nth main_grid main_i) main_j) 1) (set! main_row (conj main_row 99)) (set! main_row (conj main_row main_h))) (set! main_j (+ main_j 1)))) (set! main_heuristic (conj main_heuristic main_row)) (set! main_i (+ main_i 1)))) (set! main_result (search main_grid main_init main_goal main_cost main_heuristic)) (println "ACTION MAP") (set! main_rr 0) (while (< main_rr (count (:action main_result))) (do (println (get (:action main_result) main_rr)) (set! main_rr (+ main_rr 1)))) (set! main_p 0) (while (< main_p (count (:path main_result))) (do (println (get (:path main_result) main_p)) (set! main_p (+ main_p 1)))))))

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
