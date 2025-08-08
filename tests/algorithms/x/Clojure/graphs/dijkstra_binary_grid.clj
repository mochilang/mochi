(ns main (:refer-clojure :exclude [key path_to_string dijkstra print_result]))

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

(declare key path_to_string dijkstra print_result)

(def ^:dynamic dijkstra_INF nil)

(def ^:dynamic dijkstra_cols nil)

(def ^:dynamic dijkstra_cur_key nil)

(def ^:dynamic dijkstra_current nil)

(def ^:dynamic dijkstra_dest_key nil)

(def ^:dynamic dijkstra_dist_map nil)

(def ^:dynamic dijkstra_dx nil)

(def ^:dynamic dijkstra_dy nil)

(def ^:dynamic dijkstra_front nil)

(def ^:dynamic dijkstra_i nil)

(def ^:dynamic dijkstra_k nil)

(def ^:dynamic dijkstra_n_key nil)

(def ^:dynamic dijkstra_nx nil)

(def ^:dynamic dijkstra_ny nil)

(def ^:dynamic dijkstra_path nil)

(def ^:dynamic dijkstra_path_rev nil)

(def ^:dynamic dijkstra_prev nil)

(def ^:dynamic dijkstra_queue nil)

(def ^:dynamic dijkstra_rows nil)

(def ^:dynamic dijkstra_step_key nil)

(def ^:dynamic dijkstra_step_pt nil)

(def ^:dynamic path_to_string_i nil)

(def ^:dynamic path_to_string_pt nil)

(def ^:dynamic path_to_string_s nil)

(defn key [key_p]
  (try (throw (ex-info "return" {:v (str (str (str (:x key_p)) ",") (str (:y key_p)))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn path_to_string [path_to_string_path]
  (binding [path_to_string_i nil path_to_string_pt nil path_to_string_s nil] (try (do (set! path_to_string_s "[") (set! path_to_string_i 0) (while (< path_to_string_i (count path_to_string_path)) (do (set! path_to_string_pt (nth path_to_string_path path_to_string_i)) (set! path_to_string_s (str (str (str (str (str path_to_string_s "(") (str (:x path_to_string_pt))) ", ") (str (:y path_to_string_pt))) ")")) (when (< path_to_string_i (- (count path_to_string_path) 1)) (set! path_to_string_s (str path_to_string_s ", "))) (set! path_to_string_i (+ path_to_string_i 1)))) (set! path_to_string_s (str path_to_string_s "]")) (throw (ex-info "return" {:v path_to_string_s}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn dijkstra [dijkstra_grid dijkstra_source dijkstra_destination dijkstra_allow_diagonal]
  (binding [dijkstra_INF nil dijkstra_cols nil dijkstra_cur_key nil dijkstra_current nil dijkstra_dest_key nil dijkstra_dist_map nil dijkstra_dx nil dijkstra_dy nil dijkstra_front nil dijkstra_i nil dijkstra_k nil dijkstra_n_key nil dijkstra_nx nil dijkstra_ny nil dijkstra_path nil dijkstra_path_rev nil dijkstra_prev nil dijkstra_queue nil dijkstra_rows nil dijkstra_step_key nil dijkstra_step_pt nil] (try (do (set! dijkstra_rows (count dijkstra_grid)) (set! dijkstra_cols (count (nth dijkstra_grid 0))) (set! dijkstra_dx [(- 1) 1 0 0]) (set! dijkstra_dy [0 0 (- 1) 1]) (when dijkstra_allow_diagonal (do (set! dijkstra_dx (concat dijkstra_dx [(- 1) (- 1) 1 1])) (set! dijkstra_dy (concat dijkstra_dy [(- 1) 1 (- 1) 1])))) (set! dijkstra_INF 1000000000000.0) (set! dijkstra_queue [dijkstra_source]) (set! dijkstra_front 0) (set! dijkstra_dist_map {(key dijkstra_source) 0.0}) (set! dijkstra_prev {}) (loop [while_flag_1 true] (when (and while_flag_1 (< dijkstra_front (count dijkstra_queue))) (do (set! dijkstra_current (nth dijkstra_queue dijkstra_front)) (set! dijkstra_front (+ dijkstra_front 1)) (set! dijkstra_cur_key (key dijkstra_current)) (cond (and (= (:x dijkstra_current) (:x dijkstra_destination)) (= (:y dijkstra_current) (:y dijkstra_destination))) (recur false) :else (do (set! dijkstra_i 0) (while (< dijkstra_i (count dijkstra_dx)) (do (set! dijkstra_nx (+ (:x dijkstra_current) (nth dijkstra_dx dijkstra_i))) (set! dijkstra_ny (+ (:y dijkstra_current) (nth dijkstra_dy dijkstra_i))) (when (and (and (and (>= dijkstra_nx 0) (< dijkstra_nx dijkstra_rows)) (>= dijkstra_ny 0)) (< dijkstra_ny dijkstra_cols)) (when (= (nth (nth dijkstra_grid dijkstra_nx) dijkstra_ny) 1) (do (set! dijkstra_n_key (str (str (str dijkstra_nx) ",") (str dijkstra_ny))) (when (not (in dijkstra_n_key dijkstra_dist_map)) (do (set! dijkstra_dist_map (assoc dijkstra_dist_map dijkstra_n_key (+ (get dijkstra_dist_map dijkstra_cur_key) 1.0))) (set! dijkstra_prev (assoc dijkstra_prev dijkstra_n_key dijkstra_current)) (set! dijkstra_queue (conj dijkstra_queue {:x dijkstra_nx :y dijkstra_ny}))))))) (set! dijkstra_i (+ dijkstra_i 1)))) (recur while_flag_1)))))) (set! dijkstra_dest_key (key dijkstra_destination)) (when (in dijkstra_dest_key dijkstra_dist_map) (do (set! dijkstra_path_rev [dijkstra_destination]) (set! dijkstra_step_key dijkstra_dest_key) (set! dijkstra_step_pt dijkstra_destination) (while (not= dijkstra_step_key (key dijkstra_source)) (do (set! dijkstra_step_pt (get dijkstra_prev dijkstra_step_key)) (set! dijkstra_step_key (key dijkstra_step_pt)) (set! dijkstra_path_rev (conj dijkstra_path_rev dijkstra_step_pt)))) (set! dijkstra_path []) (set! dijkstra_k (- (count dijkstra_path_rev) 1)) (while (>= dijkstra_k 0) (do (set! dijkstra_path (conj dijkstra_path (nth dijkstra_path_rev dijkstra_k))) (set! dijkstra_k (- dijkstra_k 1)))) (throw (ex-info "return" {:v {:distance (get dijkstra_dist_map dijkstra_dest_key) :path dijkstra_path}})))) (throw (ex-info "return" {:v {:distance dijkstra_INF :path []}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn print_result [print_result_res]
  (println (str (str (str (:distance print_result_res)) ", ") (path_to_string (:path print_result_res)))))

(def ^:dynamic main_grid1 [[1 1 1] [0 1 0] [0 1 1]])

(def ^:dynamic main_grid2 [[1 1 1] [0 0 1] [0 1 1]])

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (print_result (dijkstra main_grid1 {:x 0 :y 0} {:x 2 :y 2} false))
      (print_result (dijkstra main_grid1 {:x 0 :y 0} {:x 2 :y 2} true))
      (print_result (dijkstra main_grid2 {:x 0 :y 0} {:x 2 :y 2} false))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
