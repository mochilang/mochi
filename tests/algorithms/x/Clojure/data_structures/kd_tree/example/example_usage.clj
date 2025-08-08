(ns main (:refer-clojure :exclude [rand random hypercube_points sort_points sublist shift_nodes build_kdtree square_distance nearest_neighbour_search list_to_string main]))

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

(declare rand random hypercube_points sort_points sublist shift_nodes build_kdtree square_distance nearest_neighbour_search list_to_string main)

(def ^:dynamic build_kdtree_axis nil)

(def ^:dynamic build_kdtree_k nil)

(def ^:dynamic build_kdtree_left_index nil)

(def ^:dynamic build_kdtree_left_points nil)

(def ^:dynamic build_kdtree_left_res nil)

(def ^:dynamic build_kdtree_median nil)

(def ^:dynamic build_kdtree_nodes nil)

(def ^:dynamic build_kdtree_offset nil)

(def ^:dynamic build_kdtree_points nil)

(def ^:dynamic build_kdtree_right_index nil)

(def ^:dynamic build_kdtree_right_points nil)

(def ^:dynamic build_kdtree_right_res nil)

(def ^:dynamic build_kdtree_root_index nil)

(def ^:dynamic build_kdtree_shifted_right nil)

(def ^:dynamic hypercube_points_i nil)

(def ^:dynamic hypercube_points_j nil)

(def ^:dynamic hypercube_points_p nil)

(def ^:dynamic hypercube_points_pts nil)

(def ^:dynamic list_to_string_i nil)

(def ^:dynamic list_to_string_s nil)

(def ^:dynamic main_build nil)

(def ^:dynamic main_cube_size nil)

(def ^:dynamic main_i nil)

(def ^:dynamic main_num_dimensions nil)

(def ^:dynamic main_num_points nil)

(def ^:dynamic main_pts nil)

(def ^:dynamic main_query nil)

(def ^:dynamic main_res nil)

(def ^:dynamic main_root nil)

(def ^:dynamic main_tree nil)

(def ^:dynamic nearest_neighbour_search_dist nil)

(def ^:dynamic nearest_neighbour_search_i nil)

(def ^:dynamic nearest_neighbour_search_nearest_dist nil)

(def ^:dynamic nearest_neighbour_search_nearest_point nil)

(def ^:dynamic nearest_neighbour_search_node nil)

(def ^:dynamic nearest_neighbour_search_visited nil)

(def ^:dynamic shift_nodes_i nil)

(def ^:dynamic shift_nodes_nodes nil)

(def ^:dynamic sort_points_i nil)

(def ^:dynamic sort_points_j nil)

(def ^:dynamic sort_points_key nil)

(def ^:dynamic sort_points_n nil)

(def ^:dynamic sort_points_points nil)

(def ^:dynamic square_distance_diff nil)

(def ^:dynamic square_distance_i nil)

(def ^:dynamic square_distance_sum nil)

(def ^:dynamic sublist_i nil)

(def ^:dynamic sublist_res nil)

(def ^:dynamic main_seed 1)

(defn rand []
  (try (do (alter-var-root (var main_seed) (fn [_] (mod (+ (* main_seed 1103515245) 12345) 2147483648))) (throw (ex-info "return" {:v main_seed}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn random []
  (try (throw (ex-info "return" {:v (/ (* 1.0 (rand)) 2147483648.0)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn hypercube_points [hypercube_points_num_points hypercube_points_cube_size hypercube_points_num_dimensions]
  (binding [hypercube_points_i nil hypercube_points_j nil hypercube_points_p nil hypercube_points_pts nil] (try (do (set! hypercube_points_pts []) (set! hypercube_points_i 0) (while (< hypercube_points_i hypercube_points_num_points) (do (set! hypercube_points_p []) (set! hypercube_points_j 0) (while (< hypercube_points_j hypercube_points_num_dimensions) (do (set! hypercube_points_p (conj hypercube_points_p (* hypercube_points_cube_size (random)))) (set! hypercube_points_j (+ hypercube_points_j 1)))) (set! hypercube_points_pts (conj hypercube_points_pts hypercube_points_p)) (set! hypercube_points_i (+ hypercube_points_i 1)))) (throw (ex-info "return" {:v hypercube_points_pts}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn sort_points [sort_points_points_p sort_points_axis]
  (binding [sort_points_i nil sort_points_j nil sort_points_key nil sort_points_n nil sort_points_points nil] (try (do (set! sort_points_points sort_points_points_p) (set! sort_points_n (count sort_points_points)) (set! sort_points_i 1) (while (< sort_points_i sort_points_n) (do (set! sort_points_key (nth sort_points_points sort_points_i)) (set! sort_points_j (- sort_points_i 1)) (while (and (>= sort_points_j 0) (> (nth (nth sort_points_points sort_points_j) sort_points_axis) (nth sort_points_key sort_points_axis))) (do (set! sort_points_points (assoc sort_points_points (+ sort_points_j 1) (nth sort_points_points sort_points_j))) (set! sort_points_j (- sort_points_j 1)))) (set! sort_points_points (assoc sort_points_points (+ sort_points_j 1) sort_points_key)) (set! sort_points_i (+ sort_points_i 1)))) (throw (ex-info "return" {:v sort_points_points}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn sublist [sublist_arr sublist_start sublist_end]
  (binding [sublist_i nil sublist_res nil] (try (do (set! sublist_res []) (set! sublist_i sublist_start) (while (< sublist_i sublist_end) (do (set! sublist_res (conj sublist_res (nth sublist_arr sublist_i))) (set! sublist_i (+ sublist_i 1)))) (throw (ex-info "return" {:v sublist_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn shift_nodes [shift_nodes_nodes_p shift_nodes_offset]
  (binding [shift_nodes_i nil shift_nodes_nodes nil] (try (do (set! shift_nodes_nodes shift_nodes_nodes_p) (set! shift_nodes_i 0) (while (< shift_nodes_i (count shift_nodes_nodes)) (do (when (not= (:left (nth shift_nodes_nodes shift_nodes_i)) (- 0 1)) (set! shift_nodes_nodes (assoc-in shift_nodes_nodes [shift_nodes_i :left] (+ (:left (nth shift_nodes_nodes shift_nodes_i)) shift_nodes_offset)))) (when (not= (:right (nth shift_nodes_nodes shift_nodes_i)) (- 0 1)) (set! shift_nodes_nodes (assoc-in shift_nodes_nodes [shift_nodes_i :right] (+ (:right (nth shift_nodes_nodes shift_nodes_i)) shift_nodes_offset)))) (set! shift_nodes_i (+ shift_nodes_i 1)))) (throw (ex-info "return" {:v shift_nodes_nodes}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn build_kdtree [build_kdtree_points_p build_kdtree_depth]
  (binding [build_kdtree_axis nil build_kdtree_k nil build_kdtree_left_index nil build_kdtree_left_points nil build_kdtree_left_res nil build_kdtree_median nil build_kdtree_nodes nil build_kdtree_offset nil build_kdtree_points nil build_kdtree_right_index nil build_kdtree_right_points nil build_kdtree_right_res nil build_kdtree_root_index nil build_kdtree_shifted_right nil] (try (do (set! build_kdtree_points build_kdtree_points_p) (when (= (count build_kdtree_points) 0) (throw (ex-info "return" {:v {:index (- 0 1) :nodes []}}))) (set! build_kdtree_k (count (nth build_kdtree_points 0))) (set! build_kdtree_axis (mod build_kdtree_depth build_kdtree_k)) (set! build_kdtree_points (sort_points build_kdtree_points build_kdtree_axis)) (set! build_kdtree_median (quot (count build_kdtree_points) 2)) (set! build_kdtree_left_points (sublist build_kdtree_points 0 build_kdtree_median)) (set! build_kdtree_right_points (sublist build_kdtree_points (+ build_kdtree_median 1) (count build_kdtree_points))) (set! build_kdtree_left_res (build_kdtree build_kdtree_left_points (+ build_kdtree_depth 1))) (set! build_kdtree_right_res (build_kdtree build_kdtree_right_points (+ build_kdtree_depth 1))) (set! build_kdtree_offset (+ (count (:nodes build_kdtree_left_res)) 1)) (set! build_kdtree_shifted_right (shift_nodes (:nodes build_kdtree_right_res) build_kdtree_offset)) (set! build_kdtree_nodes (:nodes build_kdtree_left_res)) (set! build_kdtree_left_index (:index build_kdtree_left_res)) (set! build_kdtree_right_index (if (= (:index build_kdtree_right_res) (- 0 1)) (- 0 1) (+ (:index build_kdtree_right_res) build_kdtree_offset))) (set! build_kdtree_nodes (conj build_kdtree_nodes {:left build_kdtree_left_index :point (nth build_kdtree_points build_kdtree_median) :right build_kdtree_right_index})) (set! build_kdtree_nodes (concat build_kdtree_nodes build_kdtree_shifted_right)) (set! build_kdtree_root_index (count (:nodes build_kdtree_left_res))) (throw (ex-info "return" {:v {:index build_kdtree_root_index :nodes build_kdtree_nodes}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn square_distance [square_distance_a square_distance_b]
  (binding [square_distance_diff nil square_distance_i nil square_distance_sum nil] (try (do (set! square_distance_sum 0.0) (set! square_distance_i 0) (while (< square_distance_i (count square_distance_a)) (do (set! square_distance_diff (- (nth square_distance_a square_distance_i) (nth square_distance_b square_distance_i))) (set! square_distance_sum (+ square_distance_sum (* square_distance_diff square_distance_diff))) (set! square_distance_i (+ square_distance_i 1)))) (throw (ex-info "return" {:v square_distance_sum}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn nearest_neighbour_search [nearest_neighbour_search_tree nearest_neighbour_search_root nearest_neighbour_search_query_point]
  (binding [nearest_neighbour_search_dist nil nearest_neighbour_search_i nil nearest_neighbour_search_nearest_dist nil nearest_neighbour_search_nearest_point nil nearest_neighbour_search_node nil nearest_neighbour_search_visited nil] (try (do (set! nearest_neighbour_search_nearest_point []) (set! nearest_neighbour_search_nearest_dist 0.0) (set! nearest_neighbour_search_visited 0) (set! nearest_neighbour_search_i 0) (while (< nearest_neighbour_search_i (count nearest_neighbour_search_tree)) (do (set! nearest_neighbour_search_node (nth nearest_neighbour_search_tree nearest_neighbour_search_i)) (set! nearest_neighbour_search_dist (square_distance nearest_neighbour_search_query_point (:point nearest_neighbour_search_node))) (set! nearest_neighbour_search_visited (+ nearest_neighbour_search_visited 1)) (when (or (= nearest_neighbour_search_visited 1) (< nearest_neighbour_search_dist nearest_neighbour_search_nearest_dist)) (do (set! nearest_neighbour_search_nearest_point (:point nearest_neighbour_search_node)) (set! nearest_neighbour_search_nearest_dist nearest_neighbour_search_dist))) (set! nearest_neighbour_search_i (+ nearest_neighbour_search_i 1)))) (throw (ex-info "return" {:v {:dist nearest_neighbour_search_nearest_dist :point nearest_neighbour_search_nearest_point :visited nearest_neighbour_search_visited}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn list_to_string [list_to_string_arr]
  (binding [list_to_string_i nil list_to_string_s nil] (try (do (set! list_to_string_s "[") (set! list_to_string_i 0) (while (< list_to_string_i (count list_to_string_arr)) (do (set! list_to_string_s (str list_to_string_s (str (nth list_to_string_arr list_to_string_i)))) (when (< list_to_string_i (- (count list_to_string_arr) 1)) (set! list_to_string_s (str list_to_string_s ", "))) (set! list_to_string_i (+ list_to_string_i 1)))) (throw (ex-info "return" {:v (str list_to_string_s "]")}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_build nil main_cube_size nil main_i nil main_num_dimensions nil main_num_points nil main_pts nil main_query nil main_res nil main_root nil main_tree nil] (do (set! main_num_points 5000) (set! main_cube_size 10.0) (set! main_num_dimensions 10) (set! main_pts (hypercube_points main_num_points main_cube_size main_num_dimensions)) (set! main_build (build_kdtree main_pts 0)) (set! main_root (:index main_build)) (set! main_tree (:nodes main_build)) (set! main_query []) (set! main_i 0) (while (< main_i main_num_dimensions) (do (set! main_query (conj main_query (random))) (set! main_i (+ main_i 1)))) (set! main_res (nearest_neighbour_search main_tree main_root main_query)) (println (str "Query point: " (list_to_string main_query))) (println (str "Nearest point: " (list_to_string (:point main_res)))) (println (str "Distance: " (str (:dist main_res)))) (println (str "Nodes visited: " (str (:visited main_res)))))))

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
