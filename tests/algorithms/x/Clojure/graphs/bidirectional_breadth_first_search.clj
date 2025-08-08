(ns main (:refer-clojure :exclude [key parse_int parse_key neighbors reverse_list bfs bidirectional_bfs path_to_string]))

(require 'clojure.set)

(defrecord VisitedB [goal])

(defrecord VisitedF [start])

(defrecord Visited [start])

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(defn indexOf [s sub]
  (let [idx (clojure.string/index-of s sub)] (if (nil? idx) -1 idx)))

(def ^:dynamic bfs_head nil)

(def ^:dynamic bfs_i nil)

(def ^:dynamic bfs_neigh nil)

(def ^:dynamic bfs_new_path nil)

(def ^:dynamic bfs_node nil)

(def ^:dynamic bfs_npos nil)

(def ^:dynamic bfs_queue nil)

(def ^:dynamic bfs_visited nil)

(def ^:dynamic bidirectional_bfs_head_b nil)

(def ^:dynamic bidirectional_bfs_head_f nil)

(def ^:dynamic bidirectional_bfs_i nil)

(def ^:dynamic bidirectional_bfs_j nil)

(def ^:dynamic bidirectional_bfs_neigh_b nil)

(def ^:dynamic bidirectional_bfs_neigh_f nil)

(def ^:dynamic bidirectional_bfs_new_path nil)

(def ^:dynamic bidirectional_bfs_new_path_b nil)

(def ^:dynamic bidirectional_bfs_node_b nil)

(def ^:dynamic bidirectional_bfs_node_f nil)

(def ^:dynamic bidirectional_bfs_npos nil)

(def ^:dynamic bidirectional_bfs_nposb nil)

(def ^:dynamic bidirectional_bfs_path_f nil)

(def ^:dynamic bidirectional_bfs_queue_b nil)

(def ^:dynamic bidirectional_bfs_queue_f nil)

(def ^:dynamic bidirectional_bfs_rev nil)

(def ^:dynamic bidirectional_bfs_t nil)

(def ^:dynamic bidirectional_bfs_visited_b nil)

(def ^:dynamic bidirectional_bfs_visited_f nil)

(def ^:dynamic first_v nil)

(def ^:dynamic neighbors_coords nil)

(def ^:dynamic neighbors_i nil)

(def ^:dynamic neighbors_nx nil)

(def ^:dynamic neighbors_ny nil)

(def ^:dynamic neighbors_res nil)

(def ^:dynamic neighbors_x nil)

(def ^:dynamic neighbors_y nil)

(def ^:dynamic parse_int_c nil)

(def ^:dynamic parse_int_i nil)

(def ^:dynamic parse_int_value nil)

(def ^:dynamic parse_key_idx nil)

(def ^:dynamic parse_key_x nil)

(def ^:dynamic parse_key_y nil)

(def ^:dynamic path_to_string_c nil)

(def ^:dynamic path_to_string_i nil)

(def ^:dynamic path_to_string_s nil)

(def ^:dynamic reverse_list_i nil)

(def ^:dynamic reverse_list_res nil)

(defn split [s sep]
  (clojure.string/split s (re-pattern sep)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare key parse_int parse_key neighbors reverse_list bfs bidirectional_bfs path_to_string)

(def ^:dynamic main_grid [[0 0 0 0 0 0 0] [0 1 0 0 0 0 0] [0 0 0 0 0 0 0] [0 0 1 0 0 0 0] [1 0 1 0 0 0 0] [0 0 0 0 0 0 0] [0 0 0 0 1 0 0]])

(def ^:dynamic main_delta [[(- 1) 0] [0 (- 1)] [1 0] [0 1]])

(defn key [key_y key_x]
  (try (throw (ex-info "return" {:v (str (str (str key_y) ",") (str key_x))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn parse_int [parse_int_s]
  (binding [parse_int_c nil parse_int_i nil parse_int_value nil] (try (do (set! parse_int_value 0) (set! parse_int_i 0) (while (< parse_int_i (count parse_int_s)) (do (set! parse_int_c (nth parse_int_s parse_int_i)) (set! parse_int_value (+ (* parse_int_value 10) (long parse_int_c))) (set! parse_int_i (+ parse_int_i 1)))) (throw (ex-info "return" {:v parse_int_value}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn parse_key [parse_key_k]
  (binding [parse_key_idx nil parse_key_x nil parse_key_y nil] (try (do (set! parse_key_idx 0) (while (and (< parse_key_idx (count parse_key_k)) (not= (subs parse_key_k parse_key_idx (min (+ parse_key_idx 1) (count parse_key_k))) ",")) (set! parse_key_idx (+ parse_key_idx 1))) (set! parse_key_y (parse_int (subs parse_key_k 0 (min parse_key_idx (count parse_key_k))))) (set! parse_key_x (parse_int (subs parse_key_k (+ parse_key_idx 1) (min (count parse_key_k) (count parse_key_k))))) (throw (ex-info "return" {:v [parse_key_y parse_key_x]}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn neighbors [neighbors_pos]
  (binding [neighbors_coords nil neighbors_i nil neighbors_nx nil neighbors_ny nil neighbors_res nil neighbors_x nil neighbors_y nil] (try (do (set! neighbors_coords (parse_key neighbors_pos)) (set! neighbors_y (nth neighbors_coords 0)) (set! neighbors_x (nth neighbors_coords 1)) (set! neighbors_res []) (set! neighbors_i 0) (while (< neighbors_i (count main_delta)) (do (set! neighbors_ny (+ neighbors_y (nth (nth main_delta neighbors_i) 0))) (set! neighbors_nx (+ neighbors_x (nth (nth main_delta neighbors_i) 1))) (when (and (and (and (>= neighbors_ny 0) (< neighbors_ny (count main_grid))) (>= neighbors_nx 0)) (< neighbors_nx (count (nth main_grid 0)))) (when (= (nth (nth main_grid neighbors_ny) neighbors_nx) 0) (set! neighbors_res (conj neighbors_res (key neighbors_ny neighbors_nx))))) (set! neighbors_i (+ neighbors_i 1)))) (throw (ex-info "return" {:v neighbors_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn reverse_list [reverse_list_lst]
  (binding [reverse_list_i nil reverse_list_res nil] (try (do (set! reverse_list_res []) (set! reverse_list_i (- (count reverse_list_lst) 1)) (while (>= reverse_list_i 0) (do (set! reverse_list_res (conj reverse_list_res (nth reverse_list_lst reverse_list_i))) (set! reverse_list_i (- reverse_list_i 1)))) (throw (ex-info "return" {:v reverse_list_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn bfs [bfs_start bfs_goal]
  (binding [bfs_head nil bfs_i nil bfs_neigh nil bfs_new_path nil bfs_node nil bfs_npos nil bfs_queue nil bfs_visited nil] (try (do (set! bfs_queue []) (set! bfs_queue (conj bfs_queue {:path [bfs_start] :pos bfs_start})) (set! bfs_head 0) (set! bfs_visited {:bfs_start true}) (while (< bfs_head (count bfs_queue)) (do (set! bfs_node (nth bfs_queue bfs_head)) (set! bfs_head (+ bfs_head 1)) (when (= (:pos bfs_node) bfs_goal) (throw (ex-info "return" {:v (:path bfs_node)}))) (set! bfs_neigh (neighbors (:pos bfs_node))) (set! bfs_i 0) (while (< bfs_i (count bfs_neigh)) (do (set! bfs_npos (nth bfs_neigh bfs_i)) (when (not (in bfs_npos bfs_visited)) (do (set! bfs_visited (assoc bfs_visited bfs_npos true)) (set! bfs_new_path (conj (:path bfs_node) bfs_npos)) (set! bfs_queue (conj bfs_queue {:path bfs_new_path :pos bfs_npos})))) (set! bfs_i (+ bfs_i 1)))))) (throw (ex-info "return" {:v []}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn bidirectional_bfs [bidirectional_bfs_start bidirectional_bfs_goal]
  (binding [bidirectional_bfs_head_b nil bidirectional_bfs_head_f nil bidirectional_bfs_i nil bidirectional_bfs_j nil bidirectional_bfs_neigh_b nil bidirectional_bfs_neigh_f nil bidirectional_bfs_new_path nil bidirectional_bfs_new_path_b nil bidirectional_bfs_node_b nil bidirectional_bfs_node_f nil bidirectional_bfs_npos nil bidirectional_bfs_nposb nil bidirectional_bfs_path_f nil bidirectional_bfs_queue_b nil bidirectional_bfs_queue_f nil bidirectional_bfs_rev nil bidirectional_bfs_t nil bidirectional_bfs_visited_b nil bidirectional_bfs_visited_f nil] (try (do (set! bidirectional_bfs_queue_f []) (set! bidirectional_bfs_queue_b []) (set! bidirectional_bfs_queue_f (conj bidirectional_bfs_queue_f {:path [bidirectional_bfs_start] :pos bidirectional_bfs_start})) (set! bidirectional_bfs_queue_b (conj bidirectional_bfs_queue_b {:path [bidirectional_bfs_goal] :pos bidirectional_bfs_goal})) (set! bidirectional_bfs_head_f 0) (set! bidirectional_bfs_head_b 0) (set! bidirectional_bfs_visited_f {:bidirectional_bfs_start [bidirectional_bfs_start]}) (set! bidirectional_bfs_visited_b {:bidirectional_bfs_goal [bidirectional_bfs_goal]}) (while (and (< bidirectional_bfs_head_f (count bidirectional_bfs_queue_f)) (< bidirectional_bfs_head_b (count bidirectional_bfs_queue_b))) (do (set! bidirectional_bfs_node_f (nth bidirectional_bfs_queue_f bidirectional_bfs_head_f)) (set! bidirectional_bfs_head_f (+ bidirectional_bfs_head_f 1)) (set! bidirectional_bfs_neigh_f (neighbors (:pos bidirectional_bfs_node_f))) (set! bidirectional_bfs_i 0) (while (< bidirectional_bfs_i (count bidirectional_bfs_neigh_f)) (do (set! bidirectional_bfs_npos (nth bidirectional_bfs_neigh_f bidirectional_bfs_i)) (when (not (in bidirectional_bfs_npos bidirectional_bfs_visited_f)) (do (set! bidirectional_bfs_new_path (conj (:path bidirectional_bfs_node_f) bidirectional_bfs_npos)) (set! bidirectional_bfs_visited_f (assoc bidirectional_bfs_visited_f bidirectional_bfs_npos bidirectional_bfs_new_path)) (when (in bidirectional_bfs_npos bidirectional_bfs_visited_b) (do (set! bidirectional_bfs_rev (reverse_list (nth bidirectional_bfs_visited_b bidirectional_bfs_npos))) (set! bidirectional_bfs_j 1) (while (< bidirectional_bfs_j (count bidirectional_bfs_rev)) (do (set! bidirectional_bfs_new_path (conj bidirectional_bfs_new_path (nth bidirectional_bfs_rev bidirectional_bfs_j))) (set! bidirectional_bfs_j (+ bidirectional_bfs_j 1)))) (throw (ex-info "return" {:v bidirectional_bfs_new_path})))) (set! bidirectional_bfs_queue_f (conj bidirectional_bfs_queue_f {:path bidirectional_bfs_new_path :pos bidirectional_bfs_npos})))) (set! bidirectional_bfs_i (+ bidirectional_bfs_i 1)))) (set! bidirectional_bfs_node_b (nth bidirectional_bfs_queue_b bidirectional_bfs_head_b)) (set! bidirectional_bfs_head_b (+ bidirectional_bfs_head_b 1)) (set! bidirectional_bfs_neigh_b (neighbors (:pos bidirectional_bfs_node_b))) (set! bidirectional_bfs_j 0) (while (< bidirectional_bfs_j (count bidirectional_bfs_neigh_b)) (do (set! bidirectional_bfs_nposb (nth bidirectional_bfs_neigh_b bidirectional_bfs_j)) (when (not (in bidirectional_bfs_nposb bidirectional_bfs_visited_b)) (do (set! bidirectional_bfs_new_path_b (conj (:path bidirectional_bfs_node_b) bidirectional_bfs_nposb)) (set! bidirectional_bfs_visited_b (assoc bidirectional_bfs_visited_b bidirectional_bfs_nposb bidirectional_bfs_new_path_b)) (when (in bidirectional_bfs_nposb bidirectional_bfs_visited_f) (do (set! bidirectional_bfs_path_f (nth bidirectional_bfs_visited_f bidirectional_bfs_nposb)) (set! bidirectional_bfs_new_path_b (reverse_list bidirectional_bfs_new_path_b)) (set! bidirectional_bfs_t 1) (while (< bidirectional_bfs_t (count bidirectional_bfs_new_path_b)) (do (set! bidirectional_bfs_path_f (conj bidirectional_bfs_path_f (nth bidirectional_bfs_new_path_b bidirectional_bfs_t))) (set! bidirectional_bfs_t (+ bidirectional_bfs_t 1)))) (throw (ex-info "return" {:v bidirectional_bfs_path_f})))) (set! bidirectional_bfs_queue_b (conj bidirectional_bfs_queue_b {:path bidirectional_bfs_new_path_b :pos bidirectional_bfs_nposb})))) (set! bidirectional_bfs_j (+ bidirectional_bfs_j 1)))))) (throw (ex-info "return" {:v [bidirectional_bfs_start]}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn path_to_string [path_to_string_path]
  (binding [first_v nil path_to_string_c nil path_to_string_i nil path_to_string_s nil] (try (do (when (= (count path_to_string_path) 0) (throw (ex-info "return" {:v "[]"}))) (set! first_v (parse_key (nth path_to_string_path 0))) (set! path_to_string_s (str (str (str (str "[(" (str (nth first_v 0))) ", ") (str (nth first_v 1))) ")")) (set! path_to_string_i 1) (while (< path_to_string_i (count path_to_string_path)) (do (set! path_to_string_c (parse_key (nth path_to_string_path path_to_string_i))) (set! path_to_string_s (str (str (str (str (str path_to_string_s ", (") (str (nth path_to_string_c 0))) ", ") (str (nth path_to_string_c 1))) ")")) (set! path_to_string_i (+ path_to_string_i 1)))) (set! path_to_string_s (str path_to_string_s "]")) (throw (ex-info "return" {:v path_to_string_s}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_start (key 0 0))

(def ^:dynamic main_goal (key (- (count main_grid) 1) (- (count (nth main_grid 0)) 1)))

(def ^:dynamic main_path1 (bfs main_start main_goal))

(def ^:dynamic main_path2 (bidirectional_bfs main_start main_goal))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (path_to_string main_path1))
      (println (path_to_string main_path2))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
