(ns main (:refer-clojure :exclude [new_adjacency_list add_edge push_front pop_front front get_shortest_path]))

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

(declare new_adjacency_list add_edge push_front pop_front front get_shortest_path)

(def ^:dynamic add_edge_al nil)

(def ^:dynamic add_edge_edges nil)

(def ^:dynamic add_edge_g nil)

(def ^:dynamic get_shortest_path_current_distance nil)

(def ^:dynamic get_shortest_path_current_vertex nil)

(def ^:dynamic get_shortest_path_dest nil)

(def ^:dynamic get_shortest_path_dest_distance nil)

(def ^:dynamic get_shortest_path_distances nil)

(def ^:dynamic get_shortest_path_edge nil)

(def ^:dynamic get_shortest_path_edges nil)

(def ^:dynamic get_shortest_path_i nil)

(def ^:dynamic get_shortest_path_j nil)

(def ^:dynamic get_shortest_path_new_distance nil)

(def ^:dynamic get_shortest_path_queue nil)

(def ^:dynamic get_shortest_path_result nil)

(def ^:dynamic new_adjacency_list_g nil)

(def ^:dynamic new_adjacency_list_i nil)

(def ^:dynamic pop_front_i nil)

(def ^:dynamic pop_front_res nil)

(def ^:dynamic push_front_i nil)

(def ^:dynamic push_front_res nil)

(defn new_adjacency_list [new_adjacency_list_size]
  (binding [new_adjacency_list_g nil new_adjacency_list_i nil] (try (do (set! new_adjacency_list_g []) (set! new_adjacency_list_i 0) (while (< new_adjacency_list_i new_adjacency_list_size) (do (set! new_adjacency_list_g (conj new_adjacency_list_g [])) (set! new_adjacency_list_i (+ new_adjacency_list_i 1)))) (throw (ex-info "return" {:v {:graph new_adjacency_list_g :size new_adjacency_list_size}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn add_edge [add_edge_al_p add_edge_from_vertex add_edge_to_vertex add_edge_weight]
  (binding [add_edge_al nil add_edge_edges nil add_edge_g nil] (do (set! add_edge_al add_edge_al_p) (when (not (or (= add_edge_weight 0) (= add_edge_weight 1))) (throw (Exception. "Edge weight must be either 0 or 1."))) (when (or (< add_edge_to_vertex 0) (>= add_edge_to_vertex (:size add_edge_al))) (throw (Exception. "Vertex indexes must be in [0; size)."))) (set! add_edge_g (:graph add_edge_al)) (set! add_edge_edges (nth add_edge_g add_edge_from_vertex)) (set! add_edge_g (assoc add_edge_g add_edge_from_vertex (conj add_edge_edges {:destination_vertex add_edge_to_vertex :weight add_edge_weight}))) (set! add_edge_al (assoc add_edge_al :graph add_edge_g)))))

(defn push_front [push_front_q push_front_v]
  (binding [push_front_i nil push_front_res nil] (try (do (set! push_front_res [push_front_v]) (set! push_front_i 0) (while (< push_front_i (count push_front_q)) (do (set! push_front_res (conj push_front_res (nth push_front_q push_front_i))) (set! push_front_i (+ push_front_i 1)))) (throw (ex-info "return" {:v push_front_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn pop_front [pop_front_q]
  (binding [pop_front_i nil pop_front_res nil] (try (do (set! pop_front_res []) (set! pop_front_i 1) (while (< pop_front_i (count pop_front_q)) (do (set! pop_front_res (conj pop_front_res (nth pop_front_q pop_front_i))) (set! pop_front_i (+ pop_front_i 1)))) (throw (ex-info "return" {:v pop_front_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn front [front_q]
  (try (throw (ex-info "return" {:v (nth front_q 0)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn get_shortest_path [get_shortest_path_al get_shortest_path_start_vertex get_shortest_path_finish_vertex]
  (binding [get_shortest_path_current_distance nil get_shortest_path_current_vertex nil get_shortest_path_dest nil get_shortest_path_dest_distance nil get_shortest_path_distances nil get_shortest_path_edge nil get_shortest_path_edges nil get_shortest_path_i nil get_shortest_path_j nil get_shortest_path_new_distance nil get_shortest_path_queue nil get_shortest_path_result nil] (try (do (set! get_shortest_path_queue [get_shortest_path_start_vertex]) (set! get_shortest_path_distances []) (set! get_shortest_path_i 0) (while (< get_shortest_path_i (:size get_shortest_path_al)) (do (set! get_shortest_path_distances (conj get_shortest_path_distances (- 1))) (set! get_shortest_path_i (+ get_shortest_path_i 1)))) (set! get_shortest_path_distances (assoc get_shortest_path_distances get_shortest_path_start_vertex 0)) (while (> (count get_shortest_path_queue) 0) (do (set! get_shortest_path_current_vertex (front get_shortest_path_queue)) (set! get_shortest_path_queue (pop_front get_shortest_path_queue)) (set! get_shortest_path_current_distance (nth get_shortest_path_distances get_shortest_path_current_vertex)) (set! get_shortest_path_edges (get (:graph get_shortest_path_al) get_shortest_path_current_vertex)) (set! get_shortest_path_j 0) (loop [while_flag_1 true] (when (and while_flag_1 (< get_shortest_path_j (count get_shortest_path_edges))) (do (set! get_shortest_path_edge (nth get_shortest_path_edges get_shortest_path_j)) (set! get_shortest_path_new_distance (+ get_shortest_path_current_distance (:weight get_shortest_path_edge))) (set! get_shortest_path_dest (:destination_vertex get_shortest_path_edge)) (set! get_shortest_path_dest_distance (nth get_shortest_path_distances get_shortest_path_dest)) (cond (and (>= get_shortest_path_dest_distance 0) (>= get_shortest_path_new_distance get_shortest_path_dest_distance)) (do (set! get_shortest_path_j (+ get_shortest_path_j 1)) (recur true)) :else (do (set! get_shortest_path_distances (assoc get_shortest_path_distances get_shortest_path_dest get_shortest_path_new_distance)) (if (= (:weight get_shortest_path_edge) 0) (set! get_shortest_path_queue (push_front get_shortest_path_queue get_shortest_path_dest)) (set! get_shortest_path_queue (conj get_shortest_path_queue get_shortest_path_dest))) (set! get_shortest_path_j (+ get_shortest_path_j 1)) (recur while_flag_1)))))))) (set! get_shortest_path_result (nth get_shortest_path_distances get_shortest_path_finish_vertex)) (when (< get_shortest_path_result 0) (throw (Exception. "No path from start_vertex to finish_vertex."))) (throw (ex-info "return" {:v get_shortest_path_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_g (new_adjacency_list 11))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (add_edge main_g 0 1 0)
      (add_edge main_g 0 3 1)
      (add_edge main_g 1 2 0)
      (add_edge main_g 2 3 0)
      (add_edge main_g 4 2 1)
      (add_edge main_g 4 5 1)
      (add_edge main_g 4 6 1)
      (add_edge main_g 5 9 0)
      (add_edge main_g 6 7 1)
      (add_edge main_g 7 8 1)
      (add_edge main_g 8 10 1)
      (add_edge main_g 9 7 0)
      (add_edge main_g 9 10 1)
      (println (str (get_shortest_path main_g 0 3)))
      (println (str (get_shortest_path main_g 4 10)))
      (println (str (get_shortest_path main_g 4 8)))
      (println (str (get_shortest_path main_g 0 1)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
