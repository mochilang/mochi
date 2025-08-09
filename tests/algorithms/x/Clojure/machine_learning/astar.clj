(ns main (:refer-clojure :exclude [get_neighbours contains get_node astar create_world mark_path print_world]))

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

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare get_neighbours contains get_node astar create_world mark_path print_world)

(def ^:dynamic astar_closed nil)

(def ^:dynamic astar_current nil)

(def ^:dynamic astar_dx nil)

(def ^:dynamic astar_dy nil)

(def ^:dynamic astar_f nil)

(def ^:dynamic astar_g nil)

(def ^:dynamic astar_h nil)

(def ^:dynamic astar_i nil)

(def ^:dynamic astar_j nil)

(def ^:dynamic astar_k nil)

(def ^:dynamic astar_min_index nil)

(def ^:dynamic astar_neighbours nil)

(def ^:dynamic astar_new_open nil)

(def ^:dynamic astar_open nil)

(def ^:dynamic astar_path nil)

(def ^:dynamic astar_rev nil)

(def ^:dynamic astar_skip nil)

(def ^:dynamic create_world_i nil)

(def ^:dynamic create_world_j nil)

(def ^:dynamic create_world_row nil)

(def ^:dynamic create_world_world nil)

(def ^:dynamic get_neighbours_deltas nil)

(def ^:dynamic get_neighbours_neighbours nil)

(def ^:dynamic get_neighbours_nx nil)

(def ^:dynamic get_neighbours_ny nil)

(def ^:dynamic mark_path_world nil)

(defn get_neighbours [get_neighbours_p get_neighbours_x_limit get_neighbours_y_limit]
  (binding [get_neighbours_deltas nil get_neighbours_neighbours nil get_neighbours_nx nil get_neighbours_ny nil] (try (do (set! get_neighbours_deltas [{:x (- 0 1) :y (- 0 1)} {:x (- 0 1) :y 0} {:x (- 0 1) :y 1} {:x 0 :y (- 0 1)} {:x 0 :y 1} {:x 1 :y (- 0 1)} {:x 1 :y 0} {:x 1 :y 1}]) (set! get_neighbours_neighbours []) (doseq [d get_neighbours_deltas] (do (set! get_neighbours_nx (+ (:x get_neighbours_p) (:x d))) (set! get_neighbours_ny (+ (:y get_neighbours_p) (:y d))) (when (and (and (and (<= 0 get_neighbours_nx) (< get_neighbours_nx get_neighbours_x_limit)) (<= 0 get_neighbours_ny)) (< get_neighbours_ny get_neighbours_y_limit)) (set! get_neighbours_neighbours (conj get_neighbours_neighbours {:x get_neighbours_nx :y get_neighbours_ny}))))) (throw (ex-info "return" {:v get_neighbours_neighbours}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn contains [contains_nodes contains_p]
  (try (do (doseq [n contains_nodes] (when (and (= (:x (:pos n)) (:x contains_p)) (= (:y (:pos n)) (:y contains_p))) (throw (ex-info "return" {:v true})))) (throw (ex-info "return" {:v false}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn get_node [get_node_nodes get_node_p]
  (try (do (doseq [n get_node_nodes] (when (and (= (:x (:pos n)) (:x get_node_p)) (= (:y (:pos n)) (:y get_node_p))) (throw (ex-info "return" {:v n})))) (throw (ex-info "return" {:v {:f 0 :g 0 :h 0 :parent {:x (- 0 1) :y (- 0 1)} :pos get_node_p}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn astar [astar_x_limit astar_y_limit astar_start astar_goal]
  (binding [astar_closed nil astar_current nil astar_dx nil astar_dy nil astar_f nil astar_g nil astar_h nil astar_i nil astar_j nil astar_k nil astar_min_index nil astar_neighbours nil astar_new_open nil astar_open nil astar_path nil astar_rev nil astar_skip nil] (try (do (set! astar_open []) (set! astar_closed []) (set! astar_open (conj astar_open {:f 0 :g 0 :h 0 :parent {:x (- 0 1) :y (- 0 1)} :pos astar_start})) (set! astar_current (nth astar_open 0)) (loop [while_flag_1 true] (when (and while_flag_1 (> (count astar_open) 0)) (do (set! astar_min_index 0) (set! astar_i 1) (while (< astar_i (count astar_open)) (do (when (< (:f (nth astar_open astar_i)) (:f (nth astar_open astar_min_index))) (set! astar_min_index astar_i)) (set! astar_i (+ astar_i 1)))) (set! astar_current (nth astar_open astar_min_index)) (set! astar_new_open []) (set! astar_j 0) (while (< astar_j (count astar_open)) (do (when (not= astar_j astar_min_index) (set! astar_new_open (conj astar_new_open (nth astar_open astar_j)))) (set! astar_j (+ astar_j 1)))) (set! astar_open astar_new_open) (set! astar_closed (conj astar_closed astar_current)) (cond (and (= (:x (:pos astar_current)) (:x astar_goal)) (= (:y (:pos astar_current)) (:y astar_goal))) (recur false) :else (do (set! astar_neighbours (get_neighbours (:pos astar_current) astar_x_limit astar_y_limit)) (loop [np_seq astar_neighbours] (when (seq np_seq) (let [np (first np_seq)] (cond (contains astar_closed np) (recur (rest np_seq)) astar_skip (recur (rest np_seq)) :else (do (set! astar_g (+ (:g astar_current) 1)) (set! astar_dx (- (:x astar_goal) (:x np))) (set! astar_dy (- (:y astar_goal) (:y np))) (set! astar_h (+ (* astar_dx astar_dx) (* astar_dy astar_dy))) (set! astar_f (+ astar_g astar_h)) (set! astar_skip false) (doseq [node astar_open] (when (and (and (= (:x (:pos node)) (:x np)) (= (:y (:pos node)) (:y np))) (< (:f node) astar_f)) (set! astar_skip true))) (set! astar_open (conj astar_open {:f astar_f :g astar_g :h astar_h :parent (:pos astar_current) :pos np})) (recur (rest np_seq))))))) (recur while_flag_1)))))) (set! astar_path []) (set! astar_path (conj astar_path (:pos astar_current))) (while (not (and (= (:x (:parent astar_current)) (- 0 1)) (= (:y (:parent astar_current)) (- 0 1)))) (do (set! astar_current (get_node astar_closed (:parent astar_current))) (set! astar_path (conj astar_path (:pos astar_current))))) (set! astar_rev []) (set! astar_k (- (count astar_path) 1)) (while (>= astar_k 0) (do (set! astar_rev (conj astar_rev (nth astar_path astar_k))) (set! astar_k (- astar_k 1)))) (throw (ex-info "return" {:v astar_rev}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn create_world [create_world_x_limit create_world_y_limit]
  (binding [create_world_i nil create_world_j nil create_world_row nil create_world_world nil] (try (do (set! create_world_world []) (set! create_world_i 0) (while (< create_world_i create_world_x_limit) (do (set! create_world_row []) (set! create_world_j 0) (while (< create_world_j create_world_y_limit) (do (set! create_world_row (conj create_world_row 0)) (set! create_world_j (+ create_world_j 1)))) (set! create_world_world (conj create_world_world create_world_row)) (set! create_world_i (+ create_world_i 1)))) (throw (ex-info "return" {:v create_world_world}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn mark_path [mark_path_world_p mark_path_path]
  (binding [mark_path_world nil] (do (set! mark_path_world mark_path_world_p) (doseq [p mark_path_path] (set! mark_path_world (assoc-in mark_path_world [(:x p) (:y p)] 1))) mark_path_world)))

(defn print_world [print_world_world]
  (do (doseq [row print_world_world] (println (str row))) print_world_world))

(def ^:dynamic main_world_x 5)

(def ^:dynamic main_world_y 5)

(def ^:dynamic main_start {:x 0 :y 0})

(def ^:dynamic main_goal {:x 4 :y 4})

(def ^:dynamic main_path (astar main_world_x main_world_y main_start main_goal))

(def ^:dynamic main_world (create_world main_world_x main_world_y))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (str (str (str (str (str (str (str "path from (" (str (:x main_start))) ", ") (str (:y main_start))) ") to (") (str (:x main_goal))) ", ") (str (:y main_goal))) ")"))
      (mark_path main_world main_path)
      (print_world main_world)
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
