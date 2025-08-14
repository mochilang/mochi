(ns main (:refer-clojure :exclude [new_graph add_edge find_component set_component union create_empty_edges boruvka main]))

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

(defn _fetch [url]
  {:data [{:from "" :intensity {:actual 0 :forecast 0 :index ""} :to ""}]})

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare new_graph add_edge find_component set_component union create_empty_edges boruvka main)

(def ^:dynamic add_edge_es nil)

(def ^:dynamic boruvka_comp nil)

(def ^:dynamic boruvka_component_size nil)

(def ^:dynamic boruvka_current_u nil)

(def ^:dynamic boruvka_current_v nil)

(def ^:dynamic boruvka_g nil)

(def ^:dynamic boruvka_i nil)

(def ^:dynamic boruvka_minimum_weight_edge nil)

(def ^:dynamic boruvka_mst_weight nil)

(def ^:dynamic boruvka_num_components nil)

(def ^:dynamic boruvka_res nil)

(def ^:dynamic boruvka_u nil)

(def ^:dynamic boruvka_u_comp nil)

(def ^:dynamic boruvka_v nil)

(def ^:dynamic boruvka_v_comp nil)

(def ^:dynamic boruvka_w nil)

(def ^:dynamic create_empty_edges_i nil)

(def ^:dynamic create_empty_edges_res nil)

(def ^:dynamic main_edges nil)

(def ^:dynamic main_g nil)

(def ^:dynamic set_component_comp nil)

(def ^:dynamic set_component_g nil)

(def ^:dynamic set_component_k nil)

(def ^:dynamic union_comp nil)

(def ^:dynamic union_comp_size nil)

(def ^:dynamic union_g nil)

(defn new_graph [new_graph_num_nodes]
  (try (throw (ex-info "return" {:v {:component {} :edges [] :num_nodes new_graph_num_nodes}})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn add_edge [add_edge_g add_edge_u add_edge_v add_edge_w]
  (binding [add_edge_es nil] (try (do (set! add_edge_es (:edges add_edge_g)) (set! add_edge_es (conj add_edge_es {:u add_edge_u :v add_edge_v :w add_edge_w})) (throw (ex-info "return" {:v {:component (:component add_edge_g) :edges add_edge_es :num_nodes (:num_nodes add_edge_g)}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn find_component [find_component_g find_component_node]
  (try (if (= (get (:component find_component_g) find_component_node) find_component_node) find_component_node (find_component find_component_g (get (:component find_component_g) find_component_node))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn set_component [set_component_g_p set_component_node]
  (binding [set_component_g set_component_g_p set_component_comp nil set_component_k nil] (try (do (when (not= (get (:component set_component_g) set_component_node) set_component_node) (do (set! set_component_comp (:component set_component_g)) (set! set_component_k 0) (while (< set_component_k (:num_nodes set_component_g)) (do (set! set_component_comp (assoc set_component_comp set_component_k (find_component set_component_g set_component_k))) (set! set_component_k (+ set_component_k 1)))) (set! set_component_g {:component set_component_comp :edges (:edges set_component_g) :num_nodes (:num_nodes set_component_g)}))) (throw (ex-info "return" {:v set_component_g}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))) (finally (alter-var-root (var set_component_g) (constantly set_component_g))))))

(defn union [union_g_p union_component_size union_u union_v]
  (binding [union_g union_g_p union_comp nil union_comp_size nil] (try (do (set! union_comp_size union_component_size) (set! union_comp (:component union_g)) (if (<= (nth union_comp_size union_u) (nth union_comp_size union_v)) (do (set! union_comp (assoc union_comp union_u union_v)) (set! union_comp_size (assoc union_comp_size union_v (+ (nth union_comp_size union_v) (nth union_comp_size union_u)))) (set! union_g {:component union_comp :edges (:edges union_g) :num_nodes (:num_nodes union_g)}) (set! union_g (let [__res (set_component union_g union_u)] (do (set! union_g set_component_g) __res)))) (do (set! union_comp (assoc union_comp union_v union_u)) (set! union_comp_size (assoc union_comp_size union_u (+ (nth union_comp_size union_u) (nth union_comp_size union_v)))) (set! union_g {:component union_comp :edges (:edges union_g) :num_nodes (:num_nodes union_g)}) (set! union_g (let [__res (set_component union_g union_v)] (do (set! union_g set_component_g) __res))))) (throw (ex-info "return" {:v {:component_size union_comp_size :graph union_g}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))) (finally (alter-var-root (var union_g) (constantly union_g))))))

(defn create_empty_edges [create_empty_edges_n]
  (binding [create_empty_edges_i nil create_empty_edges_res nil] (try (do (set! create_empty_edges_res []) (set! create_empty_edges_i 0) (while (< create_empty_edges_i create_empty_edges_n) (do (set! create_empty_edges_res (conj create_empty_edges_res {:u (- 0 1) :v (- 0 1) :w (- 0 1)})) (set! create_empty_edges_i (+ create_empty_edges_i 1)))) (throw (ex-info "return" {:v create_empty_edges_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn boruvka [boruvka_g_p]
  (binding [boruvka_g boruvka_g_p boruvka_comp nil boruvka_component_size nil boruvka_current_u nil boruvka_current_v nil boruvka_i nil boruvka_minimum_weight_edge nil boruvka_mst_weight nil boruvka_num_components nil boruvka_res nil boruvka_u nil boruvka_u_comp nil boruvka_v nil boruvka_v_comp nil boruvka_w nil] (try (do (set! boruvka_component_size []) (set! boruvka_i 0) (while (< boruvka_i (:num_nodes boruvka_g)) (do (set! boruvka_component_size (conj boruvka_component_size 1)) (set! boruvka_comp (:component boruvka_g)) (set! boruvka_comp (assoc boruvka_comp boruvka_i boruvka_i)) (set! boruvka_g {:component boruvka_comp :edges (:edges boruvka_g) :num_nodes (:num_nodes boruvka_g)}) (set! boruvka_i (+ boruvka_i 1)))) (set! boruvka_mst_weight 0) (set! boruvka_num_components (:num_nodes boruvka_g)) (set! boruvka_minimum_weight_edge (create_empty_edges (:num_nodes boruvka_g))) (while (> boruvka_num_components 1) (do (doseq [e (:edges boruvka_g)] (do (set! boruvka_u (:u e)) (set! boruvka_v (:v e)) (set! boruvka_w (:w e)) (set! boruvka_u_comp (get (:component boruvka_g) boruvka_u)) (set! boruvka_v_comp (get (:component boruvka_g) boruvka_v)) (when (not= boruvka_u_comp boruvka_v_comp) (do (set! boruvka_current_u (nth boruvka_minimum_weight_edge boruvka_u_comp)) (when (or (= (:u boruvka_current_u) (- 0 1)) (> (:w boruvka_current_u) boruvka_w)) (set! boruvka_minimum_weight_edge (assoc boruvka_minimum_weight_edge boruvka_u_comp {:u boruvka_u :v boruvka_v :w boruvka_w}))) (set! boruvka_current_v (get boruvka_minimum_weight_edge boruvka_v_comp)) (when (or (= (:u boruvka_current_v) (- 0 1)) (> (:w boruvka_current_v) boruvka_w)) (set! boruvka_minimum_weight_edge (assoc boruvka_minimum_weight_edge boruvka_v_comp {:u boruvka_u :v boruvka_v :w boruvka_w}))))))) (doseq [e boruvka_minimum_weight_edge] (when (not= (:u e) (- 0 1)) (do (set! boruvka_u (:u e)) (set! boruvka_v (:v e)) (set! boruvka_w (:w e)) (set! boruvka_u_comp (get (:component boruvka_g) boruvka_u)) (set! boruvka_v_comp (get (:component boruvka_g) boruvka_v)) (when (not= boruvka_u_comp boruvka_v_comp) (do (set! boruvka_mst_weight (+ boruvka_mst_weight boruvka_w)) (set! boruvka_res (let [__res (union boruvka_g boruvka_component_size boruvka_u_comp boruvka_v_comp)] (do (set! boruvka_g union_g) __res))) (set! boruvka_g (:graph boruvka_res)) (set! boruvka_component_size (:component_size boruvka_res)) (println (str (str (str (str "Added edge [" (str boruvka_u)) " - ") (str boruvka_v)) "]")) (println (str "Added weight: " (str boruvka_w))) (println "") (set! boruvka_num_components (- boruvka_num_components 1))))))) (set! boruvka_minimum_weight_edge (create_empty_edges (:num_nodes boruvka_g))))) (println (str "The total weight of the minimal spanning tree is: " (str boruvka_mst_weight))) (throw (ex-info "return" {:v boruvka_mst_weight}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))) (finally (alter-var-root (var boruvka_g) (constantly boruvka_g))))))

(defn main []
  (binding [main_edges nil main_g nil] (do (set! main_g (new_graph 8)) (set! main_edges [[0 1 10] [0 2 6] [0 3 5] [1 3 15] [2 3 4] [3 4 8] [4 5 10] [4 6 6] [4 7 5] [5 7 15] [6 7 4]]) (doseq [e main_edges] (set! main_g (add_edge main_g (nth e 0) (nth e 1) (nth e 2)))) (let [__res (boruvka main_g)] (do (set! main_g boruvka_g) __res)))))

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
