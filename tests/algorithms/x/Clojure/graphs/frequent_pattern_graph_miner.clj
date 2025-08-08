(ns main (:refer-clojure :exclude [contains get_distinct_edge get_bitcode count_ones get_frequency_table get_nodes get_cluster get_support contains_bits max_cluster_key get_cluster_codes create_edge construct_graph copy_list my_dfs find_freq_subgraph_given_support node_edges freq_subgraphs_edge_list print_all main]))

(require 'clojure.set)

(defrecord Entry [edge count bit])

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

(def ^:dynamic construct_graph_c nil)

(def ^:dynamic construct_graph_code nil)

(def ^:dynamic construct_graph_graph nil)

(def ^:dynamic construct_graph_i nil)

(def ^:dynamic construct_graph_keys nil)

(def ^:dynamic construct_graph_maxk nil)

(def ^:dynamic construct_graph_top_codes nil)

(def ^:dynamic contains_bits_c1 nil)

(def ^:dynamic contains_bits_c2 nil)

(def ^:dynamic contains_bits_i nil)

(def ^:dynamic copy_list_n nil)

(def ^:dynamic count_ones_c nil)

(def ^:dynamic count_ones_i nil)

(def ^:dynamic count_v nil)

(def ^:dynamic create_edge_c2 nil)

(def ^:dynamic create_edge_codes1 nil)

(def ^:dynamic create_edge_codes2 nil)

(def ^:dynamic create_edge_graph nil)

(def ^:dynamic create_edge_i_code nil)

(def ^:dynamic create_edge_idx1 nil)

(def ^:dynamic create_edge_j nil)

(def ^:dynamic create_edge_j_code nil)

(def ^:dynamic create_edge_keys nil)

(def ^:dynamic find_freq_subgraph_given_support_codes nil)

(def ^:dynamic find_freq_subgraph_given_support_i nil)

(def ^:dynamic find_freq_subgraph_given_support_k nil)

(def ^:dynamic freq_subgraphs_edge_list_a nil)

(def ^:dynamic freq_subgraphs_edge_list_b nil)

(def ^:dynamic freq_subgraphs_edge_list_code nil)

(def ^:dynamic freq_subgraphs_edge_list_e nil)

(def ^:dynamic freq_subgraphs_edge_list_edge nil)

(def ^:dynamic freq_subgraphs_edge_list_edge_list nil)

(def ^:dynamic freq_subgraphs_edge_list_el nil)

(def ^:dynamic freq_subgraphs_edge_list_freq_sub_el nil)

(def ^:dynamic freq_subgraphs_edge_list_j nil)

(def ^:dynamic get_bitcode_bitcode nil)

(def ^:dynamic get_bitcode_found nil)

(def ^:dynamic get_bitcode_i nil)

(def ^:dynamic get_cluster_clusters nil)

(def ^:dynamic get_cluster_code nil)

(def ^:dynamic get_cluster_i nil)

(def ^:dynamic get_cluster_weights nil)

(def ^:dynamic get_cluster_wt nil)

(def ^:dynamic get_distinct_edge_distinct nil)

(def ^:dynamic get_distinct_edge_e nil)

(def ^:dynamic get_frequency_table_bit nil)

(def ^:dynamic get_frequency_table_cnt nil)

(def ^:dynamic get_frequency_table_distinct nil)

(def ^:dynamic get_frequency_table_entry nil)

(def ^:dynamic get_frequency_table_i nil)

(def ^:dynamic get_frequency_table_j nil)

(def ^:dynamic get_frequency_table_max_i nil)

(def ^:dynamic get_frequency_table_table nil)

(def ^:dynamic get_frequency_table_tmp nil)

(def ^:dynamic get_nodes_code nil)

(def ^:dynamic get_nodes_edge nil)

(def ^:dynamic get_nodes_keys nil)

(def ^:dynamic get_nodes_nodes nil)

(def ^:dynamic get_support_i nil)

(def ^:dynamic get_support_sup nil)

(def ^:dynamic get_support_w nil)

(def ^:dynamic main_clusters nil)

(def ^:dynamic main_freq_subgraph_edge_list nil)

(def ^:dynamic main_frequency_table nil)

(def ^:dynamic main_graph nil)

(def ^:dynamic main_nodes nil)

(def ^:dynamic main_support nil)

(def ^:dynamic max_cluster_key_i nil)

(def ^:dynamic max_cluster_key_m nil)

(def ^:dynamic max_cluster_key_w nil)

(def ^:dynamic my_dfs_new_path nil)

(def ^:dynamic my_dfs_seen nil)

(def ^:dynamic print_all_code nil)

(def ^:dynamic print_all_i nil)

(def ^:dynamic print_all_j nil)

(def ^:dynamic print_all_k nil)

(def ^:dynamic print_all_key nil)

(def ^:dynamic print_all_w nil)

(declare contains get_distinct_edge get_bitcode count_ones get_frequency_table get_nodes get_cluster get_support contains_bits max_cluster_key get_cluster_codes create_edge construct_graph copy_list my_dfs find_freq_subgraph_given_support node_edges freq_subgraphs_edge_list print_all main)

(def ^:dynamic main_EDGE_ARRAY [[["ab" "e1"] ["ac" "e3"] ["ad" "e5"] ["bc" "e4"] ["bd" "e2"] ["be" "e6"] ["bh" "e12"] ["cd" "e2"] ["ce" "e4"] ["de" "e1"] ["df" "e8"] ["dg" "e5"] ["dh" "e10"] ["ef" "e3"] ["eg" "e2"] ["fg" "e6"] ["gh" "e6"] ["hi" "e3"]] [["ab" "e1"] ["ac" "e3"] ["ad" "e5"] ["bc" "e4"] ["bd" "e2"] ["be" "e6"] ["cd" "e2"] ["de" "e1"] ["df" "e8"] ["ef" "e3"] ["eg" "e2"] ["fg" "e6"]] [["ab" "e1"] ["ac" "e3"] ["bc" "e4"] ["bd" "e2"] ["de" "e1"] ["df" "e8"] ["dg" "e5"] ["ef" "e3"] ["eg" "e2"] ["eh" "e12"] ["fg" "e6"] ["fh" "e10"] ["gh" "e6"]] [["ab" "e1"] ["ac" "e3"] ["bc" "e4"] ["bd" "e2"] ["bh" "e12"] ["cd" "e2"] ["df" "e8"] ["dh" "e10"]] [["ab" "e1"] ["ac" "e3"] ["ad" "e5"] ["bc" "e4"] ["bd" "e2"] ["cd" "e2"] ["ce" "e4"] ["de" "e1"] ["df" "e8"] ["dg" "e5"] ["ef" "e3"] ["eg" "e2"] ["fg" "e6"]]])

(defn contains [contains_lst contains_item]
  (try (do (doseq [v contains_lst] (when (= v contains_item) (throw (ex-info "return" {:v true})))) (throw (ex-info "return" {:v false}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn get_distinct_edge [get_distinct_edge_edge_array]
  (binding [get_distinct_edge_distinct nil get_distinct_edge_e nil] (try (do (set! get_distinct_edge_distinct []) (doseq [row get_distinct_edge_edge_array] (doseq [item row] (do (set! get_distinct_edge_e (nth item 0)) (when (not (contains get_distinct_edge_distinct get_distinct_edge_e)) (set! get_distinct_edge_distinct (conj get_distinct_edge_distinct get_distinct_edge_e)))))) (throw (ex-info "return" {:v get_distinct_edge_distinct}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn get_bitcode [get_bitcode_edge_array get_bitcode_de]
  (binding [get_bitcode_bitcode nil get_bitcode_found nil get_bitcode_i nil] (try (do (set! get_bitcode_bitcode "") (set! get_bitcode_i 0) (while (< get_bitcode_i (count get_bitcode_edge_array)) (do (set! get_bitcode_found false) (loop [item_seq (nth get_bitcode_edge_array get_bitcode_i)] (when (seq item_seq) (let [item (first item_seq)] (cond (= (nth item 0) get_bitcode_de) (do (set! get_bitcode_found true) (recur nil)) :else (recur (rest item_seq)))))) (if get_bitcode_found (set! get_bitcode_bitcode (str get_bitcode_bitcode "1")) (set! get_bitcode_bitcode (str get_bitcode_bitcode "0"))) (set! get_bitcode_i (+ get_bitcode_i 1)))) (throw (ex-info "return" {:v get_bitcode_bitcode}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn count_ones [count_ones_s]
  (binding [count_ones_c nil count_ones_i nil] (try (do (set! count_ones_c 0) (set! count_ones_i 0) (while (< count_ones_i (count count_ones_s)) (do (when (= (subs count_ones_s count_ones_i (min (+ count_ones_i 1) (count count_ones_s))) "1") (set! count_ones_c (+ count_ones_c 1))) (set! count_ones_i (+ count_ones_i 1)))) (throw (ex-info "return" {:v count_ones_c}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn get_frequency_table [get_frequency_table_edge_array]
  (binding [get_frequency_table_bit nil get_frequency_table_cnt nil get_frequency_table_distinct nil get_frequency_table_entry nil get_frequency_table_i nil get_frequency_table_j nil get_frequency_table_max_i nil get_frequency_table_table nil get_frequency_table_tmp nil] (try (do (set! get_frequency_table_distinct (get_distinct_edge get_frequency_table_edge_array)) (set! get_frequency_table_table []) (doseq [e get_frequency_table_distinct] (do (set! get_frequency_table_bit (get_bitcode get_frequency_table_edge_array e)) (set! get_frequency_table_cnt (count_ones get_frequency_table_bit)) (set! get_frequency_table_entry {"bit" get_frequency_table_bit "count" (str get_frequency_table_cnt) "edge" e}) (set! get_frequency_table_table (conj get_frequency_table_table get_frequency_table_entry)))) (set! get_frequency_table_i 0) (while (< get_frequency_table_i (count get_frequency_table_table)) (do (set! get_frequency_table_max_i get_frequency_table_i) (set! get_frequency_table_j (+ get_frequency_table_i 1)) (while (< get_frequency_table_j (count get_frequency_table_table)) (do (when (> (toi (get (nth get_frequency_table_table get_frequency_table_j) "count")) (toi (get (nth get_frequency_table_table get_frequency_table_max_i) "count"))) (set! get_frequency_table_max_i get_frequency_table_j)) (set! get_frequency_table_j (+ get_frequency_table_j 1)))) (set! get_frequency_table_tmp (nth get_frequency_table_table get_frequency_table_i)) (set! get_frequency_table_table (assoc get_frequency_table_table get_frequency_table_i (nth get_frequency_table_table get_frequency_table_max_i))) (set! get_frequency_table_table (assoc get_frequency_table_table get_frequency_table_max_i get_frequency_table_tmp)) (set! get_frequency_table_i (+ get_frequency_table_i 1)))) (throw (ex-info "return" {:v get_frequency_table_table}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn get_nodes [get_nodes_freq_table]
  (binding [get_nodes_code nil get_nodes_edge nil get_nodes_keys nil get_nodes_nodes nil] (try (do (set! get_nodes_nodes {}) (set! get_nodes_keys []) (doseq [f get_nodes_freq_table] (do (set! get_nodes_code (get f "bit")) (set! get_nodes_edge (get f "edge")) (if (in get_nodes_code get_nodes_nodes) (set! get_nodes_nodes (assoc get_nodes_nodes get_nodes_code (conj (get get_nodes_nodes get_nodes_code) get_nodes_edge))) (do (set! get_nodes_nodes (assoc get_nodes_nodes get_nodes_code [get_nodes_edge])) (set! get_nodes_keys (conj get_nodes_keys get_nodes_code)))))) (throw (ex-info "return" {:v {:keys get_nodes_keys :map get_nodes_nodes}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn get_cluster [get_cluster_nodes]
  (binding [get_cluster_clusters nil get_cluster_code nil get_cluster_i nil get_cluster_weights nil get_cluster_wt nil] (try (do (set! get_cluster_clusters {}) (set! get_cluster_weights []) (set! get_cluster_i 0) (while (< get_cluster_i (count (:keys get_cluster_nodes))) (do (set! get_cluster_code (get (:keys get_cluster_nodes) get_cluster_i)) (set! get_cluster_wt (count_ones get_cluster_code)) (if (in get_cluster_wt get_cluster_clusters) (set! get_cluster_clusters (assoc get_cluster_clusters get_cluster_wt (conj (get get_cluster_clusters get_cluster_wt) get_cluster_code))) (do (set! get_cluster_clusters (assoc get_cluster_clusters get_cluster_wt [get_cluster_code])) (set! get_cluster_weights (conj get_cluster_weights get_cluster_wt)))) (set! get_cluster_i (+ get_cluster_i 1)))) (throw (ex-info "return" {:v {:clusters get_cluster_clusters :weights get_cluster_weights}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn get_support [get_support_clusters]
  (binding [get_support_i nil get_support_sup nil get_support_w nil] (try (do (set! get_support_sup []) (set! get_support_i 0) (while (< get_support_i (count (:weights get_support_clusters))) (do (set! get_support_w (get (:weights get_support_clusters) get_support_i)) (set! get_support_sup (conj get_support_sup (quot (* get_support_w 100) (count (:weights get_support_clusters))))) (set! get_support_i (+ get_support_i 1)))) (throw (ex-info "return" {:v get_support_sup}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn contains_bits [contains_bits_a contains_bits_b]
  (binding [contains_bits_c1 nil contains_bits_c2 nil contains_bits_i nil] (try (do (set! contains_bits_i 0) (while (< contains_bits_i (count contains_bits_a)) (do (set! contains_bits_c1 (subs contains_bits_a contains_bits_i (min (+ contains_bits_i 1) (count contains_bits_a)))) (set! contains_bits_c2 (subs contains_bits_b contains_bits_i (min (+ contains_bits_i 1) (count contains_bits_b)))) (when (and (= contains_bits_c1 "1") (not= contains_bits_c2 "1")) (throw (ex-info "return" {:v false}))) (set! contains_bits_i (+ contains_bits_i 1)))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn max_cluster_key [max_cluster_key_clusters]
  (binding [max_cluster_key_i nil max_cluster_key_m nil max_cluster_key_w nil] (try (do (set! max_cluster_key_m 0) (set! max_cluster_key_i 0) (while (< max_cluster_key_i (count (:weights max_cluster_key_clusters))) (do (set! max_cluster_key_w (get (:weights max_cluster_key_clusters) max_cluster_key_i)) (when (> max_cluster_key_w max_cluster_key_m) (set! max_cluster_key_m max_cluster_key_w)) (set! max_cluster_key_i (+ max_cluster_key_i 1)))) (throw (ex-info "return" {:v max_cluster_key_m}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn get_cluster_codes [get_cluster_codes_clusters get_cluster_codes_wt]
  (try (if (in get_cluster_codes_wt (:clusters get_cluster_codes_clusters)) (get (:clusters get_cluster_codes_clusters) get_cluster_codes_wt) []) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn create_edge [create_edge_nodes create_edge_graph_p create_edge_gkeys create_edge_clusters create_edge_c1 create_edge_maxk]
  (binding [count_v nil create_edge_c2 nil create_edge_codes1 nil create_edge_codes2 nil create_edge_graph nil create_edge_i_code nil create_edge_idx1 nil create_edge_j nil create_edge_j_code nil create_edge_keys nil] (try (do (set! create_edge_graph create_edge_graph_p) (set! create_edge_keys create_edge_gkeys) (set! create_edge_codes1 (get_cluster_codes create_edge_clusters create_edge_c1)) (set! create_edge_idx1 0) (while (< create_edge_idx1 (count create_edge_codes1)) (do (set! create_edge_i_code (nth create_edge_codes1 create_edge_idx1)) (set! count_v 0) (set! create_edge_c2 (+ create_edge_c1 1)) (loop [while_flag_1 true] (when (and while_flag_1 (<= create_edge_c2 create_edge_maxk)) (do (set! create_edge_codes2 (get_cluster_codes create_edge_clusters create_edge_c2)) (set! create_edge_j 0) (while (< create_edge_j (count create_edge_codes2)) (do (set! create_edge_j_code (nth create_edge_codes2 create_edge_j)) (when (contains_bits create_edge_i_code create_edge_j_code) (do (if (in create_edge_i_code create_edge_graph) (set! create_edge_graph (assoc create_edge_graph create_edge_i_code (conj (get create_edge_graph create_edge_i_code) create_edge_j_code))) (do (set! create_edge_graph (assoc create_edge_graph create_edge_i_code [create_edge_j_code])) (when (not (contains create_edge_keys create_edge_i_code)) (set! create_edge_keys (conj create_edge_keys create_edge_i_code))))) (when (not (contains create_edge_keys create_edge_j_code)) (set! create_edge_keys (conj create_edge_keys create_edge_j_code))) (set! count_v (+ count_v 1)))) (set! create_edge_j (+ create_edge_j 1)))) (if (= count_v 0) (do (set! create_edge_c2 (+ create_edge_c2 1)) (recur while_flag_1)) (recur false))))) (set! create_edge_idx1 (+ create_edge_idx1 1)))) (throw (ex-info "return" {:v create_edge_keys}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn construct_graph [construct_graph_clusters construct_graph_nodes]
  (binding [construct_graph_c nil construct_graph_code nil construct_graph_graph nil construct_graph_i nil construct_graph_keys nil construct_graph_maxk nil construct_graph_top_codes nil] (try (do (set! construct_graph_maxk (max_cluster_key construct_graph_clusters)) (set! construct_graph_top_codes (get_cluster_codes construct_graph_clusters construct_graph_maxk)) (set! construct_graph_graph {}) (set! construct_graph_keys ["Header"]) (set! construct_graph_graph (assoc construct_graph_graph "Header" [])) (set! construct_graph_i 0) (while (< construct_graph_i (count construct_graph_top_codes)) (do (set! construct_graph_code (nth construct_graph_top_codes construct_graph_i)) (set! construct_graph_graph (assoc construct_graph_graph "Header" (conj (get construct_graph_graph "Header") construct_graph_code))) (set! construct_graph_graph (assoc construct_graph_graph construct_graph_code ["Header"])) (set! construct_graph_keys (conj construct_graph_keys construct_graph_code)) (set! construct_graph_i (+ construct_graph_i 1)))) (set! construct_graph_c 1) (while (< construct_graph_c construct_graph_maxk) (do (set! construct_graph_keys (create_edge construct_graph_nodes construct_graph_graph construct_graph_keys construct_graph_clusters construct_graph_c construct_graph_maxk)) (set! construct_graph_c (+ construct_graph_c 1)))) (throw (ex-info "return" {:v {:edges construct_graph_graph :keys construct_graph_keys}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_paths [])

(defn copy_list [copy_list_lst]
  (binding [copy_list_n nil] (try (do (set! copy_list_n []) (doseq [v copy_list_lst] (set! copy_list_n (conj copy_list_n v))) (throw (ex-info "return" {:v copy_list_n}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn my_dfs [my_dfs_graph my_dfs_start my_dfs_end my_dfs_path]
  (binding [my_dfs_new_path nil my_dfs_seen nil] (try (do (set! my_dfs_new_path (copy_list my_dfs_path)) (set! my_dfs_new_path (conj my_dfs_new_path my_dfs_start)) (when (= my_dfs_start my_dfs_end) (do (alter-var-root (var main_paths) (fn [_] (conj main_paths my_dfs_new_path))) (throw (ex-info "return" {:v nil})))) (doseq [node (get my_dfs_graph my_dfs_start)] (do (set! my_dfs_seen false) (doseq [p my_dfs_new_path] (when (= p node) (set! my_dfs_seen true))) (when (not my_dfs_seen) (my_dfs my_dfs_graph node my_dfs_end my_dfs_new_path))))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn find_freq_subgraph_given_support [find_freq_subgraph_given_support_s find_freq_subgraph_given_support_clusters find_freq_subgraph_given_support_graph]
  (binding [find_freq_subgraph_given_support_codes nil find_freq_subgraph_given_support_i nil find_freq_subgraph_given_support_k nil] (do (set! find_freq_subgraph_given_support_k (quot (* find_freq_subgraph_given_support_s (count (:weights find_freq_subgraph_given_support_clusters))) 100)) (set! find_freq_subgraph_given_support_codes (get_cluster_codes find_freq_subgraph_given_support_clusters find_freq_subgraph_given_support_k)) (set! find_freq_subgraph_given_support_i 0) (while (< find_freq_subgraph_given_support_i (count find_freq_subgraph_given_support_codes)) (do (my_dfs (:edges find_freq_subgraph_given_support_graph) (nth find_freq_subgraph_given_support_codes find_freq_subgraph_given_support_i) "Header" []) (set! find_freq_subgraph_given_support_i (+ find_freq_subgraph_given_support_i 1)))))))

(defn node_edges [node_edges_nodes node_edges_code]
  (try (throw (ex-info "return" {:v (get (:map node_edges_nodes) node_edges_code)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn freq_subgraphs_edge_list [main_paths freq_subgraphs_edge_list_nodes]
  (binding [freq_subgraphs_edge_list_a nil freq_subgraphs_edge_list_b nil freq_subgraphs_edge_list_code nil freq_subgraphs_edge_list_e nil freq_subgraphs_edge_list_edge nil freq_subgraphs_edge_list_edge_list nil freq_subgraphs_edge_list_el nil freq_subgraphs_edge_list_freq_sub_el nil freq_subgraphs_edge_list_j nil] (try (do (set! freq_subgraphs_edge_list_freq_sub_el []) (doseq [path main_paths] (do (set! freq_subgraphs_edge_list_el []) (set! freq_subgraphs_edge_list_j 0) (while (< freq_subgraphs_edge_list_j (- (count path) 1)) (do (set! freq_subgraphs_edge_list_code (nth path freq_subgraphs_edge_list_j)) (set! freq_subgraphs_edge_list_edge_list (node_edges freq_subgraphs_edge_list_nodes freq_subgraphs_edge_list_code)) (set! freq_subgraphs_edge_list_e 0) (while (< freq_subgraphs_edge_list_e (count freq_subgraphs_edge_list_edge_list)) (do (set! freq_subgraphs_edge_list_edge (nth freq_subgraphs_edge_list_edge_list freq_subgraphs_edge_list_e)) (set! freq_subgraphs_edge_list_a (subs freq_subgraphs_edge_list_edge 0 (min 1 (count freq_subgraphs_edge_list_edge)))) (set! freq_subgraphs_edge_list_b (subs freq_subgraphs_edge_list_edge 1 (min 2 (count freq_subgraphs_edge_list_edge)))) (set! freq_subgraphs_edge_list_el (conj freq_subgraphs_edge_list_el [freq_subgraphs_edge_list_a freq_subgraphs_edge_list_b])) (set! freq_subgraphs_edge_list_e (+ freq_subgraphs_edge_list_e 1)))) (set! freq_subgraphs_edge_list_j (+ freq_subgraphs_edge_list_j 1)))) (set! freq_subgraphs_edge_list_freq_sub_el (conj freq_subgraphs_edge_list_freq_sub_el freq_subgraphs_edge_list_el)))) (throw (ex-info "return" {:v freq_subgraphs_edge_list_freq_sub_el}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn print_all [print_all_nodes print_all_support print_all_clusters print_all_graph print_all_freq_subgraph_edge_list]
  (binding [print_all_code nil print_all_i nil print_all_j nil print_all_k nil print_all_key nil print_all_w nil] (do (println "\nNodes\n") (set! print_all_i 0) (while (< print_all_i (count (:keys print_all_nodes))) (do (set! print_all_code (get (:keys print_all_nodes) print_all_i)) (println print_all_code) (println (get (:map print_all_nodes) print_all_code)) (set! print_all_i (+ print_all_i 1)))) (println "\nSupport\n") (println print_all_support) (println "\nCluster\n") (set! print_all_j 0) (while (< print_all_j (count (:weights print_all_clusters))) (do (set! print_all_w (get (:weights print_all_clusters) print_all_j)) (println (str (str (str print_all_w) ":") (str (get (:clusters print_all_clusters) print_all_w)))) (set! print_all_j (+ print_all_j 1)))) (println "\nGraph\n") (set! print_all_k 0) (while (< print_all_k (count (:keys print_all_graph))) (do (set! print_all_key (get (:keys print_all_graph) print_all_k)) (println print_all_key) (println (get (:edges print_all_graph) print_all_key)) (set! print_all_k (+ print_all_k 1)))) (println "\nEdge List of Frequent subgraphs\n") (doseq [el print_all_freq_subgraph_edge_list] (println el)))))

(defn main []
  (binding [main_clusters nil main_freq_subgraph_edge_list nil main_frequency_table nil main_graph nil main_nodes nil main_support nil] (do (set! main_frequency_table (get_frequency_table main_EDGE_ARRAY)) (set! main_nodes (get_nodes main_frequency_table)) (set! main_clusters (get_cluster main_nodes)) (set! main_support (get_support main_clusters)) (set! main_graph (construct_graph main_clusters main_nodes)) (find_freq_subgraph_given_support 60 main_clusters main_graph) (set! main_freq_subgraph_edge_list (freq_subgraphs_edge_list main_paths main_nodes)) (print_all main_nodes main_support main_clusters main_graph main_freq_subgraph_edge_list))))

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
