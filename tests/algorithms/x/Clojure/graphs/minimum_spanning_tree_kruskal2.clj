(ns main (:refer-clojure :exclude [new_graph add_edge make_ds find_set union_set sort_edges kruskal print_mst main]))

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

(declare new_graph add_edge make_ds find_set union_set sort_edges kruskal print_mst main)

(def ^:dynamic add_edge_es nil)

(def ^:dynamic add_edge_n nil)

(def ^:dynamic find_set_p nil)

(def ^:dynamic find_set_res nil)

(def ^:dynamic kruskal_added nil)

(def ^:dynamic kruskal_ds nil)

(def ^:dynamic kruskal_e nil)

(def ^:dynamic kruskal_edges nil)

(def ^:dynamic kruskal_fu nil)

(def ^:dynamic kruskal_fv nil)

(def ^:dynamic kruskal_i nil)

(def ^:dynamic kruskal_mst_edges nil)

(def ^:dynamic kruskal_ru nil)

(def ^:dynamic kruskal_rv nil)

(def ^:dynamic main_g nil)

(def ^:dynamic main_mst nil)

(def ^:dynamic make_ds_i nil)

(def ^:dynamic make_ds_parent nil)

(def ^:dynamic make_ds_rank nil)

(def ^:dynamic print_mst_es nil)

(def ^:dynamic sort_edges_arr nil)

(def ^:dynamic sort_edges_i nil)

(def ^:dynamic sort_edges_j nil)

(def ^:dynamic sort_edges_key nil)

(def ^:dynamic sort_edges_temp nil)

(def ^:dynamic union_set_ds1 nil)

(def ^:dynamic union_set_ds2 nil)

(def ^:dynamic union_set_fx nil)

(def ^:dynamic union_set_fy nil)

(def ^:dynamic union_set_p nil)

(def ^:dynamic union_set_r nil)

(def ^:dynamic union_set_x_root nil)

(def ^:dynamic union_set_y_root nil)

(defn new_graph []
  (try (throw (ex-info "return" {:v {:edges [] :num_nodes 0}})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn add_edge [add_edge_g add_edge_u add_edge_v add_edge_w]
  (binding [add_edge_es nil add_edge_n nil] (try (do (set! add_edge_es (:edges add_edge_g)) (set! add_edge_es (conj add_edge_es {:u add_edge_u :v add_edge_v :w add_edge_w})) (set! add_edge_n (:num_nodes add_edge_g)) (when (> add_edge_u add_edge_n) (set! add_edge_n add_edge_u)) (when (> add_edge_v add_edge_n) (set! add_edge_n add_edge_v)) (throw (ex-info "return" {:v {:edges add_edge_es :num_nodes add_edge_n}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn make_ds [make_ds_n]
  (binding [make_ds_i nil make_ds_parent nil make_ds_rank nil] (try (do (set! make_ds_parent []) (set! make_ds_rank []) (set! make_ds_i 0) (while (<= make_ds_i make_ds_n) (do (set! make_ds_parent (conj make_ds_parent make_ds_i)) (set! make_ds_rank (conj make_ds_rank 0)) (set! make_ds_i (+ make_ds_i 1)))) (throw (ex-info "return" {:v {:parent make_ds_parent :rank make_ds_rank}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn find_set [find_set_ds find_set_x]
  (binding [find_set_p nil find_set_res nil] (try (do (when (= (get (:parent find_set_ds) find_set_x) find_set_x) (throw (ex-info "return" {:v {:ds find_set_ds :root find_set_x}}))) (set! find_set_res (find_set find_set_ds (get (:parent find_set_ds) find_set_x))) (set! find_set_p (:parent (:ds find_set_res))) (set! find_set_p (assoc find_set_p find_set_x (:root find_set_res))) (throw (ex-info "return" {:v {:ds {:parent find_set_p :rank (:rank (:ds find_set_res))} :root (:root find_set_res)}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn union_set [union_set_ds union_set_x union_set_y]
  (binding [union_set_ds1 nil union_set_ds2 nil union_set_fx nil union_set_fy nil union_set_p nil union_set_r nil union_set_x_root nil union_set_y_root nil] (try (do (set! union_set_fx (find_set union_set_ds union_set_x)) (set! union_set_ds1 (:ds union_set_fx)) (set! union_set_x_root (:root union_set_fx)) (set! union_set_fy (find_set union_set_ds1 union_set_y)) (set! union_set_ds2 (:ds union_set_fy)) (set! union_set_y_root (:root union_set_fy)) (when (= union_set_x_root union_set_y_root) (throw (ex-info "return" {:v union_set_ds2}))) (set! union_set_p (:parent union_set_ds2)) (set! union_set_r (:rank union_set_ds2)) (if (> (get union_set_r union_set_x_root) (get union_set_r union_set_y_root)) (set! union_set_p (assoc union_set_p union_set_y_root union_set_x_root)) (do (set! union_set_p (assoc union_set_p union_set_x_root union_set_y_root)) (when (= (get union_set_r union_set_x_root) (get union_set_r union_set_y_root)) (set! union_set_r (assoc union_set_r union_set_y_root (+ (get union_set_r union_set_y_root) 1)))))) (throw (ex-info "return" {:v {:parent union_set_p :rank union_set_r}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn sort_edges [sort_edges_edges]
  (binding [sort_edges_arr nil sort_edges_i nil sort_edges_j nil sort_edges_key nil sort_edges_temp nil] (try (do (set! sort_edges_arr sort_edges_edges) (set! sort_edges_i 1) (while (< sort_edges_i (count sort_edges_arr)) (do (set! sort_edges_key (nth sort_edges_arr sort_edges_i)) (set! sort_edges_j (- sort_edges_i 1)) (loop [while_flag_1 true] (when (and while_flag_1 (>= sort_edges_j 0)) (do (set! sort_edges_temp (nth sort_edges_arr sort_edges_j)) (if (or (> (:w sort_edges_temp) (:w sort_edges_key)) (and (= (:w sort_edges_temp) (:w sort_edges_key)) (or (> (:u sort_edges_temp) (:u sort_edges_key)) (and (= (:u sort_edges_temp) (:u sort_edges_key)) (> (:v sort_edges_temp) (:v sort_edges_key)))))) (do (set! sort_edges_arr (assoc sort_edges_arr (+ sort_edges_j 1) sort_edges_temp)) (set! sort_edges_j (- sort_edges_j 1)) (recur while_flag_1)) (recur false))))) (set! sort_edges_arr (assoc sort_edges_arr (+ sort_edges_j 1) sort_edges_key)) (set! sort_edges_i (+ sort_edges_i 1)))) (throw (ex-info "return" {:v sort_edges_arr}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn kruskal [kruskal_g]
  (binding [kruskal_added nil kruskal_ds nil kruskal_e nil kruskal_edges nil kruskal_fu nil kruskal_fv nil kruskal_i nil kruskal_mst_edges nil kruskal_ru nil kruskal_rv nil] (try (do (set! kruskal_edges (sort_edges (:edges kruskal_g))) (set! kruskal_ds (make_ds (:num_nodes kruskal_g))) (set! kruskal_mst_edges []) (set! kruskal_i 0) (set! kruskal_added 0) (while (and (< kruskal_added (- (:num_nodes kruskal_g) 1)) (< kruskal_i (count kruskal_edges))) (do (set! kruskal_e (nth kruskal_edges kruskal_i)) (set! kruskal_i (+ kruskal_i 1)) (set! kruskal_fu (find_set kruskal_ds (:u kruskal_e))) (set! kruskal_ds (:ds kruskal_fu)) (set! kruskal_ru (:root kruskal_fu)) (set! kruskal_fv (find_set kruskal_ds (:v kruskal_e))) (set! kruskal_ds (:ds kruskal_fv)) (set! kruskal_rv (:root kruskal_fv)) (when (not= kruskal_ru kruskal_rv) (do (set! kruskal_mst_edges (conj kruskal_mst_edges kruskal_e)) (set! kruskal_added (+ kruskal_added 1)) (set! kruskal_ds (union_set kruskal_ds kruskal_ru kruskal_rv)))))) (throw (ex-info "return" {:v {:edges kruskal_mst_edges :num_nodes (:num_nodes kruskal_g)}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn print_mst [print_mst_g]
  (binding [print_mst_es nil] (do (set! print_mst_es (sort_edges (:edges print_mst_g))) (doseq [e print_mst_es] (println (str (str (str (str (str (:u e)) "-") (str (:v e))) ":") (str (:w e))))) print_mst_g)))

(defn main []
  (binding [main_g nil main_mst nil] (do (set! main_g (new_graph)) (set! main_g (add_edge main_g 1 2 1)) (set! main_g (add_edge main_g 2 3 2)) (set! main_g (add_edge main_g 3 4 1)) (set! main_g (add_edge main_g 3 5 100)) (set! main_g (add_edge main_g 4 5 5)) (set! main_mst (kruskal main_g)) (set! main_mst (print_mst main_mst)))))

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
