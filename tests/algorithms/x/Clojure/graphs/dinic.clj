(ns main (:refer-clojure :exclude [pow2 min2 new_dinic add_edge dfs max_flow]))

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

(declare pow2 min2 new_dinic add_edge dfs max_flow)

(def ^:dynamic add_edge_adj nil)

(def ^:dynamic add_edge_e1 nil)

(def ^:dynamic add_edge_e2 nil)

(def ^:dynamic add_edge_g nil)

(def ^:dynamic add_edge_list_a nil)

(def ^:dynamic add_edge_list_b nil)

(def ^:dynamic dfs_adj_all nil)

(def ^:dynamic dfs_adj_to nil)

(def ^:dynamic dfs_adj_v nil)

(def ^:dynamic dfs_avail nil)

(def ^:dynamic dfs_back nil)

(def ^:dynamic dfs_e nil)

(def ^:dynamic dfs_g nil)

(def ^:dynamic dfs_i nil)

(def ^:dynamic dfs_ptr nil)

(def ^:dynamic dfs_pushed nil)

(def ^:dynamic dfs_to nil)

(def ^:dynamic main_v nil)

(def ^:dynamic max_flow_e nil)

(def ^:dynamic max_flow_edges nil)

(def ^:dynamic max_flow_flow nil)

(def ^:dynamic max_flow_g nil)

(def ^:dynamic max_flow_i nil)

(def ^:dynamic max_flow_j nil)

(def ^:dynamic max_flow_l nil)

(def ^:dynamic max_flow_lvl nil)

(def ^:dynamic max_flow_lvl_inner nil)

(def ^:dynamic max_flow_p nil)

(def ^:dynamic max_flow_ptr nil)

(def ^:dynamic max_flow_q nil)

(def ^:dynamic max_flow_qe nil)

(def ^:dynamic max_flow_qi nil)

(def ^:dynamic max_flow_residual nil)

(def ^:dynamic max_flow_threshold nil)

(def ^:dynamic max_flow_to nil)

(def ^:dynamic max_flow_v nil)

(def ^:dynamic new_dinic_adj nil)

(def ^:dynamic new_dinic_edges nil)

(def ^:dynamic new_dinic_i nil)

(def ^:dynamic new_dinic_lvl nil)

(def ^:dynamic new_dinic_ptr nil)

(def ^:dynamic new_dinic_q nil)

(def ^:dynamic pow2_i nil)

(def ^:dynamic pow2_res nil)

(def ^:dynamic main_INF 1000000000)

(defn pow2 [pow2_k]
  (binding [pow2_i nil pow2_res nil] (try (do (set! pow2_res 1) (set! pow2_i 0) (while (< pow2_i pow2_k) (do (set! pow2_res (* pow2_res 2)) (set! pow2_i (+ pow2_i 1)))) (throw (ex-info "return" {:v pow2_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn min2 [min2_a min2_b]
  (try (if (< min2_a min2_b) min2_a min2_b) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn new_dinic [new_dinic_n]
  (binding [new_dinic_adj nil new_dinic_edges nil new_dinic_i nil new_dinic_lvl nil new_dinic_ptr nil new_dinic_q nil] (try (do (set! new_dinic_lvl []) (set! new_dinic_ptr []) (set! new_dinic_q []) (set! new_dinic_adj []) (set! new_dinic_i 0) (while (< new_dinic_i new_dinic_n) (do (set! new_dinic_lvl (conj new_dinic_lvl 0)) (set! new_dinic_ptr (conj new_dinic_ptr 0)) (set! new_dinic_q (conj new_dinic_q 0)) (set! new_dinic_edges []) (set! new_dinic_adj (conj new_dinic_adj new_dinic_edges)) (set! new_dinic_i (+ new_dinic_i 1)))) (throw (ex-info "return" {:v {:adj new_dinic_adj :lvl new_dinic_lvl :n new_dinic_n :ptr new_dinic_ptr :q new_dinic_q}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn add_edge [add_edge_g_p add_edge_a add_edge_b add_edge_c add_edge_rcap]
  (binding [add_edge_adj nil add_edge_e1 nil add_edge_e2 nil add_edge_g nil add_edge_list_a nil add_edge_list_b nil] (do (set! add_edge_g add_edge_g_p) (set! add_edge_adj (:adj add_edge_g)) (set! add_edge_list_a (nth add_edge_adj add_edge_a)) (set! add_edge_list_b (nth add_edge_adj add_edge_b)) (set! add_edge_e1 [add_edge_b (count add_edge_list_b) add_edge_c 0]) (set! add_edge_e2 [add_edge_a (count add_edge_list_a) add_edge_rcap 0]) (set! add_edge_list_a (conj add_edge_list_a add_edge_e1)) (set! add_edge_list_b (conj add_edge_list_b add_edge_e2)) (set! add_edge_adj (assoc add_edge_adj add_edge_a add_edge_list_a)) (set! add_edge_adj (assoc add_edge_adj add_edge_b add_edge_list_b)) (set! add_edge_g (assoc add_edge_g :adj add_edge_adj)))))

(defn dfs [dfs_g_p dfs_v dfs_sink dfs_flow]
  (binding [dfs_adj_all nil dfs_adj_to nil dfs_adj_v nil dfs_avail nil dfs_back nil dfs_e nil dfs_g nil dfs_i nil dfs_ptr nil dfs_pushed nil dfs_to nil] (try (do (set! dfs_g dfs_g_p) (when (or (= dfs_v dfs_sink) (= dfs_flow 0)) (throw (ex-info "return" {:v dfs_flow}))) (set! dfs_ptr (:ptr dfs_g)) (set! dfs_i (nth dfs_ptr dfs_v)) (set! dfs_adj_all (:adj dfs_g)) (set! dfs_adj_v (nth dfs_adj_all dfs_v)) (while (< dfs_i (count dfs_adj_v)) (do (set! dfs_e (nth dfs_adj_v dfs_i)) (set! dfs_to (nth dfs_e 0)) (when (= (get (:lvl dfs_g) dfs_to) (+ (get (:lvl dfs_g) dfs_v) 1)) (do (set! dfs_avail (- (nth dfs_e 2) (nth dfs_e 3))) (set! dfs_pushed (dfs dfs_g dfs_to dfs_sink (min2 dfs_flow dfs_avail))) (when (> dfs_pushed 0) (do (set! dfs_e (assoc dfs_e 3 (+ (nth dfs_e 3) dfs_pushed))) (set! dfs_adj_v (assoc dfs_adj_v dfs_i dfs_e)) (set! dfs_adj_to (nth dfs_adj_all dfs_to)) (set! dfs_back (nth dfs_adj_to (nth dfs_e 1))) (set! dfs_back (assoc dfs_back 3 (- (nth dfs_back 3) dfs_pushed))) (set! dfs_adj_to (assoc dfs_adj_to (nth dfs_e 1) dfs_back)) (set! dfs_adj_all (assoc dfs_adj_all dfs_to dfs_adj_to)) (set! dfs_adj_all (assoc dfs_adj_all dfs_v dfs_adj_v)) (set! dfs_g (assoc dfs_g :adj dfs_adj_all)) (throw (ex-info "return" {:v dfs_pushed})))))) (set! dfs_i (+ dfs_i 1)) (set! dfs_ptr (assoc dfs_ptr dfs_v dfs_i)))) (set! dfs_g (assoc dfs_g :ptr dfs_ptr)) (set! dfs_adj_all (assoc dfs_adj_all dfs_v dfs_adj_v)) (set! dfs_g (assoc dfs_g :adj dfs_adj_all)) (throw (ex-info "return" {:v 0}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn max_flow [max_flow_g_p max_flow_source max_flow_sink]
  (binding [max_flow_e nil max_flow_edges nil max_flow_flow nil max_flow_g nil max_flow_i nil max_flow_j nil max_flow_l nil max_flow_lvl nil max_flow_lvl_inner nil max_flow_p nil max_flow_ptr nil max_flow_q nil max_flow_qe nil max_flow_qi nil max_flow_residual nil max_flow_threshold nil max_flow_to nil max_flow_v nil] (try (do (set! max_flow_g max_flow_g_p) (set! max_flow_flow 0) (set! max_flow_l 0) (while (< max_flow_l 31) (do (set! max_flow_threshold (pow2 (- 30 max_flow_l))) (loop [while_flag_1 true] (when (and while_flag_1 true) (do (set! max_flow_lvl []) (set! max_flow_ptr []) (set! max_flow_i 0) (while (< max_flow_i (:n max_flow_g)) (do (set! max_flow_lvl (conj max_flow_lvl 0)) (set! max_flow_ptr (conj max_flow_ptr 0)) (set! max_flow_i (+ max_flow_i 1)))) (set! max_flow_g (assoc max_flow_g :lvl max_flow_lvl)) (set! max_flow_g (assoc max_flow_g :ptr max_flow_ptr)) (set! max_flow_qi 0) (set! max_flow_qe 1) (set! max_flow_lvl (assoc max_flow_lvl max_flow_source 1)) (set! max_flow_g (assoc max_flow_g :lvl max_flow_lvl)) (set! max_flow_q (:q max_flow_g)) (set! max_flow_q (assoc max_flow_q 0 max_flow_source)) (while (and (< max_flow_qi max_flow_qe) (= (get (:lvl max_flow_g) max_flow_sink) 0)) (do (set! max_flow_v (nth max_flow_q max_flow_qi)) (set! max_flow_qi (+ max_flow_qi 1)) (set! max_flow_edges (get (:adj max_flow_g) max_flow_v)) (set! max_flow_j 0) (while (< max_flow_j (count max_flow_edges)) (do (set! max_flow_e (nth max_flow_edges max_flow_j)) (set! max_flow_to (nth max_flow_e 0)) (set! max_flow_residual (- (nth max_flow_e 2) (nth max_flow_e 3))) (set! max_flow_lvl_inner (:lvl max_flow_g)) (when (and (= (nth max_flow_lvl_inner max_flow_to) 0) (>= max_flow_residual max_flow_threshold)) (do (set! max_flow_q (assoc max_flow_q max_flow_qe max_flow_to)) (set! max_flow_qe (+ max_flow_qe 1)) (set! max_flow_lvl_inner (assoc max_flow_lvl_inner max_flow_to (+ (nth max_flow_lvl_inner max_flow_v) 1))) (set! max_flow_g (assoc max_flow_g :lvl max_flow_lvl_inner)))) (set! max_flow_j (+ max_flow_j 1)))))) (set! max_flow_p (dfs max_flow_g max_flow_source max_flow_sink main_INF)) (while (> max_flow_p 0) (do (set! max_flow_flow (+ max_flow_flow max_flow_p)) (set! max_flow_p (dfs max_flow_g max_flow_source max_flow_sink main_INF)))) (cond (= (get (:lvl max_flow_g) max_flow_sink) 0) (recur false) :else (recur while_flag_1))))) (set! max_flow_l (+ max_flow_l 1)))) (throw (ex-info "return" {:v max_flow_flow}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_graph (new_dinic 10))

(def ^:dynamic main_source 0)

(def ^:dynamic main_sink 9)

(def ^:dynamic main_v 1)

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (while (< main_v 5) (do (add_edge main_graph main_source main_v 1 0) (def main_v (+ main_v 1))))
      (def main_v 5)
      (while (< main_v 9) (do (add_edge main_graph main_v main_sink 1 0) (def main_v (+ main_v 1))))
      (def main_v 1)
      (while (< main_v 5) (do (add_edge main_graph main_v (+ main_v 4) 1 0) (def main_v (+ main_v 1))))
      (println (str (max_flow main_graph main_source main_sink)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
