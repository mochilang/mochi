(ns main (:refer-clojure :exclude [rand_int contains remove_all partition_graph cut_to_string]))

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

(declare rand_int contains remove_all partition_graph cut_to_string)

(def ^:dynamic contains_i nil)

(def ^:dynamic cut_to_string_i nil)

(def ^:dynamic cut_to_string_p nil)

(def ^:dynamic cut_to_string_s nil)

(def ^:dynamic partition_graph_contracted nil)

(def ^:dynamic partition_graph_cut nil)

(def ^:dynamic partition_graph_graph_copy nil)

(def ^:dynamic partition_graph_group nil)

(def ^:dynamic partition_graph_groupA nil)

(def ^:dynamic partition_graph_groupB nil)

(def ^:dynamic partition_graph_groups nil)

(def ^:dynamic partition_graph_i nil)

(def ^:dynamic partition_graph_j nil)

(def ^:dynamic partition_graph_k nil)

(def ^:dynamic partition_graph_l nil)

(def ^:dynamic partition_graph_lst nil)

(def ^:dynamic partition_graph_n nil)

(def ^:dynamic partition_graph_nb nil)

(def ^:dynamic partition_graph_neigh nil)

(def ^:dynamic partition_graph_node nil)

(def ^:dynamic partition_graph_nodes nil)

(def ^:dynamic partition_graph_u nil)

(def ^:dynamic partition_graph_u_neighbors nil)

(def ^:dynamic partition_graph_uv nil)

(def ^:dynamic partition_graph_uv_neighbors nil)

(def ^:dynamic partition_graph_v nil)

(def ^:dynamic partition_graph_val nil)

(def ^:dynamic remove_all_i nil)

(def ^:dynamic remove_all_res nil)

(def ^:dynamic main_seed 1)

(defn rand_int [rand_int_n]
  (try (do (alter-var-root (var main_seed) (fn [_] (mod (+ (* main_seed 1103515245) 12345) 2147483648))) (throw (ex-info "return" {:v (mod main_seed rand_int_n)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn contains [contains_list contains_value]
  (binding [contains_i nil] (try (do (set! contains_i 0) (while (< contains_i (count contains_list)) (do (when (= (nth contains_list contains_i) contains_value) (throw (ex-info "return" {:v true}))) (set! contains_i (+ contains_i 1)))) (throw (ex-info "return" {:v false}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn remove_all [remove_all_list remove_all_value]
  (binding [remove_all_i nil remove_all_res nil] (try (do (set! remove_all_res []) (set! remove_all_i 0) (while (< remove_all_i (count remove_all_list)) (do (when (not= (nth remove_all_list remove_all_i) remove_all_value) (set! remove_all_res (conj remove_all_res (nth remove_all_list remove_all_i)))) (set! remove_all_i (+ remove_all_i 1)))) (throw (ex-info "return" {:v remove_all_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn partition_graph [partition_graph_graph]
  (binding [partition_graph_contracted nil partition_graph_cut nil partition_graph_graph_copy nil partition_graph_group nil partition_graph_groupA nil partition_graph_groupB nil partition_graph_groups nil partition_graph_i nil partition_graph_j nil partition_graph_k nil partition_graph_l nil partition_graph_lst nil partition_graph_n nil partition_graph_nb nil partition_graph_neigh nil partition_graph_node nil partition_graph_nodes nil partition_graph_u nil partition_graph_u_neighbors nil partition_graph_uv nil partition_graph_uv_neighbors nil partition_graph_v nil partition_graph_val nil] (try (do (set! partition_graph_contracted {}) (doseq [node (keys partition_graph_graph)] (set! partition_graph_contracted (assoc partition_graph_contracted node [node]))) (set! partition_graph_graph_copy {}) (doseq [node (keys partition_graph_graph)] (do (set! partition_graph_lst []) (set! partition_graph_neigh (get partition_graph_graph node)) (set! partition_graph_i 0) (while (< partition_graph_i (count partition_graph_neigh)) (do (set! partition_graph_lst (conj partition_graph_lst (nth partition_graph_neigh partition_graph_i))) (set! partition_graph_i (+ partition_graph_i 1)))) (set! partition_graph_graph_copy (assoc partition_graph_graph_copy node partition_graph_lst)))) (set! partition_graph_nodes (keys partition_graph_graph_copy)) (while (> (count partition_graph_nodes) 2) (do (set! partition_graph_u (nth partition_graph_nodes (rand_int (count partition_graph_nodes)))) (set! partition_graph_u_neighbors (get partition_graph_graph_copy partition_graph_u)) (set! partition_graph_v (nth partition_graph_u_neighbors (rand_int (count partition_graph_u_neighbors)))) (set! partition_graph_uv (+ partition_graph_u partition_graph_v)) (set! partition_graph_uv_neighbors []) (set! partition_graph_i 0) (while (< partition_graph_i (count (get partition_graph_graph_copy partition_graph_u))) (do (set! partition_graph_n (nth (get partition_graph_graph_copy partition_graph_u) partition_graph_i)) (when (and (and (not= partition_graph_n partition_graph_u) (not= partition_graph_n partition_graph_v)) (= (contains partition_graph_uv_neighbors partition_graph_n) false)) (set! partition_graph_uv_neighbors (conj partition_graph_uv_neighbors partition_graph_n))) (set! partition_graph_i (+ partition_graph_i 1)))) (set! partition_graph_i 0) (while (< partition_graph_i (count (get partition_graph_graph_copy partition_graph_v))) (do (set! partition_graph_n (nth (get partition_graph_graph_copy partition_graph_v) partition_graph_i)) (when (and (and (not= partition_graph_n partition_graph_u) (not= partition_graph_n partition_graph_v)) (= (contains partition_graph_uv_neighbors partition_graph_n) false)) (set! partition_graph_uv_neighbors (conj partition_graph_uv_neighbors partition_graph_n))) (set! partition_graph_i (+ partition_graph_i 1)))) (set! partition_graph_graph_copy (assoc partition_graph_graph_copy partition_graph_uv partition_graph_uv_neighbors)) (set! partition_graph_k 0) (while (< partition_graph_k (count partition_graph_uv_neighbors)) (do (set! partition_graph_nb (nth partition_graph_uv_neighbors partition_graph_k)) (set! partition_graph_graph_copy (assoc partition_graph_graph_copy partition_graph_nb (conj (get partition_graph_graph_copy partition_graph_nb) partition_graph_uv))) (set! partition_graph_graph_copy (assoc partition_graph_graph_copy partition_graph_nb (remove_all (get partition_graph_graph_copy partition_graph_nb) partition_graph_u))) (set! partition_graph_graph_copy (assoc partition_graph_graph_copy partition_graph_nb (remove_all (get partition_graph_graph_copy partition_graph_nb) partition_graph_v))) (set! partition_graph_k (+ partition_graph_k 1)))) (set! partition_graph_group []) (set! partition_graph_i 0) (while (< partition_graph_i (count (get partition_graph_contracted partition_graph_u))) (do (set! partition_graph_group (conj partition_graph_group (nth (get partition_graph_contracted partition_graph_u) partition_graph_i))) (set! partition_graph_i (+ partition_graph_i 1)))) (set! partition_graph_i 0) (while (< partition_graph_i (count (get partition_graph_contracted partition_graph_v))) (do (set! partition_graph_val (nth (get partition_graph_contracted partition_graph_v) partition_graph_i)) (when (= (contains partition_graph_group partition_graph_val) false) (set! partition_graph_group (conj partition_graph_group partition_graph_val))) (set! partition_graph_i (+ partition_graph_i 1)))) (set! partition_graph_contracted (assoc partition_graph_contracted partition_graph_uv partition_graph_group)) (set! partition_graph_nodes (remove_all partition_graph_nodes partition_graph_u)) (set! partition_graph_nodes (remove_all partition_graph_nodes partition_graph_v)) (set! partition_graph_nodes (conj partition_graph_nodes partition_graph_uv)))) (set! partition_graph_groups []) (set! partition_graph_j 0) (while (< partition_graph_j (count partition_graph_nodes)) (do (set! partition_graph_n (nth partition_graph_nodes partition_graph_j)) (set! partition_graph_groups (conj partition_graph_groups (get partition_graph_contracted partition_graph_n))) (set! partition_graph_j (+ partition_graph_j 1)))) (set! partition_graph_groupA (nth partition_graph_groups 0)) (set! partition_graph_groupB (nth partition_graph_groups 1)) (set! partition_graph_cut []) (set! partition_graph_j 0) (while (< partition_graph_j (count partition_graph_groupA)) (do (set! partition_graph_node (nth partition_graph_groupA partition_graph_j)) (set! partition_graph_neigh (get partition_graph_graph partition_graph_node)) (set! partition_graph_l 0) (while (< partition_graph_l (count partition_graph_neigh)) (do (set! partition_graph_nb (nth partition_graph_neigh partition_graph_l)) (when (contains partition_graph_groupB partition_graph_nb) (set! partition_graph_cut (conj partition_graph_cut {:a partition_graph_node :b partition_graph_nb}))) (set! partition_graph_l (+ partition_graph_l 1)))) (set! partition_graph_j (+ partition_graph_j 1)))) (throw (ex-info "return" {:v partition_graph_cut}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn cut_to_string [cut_to_string_cut]
  (binding [cut_to_string_i nil cut_to_string_p nil cut_to_string_s nil] (try (do (set! cut_to_string_s "{") (set! cut_to_string_i 0) (while (< cut_to_string_i (count cut_to_string_cut)) (do (set! cut_to_string_p (nth cut_to_string_cut cut_to_string_i)) (set! cut_to_string_s (str (str (str (str (str cut_to_string_s "(") (:a cut_to_string_p)) ", ") (:b cut_to_string_p)) ")")) (when (< cut_to_string_i (- (count cut_to_string_cut) 1)) (set! cut_to_string_s (str cut_to_string_s ", "))) (set! cut_to_string_i (+ cut_to_string_i 1)))) (set! cut_to_string_s (str cut_to_string_s "}")) (throw (ex-info "return" {:v cut_to_string_s}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_TEST_GRAPH {"1" ["2" "3" "4" "5"] "10" ["6" "7" "8" "9" "3"] "2" ["1" "3" "4" "5"] "3" ["1" "2" "4" "5" "10"] "4" ["1" "2" "3" "5" "6"] "5" ["1" "2" "3" "4" "7"] "6" ["7" "8" "9" "10" "4"] "7" ["6" "8" "9" "10" "5"] "8" ["6" "7" "9" "10"] "9" ["6" "7" "8" "10"]})

(def ^:dynamic main_result (partition_graph main_TEST_GRAPH))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (cut_to_string main_result))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
