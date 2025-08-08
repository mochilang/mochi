(ns main (:refer-clojure :exclude [sort_nodes print_node print_binary_search_tree find_optimal_binary_search_tree main]))

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

(declare sort_nodes print_node print_binary_search_tree find_optimal_binary_search_tree main)

(def ^:dynamic find_optimal_binary_search_tree_INF nil)

(def ^:dynamic find_optimal_binary_search_tree_cost nil)

(def ^:dynamic find_optimal_binary_search_tree_dp nil)

(def ^:dynamic find_optimal_binary_search_tree_dp_row nil)

(def ^:dynamic find_optimal_binary_search_tree_freqs nil)

(def ^:dynamic find_optimal_binary_search_tree_i nil)

(def ^:dynamic find_optimal_binary_search_tree_interval_length nil)

(def ^:dynamic find_optimal_binary_search_tree_j nil)

(def ^:dynamic find_optimal_binary_search_tree_keys nil)

(def ^:dynamic find_optimal_binary_search_tree_left nil)

(def ^:dynamic find_optimal_binary_search_tree_n nil)

(def ^:dynamic find_optimal_binary_search_tree_node nil)

(def ^:dynamic find_optimal_binary_search_tree_nodes nil)

(def ^:dynamic find_optimal_binary_search_tree_r nil)

(def ^:dynamic find_optimal_binary_search_tree_right nil)

(def ^:dynamic find_optimal_binary_search_tree_root nil)

(def ^:dynamic find_optimal_binary_search_tree_root_row nil)

(def ^:dynamic find_optimal_binary_search_tree_total nil)

(def ^:dynamic find_optimal_binary_search_tree_total_row nil)

(def ^:dynamic main_nodes nil)

(def ^:dynamic print_binary_search_tree_node nil)

(def ^:dynamic sort_nodes_arr nil)

(def ^:dynamic sort_nodes_i nil)

(def ^:dynamic sort_nodes_j nil)

(def ^:dynamic sort_nodes_key_node nil)

(def ^:dynamic sort_nodes_temp nil)

(defn sort_nodes [sort_nodes_nodes]
  (binding [sort_nodes_arr nil sort_nodes_i nil sort_nodes_j nil sort_nodes_key_node nil sort_nodes_temp nil] (try (do (set! sort_nodes_arr sort_nodes_nodes) (set! sort_nodes_i 1) (while (< sort_nodes_i (count sort_nodes_arr)) (do (set! sort_nodes_key_node (nth sort_nodes_arr sort_nodes_i)) (set! sort_nodes_j (- sort_nodes_i 1)) (loop [while_flag_1 true] (when (and while_flag_1 (>= sort_nodes_j 0)) (do (set! sort_nodes_temp (nth sort_nodes_arr sort_nodes_j)) (if (> (:key sort_nodes_temp) (:key sort_nodes_key_node)) (do (set! sort_nodes_arr (assoc sort_nodes_arr (+ sort_nodes_j 1) sort_nodes_temp)) (set! sort_nodes_j (- sort_nodes_j 1)) (recur while_flag_1)) (recur false))))) (set! sort_nodes_arr (assoc sort_nodes_arr (+ sort_nodes_j 1) sort_nodes_key_node)) (set! sort_nodes_i (+ sort_nodes_i 1)))) (throw (ex-info "return" {:v sort_nodes_arr}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn print_node [print_node_n]
  (println (str (str (str (str "Node(key=" (str (:key print_node_n))) ", freq=") (str (:freq print_node_n))) ")")))

(defn print_binary_search_tree [print_binary_search_tree_root print_binary_search_tree_keys print_binary_search_tree_i print_binary_search_tree_j print_binary_search_tree_parent print_binary_search_tree_is_left]
  (binding [print_binary_search_tree_node nil] (try (do (when (or (or (> print_binary_search_tree_i print_binary_search_tree_j) (< print_binary_search_tree_i 0)) (> print_binary_search_tree_j (- (count print_binary_search_tree_root) 1))) (throw (ex-info "return" {:v nil}))) (set! print_binary_search_tree_node (nth (nth print_binary_search_tree_root print_binary_search_tree_i) print_binary_search_tree_j)) (if (= print_binary_search_tree_parent (- 1)) (println (str (str (nth print_binary_search_tree_keys print_binary_search_tree_node)) " is the root of the binary search tree.")) (if print_binary_search_tree_is_left (println (str (str (str (str (nth print_binary_search_tree_keys print_binary_search_tree_node)) " is the left child of key ") (str print_binary_search_tree_parent)) ".")) (println (str (str (str (str (nth print_binary_search_tree_keys print_binary_search_tree_node)) " is the right child of key ") (str print_binary_search_tree_parent)) ".")))) (print_binary_search_tree print_binary_search_tree_root print_binary_search_tree_keys print_binary_search_tree_i (- print_binary_search_tree_node 1) (nth print_binary_search_tree_keys print_binary_search_tree_node) true) (print_binary_search_tree print_binary_search_tree_root print_binary_search_tree_keys (+ print_binary_search_tree_node 1) print_binary_search_tree_j (nth print_binary_search_tree_keys print_binary_search_tree_node) false)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn find_optimal_binary_search_tree [find_optimal_binary_search_tree_original_nodes]
  (binding [find_optimal_binary_search_tree_INF nil find_optimal_binary_search_tree_cost nil find_optimal_binary_search_tree_dp nil find_optimal_binary_search_tree_dp_row nil find_optimal_binary_search_tree_freqs nil find_optimal_binary_search_tree_i nil find_optimal_binary_search_tree_interval_length nil find_optimal_binary_search_tree_j nil find_optimal_binary_search_tree_keys nil find_optimal_binary_search_tree_left nil find_optimal_binary_search_tree_n nil find_optimal_binary_search_tree_node nil find_optimal_binary_search_tree_nodes nil find_optimal_binary_search_tree_r nil find_optimal_binary_search_tree_right nil find_optimal_binary_search_tree_root nil find_optimal_binary_search_tree_root_row nil find_optimal_binary_search_tree_total nil find_optimal_binary_search_tree_total_row nil] (do (set! find_optimal_binary_search_tree_nodes (sort_nodes find_optimal_binary_search_tree_original_nodes)) (set! find_optimal_binary_search_tree_n (count find_optimal_binary_search_tree_nodes)) (set! find_optimal_binary_search_tree_keys []) (set! find_optimal_binary_search_tree_freqs []) (set! find_optimal_binary_search_tree_i 0) (while (< find_optimal_binary_search_tree_i find_optimal_binary_search_tree_n) (do (set! find_optimal_binary_search_tree_node (nth find_optimal_binary_search_tree_nodes find_optimal_binary_search_tree_i)) (set! find_optimal_binary_search_tree_keys (conj find_optimal_binary_search_tree_keys (:key find_optimal_binary_search_tree_node))) (set! find_optimal_binary_search_tree_freqs (conj find_optimal_binary_search_tree_freqs (:freq find_optimal_binary_search_tree_node))) (set! find_optimal_binary_search_tree_i (+ find_optimal_binary_search_tree_i 1)))) (set! find_optimal_binary_search_tree_dp []) (set! find_optimal_binary_search_tree_total []) (set! find_optimal_binary_search_tree_root []) (set! find_optimal_binary_search_tree_i 0) (while (< find_optimal_binary_search_tree_i find_optimal_binary_search_tree_n) (do (set! find_optimal_binary_search_tree_dp_row []) (set! find_optimal_binary_search_tree_total_row []) (set! find_optimal_binary_search_tree_root_row []) (set! find_optimal_binary_search_tree_j 0) (while (< find_optimal_binary_search_tree_j find_optimal_binary_search_tree_n) (do (if (= find_optimal_binary_search_tree_i find_optimal_binary_search_tree_j) (do (set! find_optimal_binary_search_tree_dp_row (conj find_optimal_binary_search_tree_dp_row (nth find_optimal_binary_search_tree_freqs find_optimal_binary_search_tree_i))) (set! find_optimal_binary_search_tree_total_row (conj find_optimal_binary_search_tree_total_row (nth find_optimal_binary_search_tree_freqs find_optimal_binary_search_tree_i))) (set! find_optimal_binary_search_tree_root_row (conj find_optimal_binary_search_tree_root_row find_optimal_binary_search_tree_i))) (do (set! find_optimal_binary_search_tree_dp_row (conj find_optimal_binary_search_tree_dp_row 0)) (set! find_optimal_binary_search_tree_total_row (conj find_optimal_binary_search_tree_total_row 0)) (set! find_optimal_binary_search_tree_root_row (conj find_optimal_binary_search_tree_root_row 0)))) (set! find_optimal_binary_search_tree_j (+ find_optimal_binary_search_tree_j 1)))) (set! find_optimal_binary_search_tree_dp (conj find_optimal_binary_search_tree_dp find_optimal_binary_search_tree_dp_row)) (set! find_optimal_binary_search_tree_total (conj find_optimal_binary_search_tree_total find_optimal_binary_search_tree_total_row)) (set! find_optimal_binary_search_tree_root (conj find_optimal_binary_search_tree_root find_optimal_binary_search_tree_root_row)) (set! find_optimal_binary_search_tree_i (+ find_optimal_binary_search_tree_i 1)))) (set! find_optimal_binary_search_tree_interval_length 2) (set! find_optimal_binary_search_tree_INF 2147483647) (while (<= find_optimal_binary_search_tree_interval_length find_optimal_binary_search_tree_n) (do (set! find_optimal_binary_search_tree_i 0) (while (< find_optimal_binary_search_tree_i (+ (- find_optimal_binary_search_tree_n find_optimal_binary_search_tree_interval_length) 1)) (do (set! find_optimal_binary_search_tree_j (- (+ find_optimal_binary_search_tree_i find_optimal_binary_search_tree_interval_length) 1)) (set! find_optimal_binary_search_tree_dp (assoc-in find_optimal_binary_search_tree_dp [find_optimal_binary_search_tree_i find_optimal_binary_search_tree_j] find_optimal_binary_search_tree_INF)) (set! find_optimal_binary_search_tree_total (assoc-in find_optimal_binary_search_tree_total [find_optimal_binary_search_tree_i find_optimal_binary_search_tree_j] (+ (nth (nth find_optimal_binary_search_tree_total find_optimal_binary_search_tree_i) (- find_optimal_binary_search_tree_j 1)) (nth find_optimal_binary_search_tree_freqs find_optimal_binary_search_tree_j)))) (set! find_optimal_binary_search_tree_r (nth (nth find_optimal_binary_search_tree_root find_optimal_binary_search_tree_i) (- find_optimal_binary_search_tree_j 1))) (while (<= find_optimal_binary_search_tree_r (nth (nth find_optimal_binary_search_tree_root (+ find_optimal_binary_search_tree_i 1)) find_optimal_binary_search_tree_j)) (do (set! find_optimal_binary_search_tree_left (if (not= find_optimal_binary_search_tree_r find_optimal_binary_search_tree_i) (nth (nth find_optimal_binary_search_tree_dp find_optimal_binary_search_tree_i) (- find_optimal_binary_search_tree_r 1)) 0)) (set! find_optimal_binary_search_tree_right (if (not= find_optimal_binary_search_tree_r find_optimal_binary_search_tree_j) (nth (nth find_optimal_binary_search_tree_dp (+ find_optimal_binary_search_tree_r 1)) find_optimal_binary_search_tree_j) 0)) (set! find_optimal_binary_search_tree_cost (+ (+ find_optimal_binary_search_tree_left (nth (nth find_optimal_binary_search_tree_total find_optimal_binary_search_tree_i) find_optimal_binary_search_tree_j)) find_optimal_binary_search_tree_right)) (when (> (nth (nth find_optimal_binary_search_tree_dp find_optimal_binary_search_tree_i) find_optimal_binary_search_tree_j) find_optimal_binary_search_tree_cost) (do (set! find_optimal_binary_search_tree_dp (assoc-in find_optimal_binary_search_tree_dp [find_optimal_binary_search_tree_i find_optimal_binary_search_tree_j] find_optimal_binary_search_tree_cost)) (set! find_optimal_binary_search_tree_root (assoc-in find_optimal_binary_search_tree_root [find_optimal_binary_search_tree_i find_optimal_binary_search_tree_j] find_optimal_binary_search_tree_r)))) (set! find_optimal_binary_search_tree_r (+ find_optimal_binary_search_tree_r 1)))) (set! find_optimal_binary_search_tree_i (+ find_optimal_binary_search_tree_i 1)))) (set! find_optimal_binary_search_tree_interval_length (+ find_optimal_binary_search_tree_interval_length 1)))) (println "Binary search tree nodes:") (set! find_optimal_binary_search_tree_i 0) (while (< find_optimal_binary_search_tree_i find_optimal_binary_search_tree_n) (do (print_node (nth find_optimal_binary_search_tree_nodes find_optimal_binary_search_tree_i)) (set! find_optimal_binary_search_tree_i (+ find_optimal_binary_search_tree_i 1)))) (println (str (str "\nThe cost of optimal BST for given tree nodes is " (str (nth (nth find_optimal_binary_search_tree_dp 0) (- find_optimal_binary_search_tree_n 1)))) ".")) (print_binary_search_tree find_optimal_binary_search_tree_root find_optimal_binary_search_tree_keys 0 (- find_optimal_binary_search_tree_n 1) (- 1) false))))

(defn main []
  (binding [main_nodes nil] (do (set! main_nodes [{:freq 8 :key 12} {:freq 34 :key 10} {:freq 50 :key 20} {:freq 3 :key 42} {:freq 40 :key 25} {:freq 30 :key 37}]) (find_optimal_binary_search_tree main_nodes))))

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
