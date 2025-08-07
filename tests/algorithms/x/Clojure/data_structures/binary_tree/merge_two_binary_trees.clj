(ns main (:refer-clojure :exclude [merge_two_binary_trees is_leaf get_left get_right get_value print_preorder]))

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

(declare merge_two_binary_trees is_leaf get_left get_right get_value print_preorder)

(def ^:dynamic print_preorder_l nil)

(def ^:dynamic print_preorder_r nil)

(def ^:dynamic print_preorder_v nil)

(defn merge_two_binary_trees [merge_two_binary_trees_t1 merge_two_binary_trees_t2]
  (try (throw (ex-info "return" {:v (cond (= merge_two_binary_trees_t1 merge_two_binary_trees_Leaf) merge_two_binary_trees_t2 (and (map? merge_two_binary_trees_t1) (= (:__tag merge_two_binary_trees_t1) "Node") (contains? merge_two_binary_trees_t1 :left) (contains? merge_two_binary_trees_t1 :value) (contains? merge_two_binary_trees_t1 :right)) (let [merge_two_binary_trees_l1 (:left merge_two_binary_trees_t1) merge_two_binary_trees_v1 (:value merge_two_binary_trees_t1) merge_two_binary_trees_r1 (:right merge_two_binary_trees_t1)] (cond (= merge_two_binary_trees_t2 merge_two_binary_trees_Leaf) merge_two_binary_trees_t1 (and (map? merge_two_binary_trees_t2) (= (:__tag merge_two_binary_trees_t2) "Node") (contains? merge_two_binary_trees_t2 :left) (contains? merge_two_binary_trees_t2 :value) (contains? merge_two_binary_trees_t2 :right)) (let [merge_two_binary_trees_l2 (:left merge_two_binary_trees_t2) merge_two_binary_trees_v2 (:value merge_two_binary_trees_t2) merge_two_binary_trees_r2 (:right merge_two_binary_trees_t2)] {:__tag "Node" :left (merge_two_binary_trees merge_two_binary_trees_l1 merge_two_binary_trees_l2) :value (+ merge_two_binary_trees_v1 merge_two_binary_trees_v2) :right (merge_two_binary_trees merge_two_binary_trees_r1 merge_two_binary_trees_r2)}))))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn is_leaf [is_leaf_t]
  (try (throw (ex-info "return" {:v (cond (= is_leaf_t is_leaf_Leaf) true (= is_leaf_t is_leaf__) false)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn get_left [get_left_t]
  (try (throw (ex-info "return" {:v (cond (and (map? get_left_t) (= (:__tag get_left_t) "Node") (contains? get_left_t :left) (contains? get_left_t :value) (contains? get_left_t :right)) (let [get_left_l (:left get_left_t) get_left__ (:value get_left_t) get_left__ (:right get_left_t)] get_left_l) (= get_left_t get_left__) {:__tag "Leaf"})})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn get_right [get_right_t]
  (try (throw (ex-info "return" {:v (cond (and (map? get_right_t) (= (:__tag get_right_t) "Node") (contains? get_right_t :left) (contains? get_right_t :value) (contains? get_right_t :right)) (let [get_right__ (:left get_right_t) get_right__ (:value get_right_t) get_right_r (:right get_right_t)] get_right_r) (= get_right_t get_right__) {:__tag "Leaf"})})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn get_value [get_value_t]
  (try (throw (ex-info "return" {:v (cond (and (map? get_value_t) (= (:__tag get_value_t) "Node") (contains? get_value_t :left) (contains? get_value_t :value) (contains? get_value_t :right)) (let [get_value__ (:left get_value_t) get_value_v (:value get_value_t) get_value__ (:right get_value_t)] get_value_v) (= get_value_t get_value__) 0)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn print_preorder [print_preorder_t]
  (binding [print_preorder_l nil print_preorder_r nil print_preorder_v nil] (when (not (is_leaf print_preorder_t)) (do (set! print_preorder_v (get_value print_preorder_t)) (set! print_preorder_l (get_left print_preorder_t)) (set! print_preorder_r (get_right print_preorder_t)) (println print_preorder_v) (print_preorder print_preorder_l) (print_preorder print_preorder_r)))))

(def ^:dynamic main_tree1 {:__tag "Node" :left {:__tag "Node" :left {:__tag "Node" :left {:__tag "Leaf"} :value 4 :right {:__tag "Leaf"}} :value 2 :right {:__tag "Leaf"}} :value 1 :right {:__tag "Node" :left {:__tag "Leaf"} :value 3 :right {:__tag "Leaf"}}})

(def ^:dynamic main_tree2 {:__tag "Node" :left {:__tag "Node" :left {:__tag "Leaf"} :value 4 :right {:__tag "Node" :left {:__tag "Leaf"} :value 9 :right {:__tag "Leaf"}}} :value 2 :right {:__tag "Node" :left {:__tag "Leaf"} :value 6 :right {:__tag "Node" :left {:__tag "Leaf"} :value 5 :right {:__tag "Leaf"}}}})

(def ^:dynamic main_merged_tree (merge_two_binary_trees main_tree1 main_tree2))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println "Tree1 is:")
      (print_preorder main_tree1)
      (println "Tree2 is:")
      (print_preorder main_tree2)
      (println "Merged Tree is:")
      (print_preorder main_merged_tree)
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
