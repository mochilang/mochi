(ns main (:refer-clojure :exclude [dfs path_sum sample_tree_one sample_tree_two main]))

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

(declare dfs path_sum sample_tree_one sample_tree_two main)

(def ^:dynamic main_tree1 nil)

(def ^:dynamic main_tree2 nil)

(defn dfs [dfs_node dfs_target dfs_current]
  (try (throw (ex-info "return" {:v (cond (= dfs_node dfs_Empty) 0 (and (map? dfs_node) (= (:__tag dfs_node) "Node") (contains? dfs_node :left) (contains? dfs_node :value) (contains? dfs_node :right)) (let [dfs_l (:left dfs_node) dfs_v (:value dfs_node) dfs_r (:right dfs_node)] (+ (+ (if (= (+ dfs_current dfs_v) dfs_target) 1 0) (dfs dfs_l dfs_target (+ dfs_current dfs_v))) (dfs dfs_r dfs_target (+ dfs_current dfs_v)))))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn path_sum [path_sum_node path_sum_target]
  (try (throw (ex-info "return" {:v (cond (= path_sum_node path_sum_Empty) 0 (and (map? path_sum_node) (= (:__tag path_sum_node) "Node") (contains? path_sum_node :left) (contains? path_sum_node :value) (contains? path_sum_node :right)) (let [path_sum_l (:left path_sum_node) path_sum_v (:value path_sum_node) path_sum_r (:right path_sum_node)] (+ (+ (dfs path_sum_node path_sum_target 0) (path_sum path_sum_l path_sum_target)) (path_sum path_sum_r path_sum_target))))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn sample_tree_one []
  (try (throw (ex-info "return" {:v {:__tag "Node" :left 10 :value {:__tag "Node" :left 5 :value {:__tag "Node" :left 3 :value {:__tag "Node" :left 3 :value {:__tag "Empty"} :right {:__tag "Empty"}} :right {:__tag "Node" :left (- 2) :value {:__tag "Empty"} :right {:__tag "Empty"}}} :right {:__tag "Node" :left 2 :value {:__tag "Empty"} :right {:__tag "Node" :left 1 :value {:__tag "Empty"} :right {:__tag "Empty"}}}} :right {:__tag "Node" :left (- 3) :value {:__tag "Empty"} :right {:__tag "Node" :left 11 :value {:__tag "Empty"} :right {:__tag "Empty"}}}}})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn sample_tree_two []
  (try (throw (ex-info "return" {:v {:__tag "Node" :left 10 :value {:__tag "Node" :left 5 :value {:__tag "Node" :left 3 :value {:__tag "Node" :left 3 :value {:__tag "Empty"} :right {:__tag "Empty"}} :right {:__tag "Node" :left (- 2) :value {:__tag "Empty"} :right {:__tag "Empty"}}} :right {:__tag "Node" :left 2 :value {:__tag "Empty"} :right {:__tag "Node" :left 1 :value {:__tag "Empty"} :right {:__tag "Empty"}}}} :right {:__tag "Node" :left (- 3) :value {:__tag "Empty"} :right {:__tag "Node" :left 10 :value {:__tag "Empty"} :right {:__tag "Empty"}}}}})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (binding [main_tree1 nil main_tree2 nil] (do (set! main_tree1 (sample_tree_one)) (println (path_sum main_tree1 8)) (println (path_sum main_tree1 7)) (set! main_tree2 (sample_tree_two)) (println (path_sum main_tree2 8)))))

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
