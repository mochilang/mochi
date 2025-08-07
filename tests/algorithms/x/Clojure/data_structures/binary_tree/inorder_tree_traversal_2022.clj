(ns main (:refer-clojure :exclude [new_node insert inorder make_tree main]))

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

(declare new_node insert inorder make_tree main)

(def ^:dynamic inorder_i nil)

(def ^:dynamic inorder_node nil)

(def ^:dynamic inorder_result nil)

(def ^:dynamic inorder_right_part nil)

(def ^:dynamic insert_current nil)

(def ^:dynamic insert_node nil)

(def ^:dynamic insert_nodes nil)

(def ^:dynamic insert_state nil)

(def ^:dynamic main_state nil)

(def ^:dynamic make_tree_state nil)

(def ^:dynamic new_node_state nil)

(defn new_node [new_node_state_p new_node_value]
  (binding [new_node_state nil] (try (do (set! new_node_state new_node_state_p) (set! new_node_state (assoc new_node_state :nodes (conj (:nodes new_node_state) {:data new_node_value :left (- 1) :right (- 1)}))) (throw (ex-info "return" {:v (- (count (:nodes new_node_state)) 1)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn insert [insert_state_p insert_value]
  (binding [insert_current nil insert_node nil insert_nodes nil insert_state nil] (try (do (set! insert_state insert_state_p) (when (= (:root insert_state) (- 1)) (do (set! insert_state (assoc insert_state :root (new_node insert_state insert_value))) (throw (ex-info "return" {:v nil})))) (set! insert_current (:root insert_state)) (set! insert_nodes (:nodes insert_state)) (while true (do (set! insert_node (nth insert_nodes insert_current)) (if (< insert_value (:data insert_node)) (do (when (= (:left insert_node) (- 1)) (do (set! insert_node (assoc insert_node :left (new_node insert_state insert_value))) (set! insert_nodes (assoc insert_nodes insert_current insert_node)) (set! insert_state (assoc insert_state :nodes insert_nodes)) (throw (ex-info "return" {:v nil})))) (set! insert_current (:left insert_node))) (do (when (= (:right insert_node) (- 1)) (do (set! insert_node (assoc insert_node :right (new_node insert_state insert_value))) (set! insert_nodes (assoc insert_nodes insert_current insert_node)) (set! insert_state (assoc insert_state :nodes insert_nodes)) (throw (ex-info "return" {:v nil})))) (set! insert_current (:right insert_node))))))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn inorder [inorder_state inorder_idx]
  (binding [inorder_i nil inorder_node nil inorder_result nil inorder_right_part nil] (try (do (when (= inorder_idx (- 1)) (throw (ex-info "return" {:v []}))) (set! inorder_node (get (:nodes inorder_state) inorder_idx)) (set! inorder_result (inorder inorder_state (:left inorder_node))) (set! inorder_result (conj inorder_result (:data inorder_node))) (set! inorder_right_part (inorder inorder_state (:right inorder_node))) (set! inorder_i 0) (while (< inorder_i (count inorder_right_part)) (do (set! inorder_result (conj inorder_result (nth inorder_right_part inorder_i))) (set! inorder_i (+ inorder_i 1)))) (throw (ex-info "return" {:v inorder_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn make_tree []
  (binding [make_tree_state nil] (try (do (set! make_tree_state {:nodes [] :root (- 1)}) (insert make_tree_state 15) (insert make_tree_state 10) (insert make_tree_state 25) (insert make_tree_state 6) (insert make_tree_state 14) (insert make_tree_state 20) (insert make_tree_state 60) (throw (ex-info "return" {:v make_tree_state}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_state nil] (do (set! main_state (make_tree)) (println "Printing values of binary search tree in Inorder Traversal.") (println (inorder main_state (:root main_state))))))

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
