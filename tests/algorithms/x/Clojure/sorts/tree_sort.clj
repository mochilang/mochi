(ns main (:refer-clojure :exclude [new_node insert inorder tree_sort]))

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

(declare new_node insert inorder tree_sort)

(def ^:dynamic inorder_i nil)

(def ^:dynamic inorder_node nil)

(def ^:dynamic inorder_result nil)

(def ^:dynamic inorder_right_part nil)

(def ^:dynamic insert_current nil)

(def ^:dynamic insert_idx nil)

(def ^:dynamic insert_node nil)

(def ^:dynamic insert_nodes nil)

(def ^:dynamic insert_state nil)

(def ^:dynamic new_node_state nil)

(def ^:dynamic tree_sort_i nil)

(def ^:dynamic tree_sort_state nil)

(defn new_node [new_node_state_p new_node_value]
  (binding [new_node_state nil] (try (do (set! new_node_state new_node_state_p) (set! new_node_state (assoc new_node_state :nodes (conj (:nodes new_node_state) {:left (- 1) :right (- 1) :value new_node_value}))) (throw (ex-info "return" {:v (- (count (:nodes new_node_state)) 1)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn insert [insert_state_p insert_value]
  (binding [insert_current nil insert_idx nil insert_node nil insert_nodes nil insert_state nil] (try (do (set! insert_state insert_state_p) (when (= (:root insert_state) (- 1)) (do (set! insert_state (assoc insert_state :root (new_node insert_state insert_value))) (throw (ex-info "return" {:v nil})))) (set! insert_current (:root insert_state)) (set! insert_nodes (:nodes insert_state)) (while true (do (set! insert_node (get insert_nodes insert_current)) (if (< insert_value (:value insert_node)) (do (when (= (:left insert_node) (- 1)) (do (set! insert_idx (new_node insert_state insert_value)) (set! insert_nodes (:nodes insert_state)) (set! insert_node (assoc insert_node :left insert_idx)) (set! insert_nodes (assoc insert_nodes insert_current insert_node)) (set! insert_state (assoc insert_state :nodes insert_nodes)) (throw (ex-info "return" {:v nil})))) (set! insert_current (:left insert_node))) (if (> insert_value (:value insert_node)) (do (when (= (:right insert_node) (- 1)) (do (set! insert_idx (new_node insert_state insert_value)) (set! insert_nodes (:nodes insert_state)) (set! insert_node (assoc insert_node :right insert_idx)) (set! insert_nodes (assoc insert_nodes insert_current insert_node)) (set! insert_state (assoc insert_state :nodes insert_nodes)) (throw (ex-info "return" {:v nil})))) (set! insert_current (:right insert_node))) (throw (ex-info "return" {:v nil}))))))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn inorder [inorder_state inorder_idx]
  (binding [inorder_i nil inorder_node nil inorder_result nil inorder_right_part nil] (try (do (when (= inorder_idx (- 1)) (throw (ex-info "return" {:v []}))) (set! inorder_node (get (:nodes inorder_state) inorder_idx)) (set! inorder_result (inorder inorder_state (:left inorder_node))) (set! inorder_result (conj inorder_result (:value inorder_node))) (set! inorder_right_part (inorder inorder_state (:right inorder_node))) (set! inorder_i 0) (while (< inorder_i (count inorder_right_part)) (do (set! inorder_result (conj inorder_result (nth inorder_right_part inorder_i))) (set! inorder_i (+ inorder_i 1)))) (throw (ex-info "return" {:v inorder_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn tree_sort [tree_sort_arr]
  (binding [tree_sort_i nil tree_sort_state nil] (try (do (set! tree_sort_state {:nodes [] :root (- 1)}) (set! tree_sort_i 0) (while (< tree_sort_i (count tree_sort_arr)) (do (insert tree_sort_state (nth tree_sort_arr tree_sort_i)) (set! tree_sort_i (+ tree_sort_i 1)))) (if (= (:root tree_sort_state) (- 1)) [] (inorder tree_sort_state (:root tree_sort_state)))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (tree_sort [])))
      (println (str (tree_sort [1])))
      (println (str (tree_sort [1 2])))
      (println (str (tree_sort [5 2 7])))
      (println (str (tree_sort [5 (- 4) 9 2 7])))
      (println (str (tree_sort [5 6 1 (- 1) 4 37 2 7])))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
