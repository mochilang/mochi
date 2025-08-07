(ns main (:refer-clojure :exclude [random new_node split merge insert erase inorder main]))

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

(declare random new_node split merge insert erase inorder main)

(def ^:dynamic erase_res1 nil)

(def ^:dynamic erase_res2 nil)

(def ^:dynamic inorder_left_acc nil)

(def ^:dynamic inorder_with_node nil)

(def ^:dynamic insert_node nil)

(def ^:dynamic insert_res nil)

(def ^:dynamic main_root nil)

(def ^:dynamic merge_node_lefts nil)

(def ^:dynamic merge_node_rights nil)

(def ^:dynamic new_node_node_lefts nil)

(def ^:dynamic new_node_node_priors nil)

(def ^:dynamic new_node_node_rights nil)

(def ^:dynamic new_node_node_values nil)

(def ^:dynamic random_seed nil)

(def ^:dynamic split_node_lefts nil)

(def ^:dynamic split_node_rights nil)

(def ^:dynamic split_res nil)

(def ^:dynamic main_NIL (- 0 1))

(def ^:dynamic main_node_values [])

(def ^:dynamic main_node_priors [])

(def ^:dynamic main_node_lefts [])

(def ^:dynamic main_node_rights [])

(def ^:dynamic main_seed 1)

(defn random []
  (binding [random_seed nil] (try (do (set! random_seed (mod (+ (* main_seed 13) 7) 100)) (throw (ex-info "return" {:v (/ (double random_seed) 100.0)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn new_node [new_node_value]
  (binding [new_node_node_lefts nil new_node_node_priors nil new_node_node_rights nil new_node_node_values nil] (try (do (set! new_node_node_values (conj main_node_values new_node_value)) (set! new_node_node_priors (conj main_node_priors (random))) (set! new_node_node_lefts (conj main_node_lefts main_NIL)) (set! new_node_node_rights (conj main_node_rights main_NIL)) (throw (ex-info "return" {:v (- (count new_node_node_values) 1)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn split [split_root split_value]
  (binding [split_node_lefts nil split_node_rights nil split_res nil] (try (do (when (= split_root main_NIL) (throw (ex-info "return" {:v {:left main_NIL :right main_NIL}}))) (when (< split_value (nth main_node_values split_root)) (do (set! split_res (split (nth main_node_lefts split_root) split_value)) (set! split_node_lefts (assoc split_node_lefts split_root (:right split_res))) (throw (ex-info "return" {:v {:left (:left split_res) :right split_root}})))) (set! split_res (split (nth main_node_rights split_root) split_value)) (set! split_node_rights (assoc split_node_rights split_root (:left split_res))) (throw (ex-info "return" {:v {:left split_root :right (:right split_res)}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn merge [merge_left merge_right]
  (binding [merge_node_lefts nil merge_node_rights nil] (try (do (when (= merge_left main_NIL) (throw (ex-info "return" {:v merge_right}))) (when (= merge_right main_NIL) (throw (ex-info "return" {:v merge_left}))) (when (< (nth main_node_priors merge_left) (nth main_node_priors merge_right)) (do (set! merge_node_rights (assoc merge_node_rights merge_left (merge (nth main_node_rights merge_left) merge_right))) (throw (ex-info "return" {:v merge_left})))) (set! merge_node_lefts (assoc merge_node_lefts merge_right (merge merge_left (nth main_node_lefts merge_right)))) (throw (ex-info "return" {:v merge_right}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn insert [insert_root insert_value]
  (binding [insert_node nil insert_res nil] (try (do (set! insert_node (new_node insert_value)) (set! insert_res (split insert_root insert_value)) (throw (ex-info "return" {:v (merge (merge (:left insert_res) insert_node) (:right insert_res))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn erase [erase_root erase_value]
  (binding [erase_res1 nil erase_res2 nil] (try (do (set! erase_res1 (split erase_root (- erase_value 1))) (set! erase_res2 (split (:right erase_res1) erase_value)) (throw (ex-info "return" {:v (merge (:left erase_res1) (:right erase_res2))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn inorder [inorder_i inorder_acc]
  (binding [inorder_left_acc nil inorder_with_node nil] (try (do (when (= inorder_i main_NIL) (throw (ex-info "return" {:v inorder_acc}))) (set! inorder_left_acc (inorder (nth main_node_lefts inorder_i) inorder_acc)) (set! inorder_with_node (conj inorder_left_acc (nth main_node_values inorder_i))) (throw (ex-info "return" {:v (inorder (nth main_node_rights inorder_i) inorder_with_node)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_root nil] (do (set! main_root main_NIL) (set! main_root (insert main_root 1)) (println (str (inorder main_root []))) (set! main_root (insert main_root 3)) (set! main_root (insert main_root 5)) (set! main_root (insert main_root 17)) (set! main_root (insert main_root 19)) (set! main_root (insert main_root 2)) (set! main_root (insert main_root 16)) (set! main_root (insert main_root 4)) (set! main_root (insert main_root 0)) (println (str (inorder main_root []))) (set! main_root (insert main_root 4)) (set! main_root (insert main_root 4)) (set! main_root (insert main_root 4)) (println (str (inorder main_root []))) (set! main_root (erase main_root 0)) (println (str (inorder main_root []))) (set! main_root (erase main_root 4)) (println (str (inorder main_root []))))))

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
