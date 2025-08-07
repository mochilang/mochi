(ns main (:refer-clojure :exclude [new_suffix_tree_node empty_suffix_tree_node has_key]))

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

(declare new_suffix_tree_node empty_suffix_tree_node has_key)

(defn new_suffix_tree_node [new_suffix_tree_node_children new_suffix_tree_node_is_end_of_string new_suffix_tree_node_start new_suffix_tree_node_end new_suffix_tree_node_suffix_link]
  (try (throw (ex-info "return" {:v {:children new_suffix_tree_node_children :end new_suffix_tree_node_end :is_end_of_string new_suffix_tree_node_is_end_of_string :start new_suffix_tree_node_start :suffix_link new_suffix_tree_node_suffix_link}})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn empty_suffix_tree_node []
  (try (throw (ex-info "return" {:v (new_suffix_tree_node {} false (- 0 1) (- 0 1) (- 0 1))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn has_key [has_key_m has_key_k]
  (try (do (doseq [key has_key_m] (when (= key has_key_k) (throw (ex-info "return" {:v true})))) (throw (ex-info "return" {:v false}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(def ^:dynamic main_root (new_suffix_tree_node {"a" 1} false (- 0 1) (- 0 1) (- 0 1)))

(def ^:dynamic main_leaf (new_suffix_tree_node {} true 0 2 0))

(def ^:dynamic main_nodes [main_root main_leaf])

(def ^:dynamic main_root_check (nth main_nodes 0))

(def ^:dynamic main_leaf_check (nth main_nodes 1))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (has_key (:children main_root_check) "a")))
      (println (str (:is_end_of_string main_leaf_check)))
      (println (str (:start main_leaf_check)))
      (println (str (:end main_leaf_check)))
      (println (str (:suffix_link main_leaf_check)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
