(ns main (:refer-clojure :exclude [new_node has_key add_suffix build_suffix_tree new_suffix_tree search]))

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

(declare new_node has_key add_suffix build_suffix_tree new_suffix_tree search)

(def ^:dynamic add_suffix_ch nil)

(def ^:dynamic add_suffix_children nil)

(def ^:dynamic add_suffix_j nil)

(def ^:dynamic add_suffix_new_idx nil)

(def ^:dynamic add_suffix_node nil)

(def ^:dynamic add_suffix_node_idx nil)

(def ^:dynamic add_suffix_nodes nil)

(def ^:dynamic add_suffix_tree nil)

(def ^:dynamic build_suffix_tree_i nil)

(def ^:dynamic build_suffix_tree_k nil)

(def ^:dynamic build_suffix_tree_n nil)

(def ^:dynamic build_suffix_tree_suffix nil)

(def ^:dynamic build_suffix_tree_t nil)

(def ^:dynamic build_suffix_tree_text nil)

(def ^:dynamic new_suffix_tree_tree nil)

(def ^:dynamic search_ch nil)

(def ^:dynamic search_children nil)

(def ^:dynamic search_i nil)

(def ^:dynamic search_node nil)

(def ^:dynamic search_node_idx nil)

(def ^:dynamic search_nodes nil)

(defn new_node []
  (try (throw (ex-info "return" {:v {:children {} :end (- 1) :is_end_of_string false :start (- 1)}})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn has_key [has_key_m has_key_k]
  (try (do (doseq [key has_key_m] (when (= key has_key_k) (throw (ex-info "return" {:v true})))) (throw (ex-info "return" {:v false}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn add_suffix [add_suffix_tree_p add_suffix_suffix add_suffix_index]
  (binding [add_suffix_ch nil add_suffix_children nil add_suffix_j nil add_suffix_new_idx nil add_suffix_node nil add_suffix_node_idx nil add_suffix_nodes nil add_suffix_tree nil] (try (do (set! add_suffix_tree add_suffix_tree_p) (set! add_suffix_nodes (:nodes add_suffix_tree)) (set! add_suffix_node_idx 0) (set! add_suffix_j 0) (while (< add_suffix_j (count add_suffix_suffix)) (do (set! add_suffix_ch (subs add_suffix_suffix add_suffix_j (min (+ add_suffix_j 1) (count add_suffix_suffix)))) (set! add_suffix_node (nth add_suffix_nodes add_suffix_node_idx)) (set! add_suffix_children (:children add_suffix_node)) (when (not (has_key add_suffix_children add_suffix_ch)) (do (set! add_suffix_nodes (conj add_suffix_nodes (new_node))) (set! add_suffix_new_idx (- (count add_suffix_nodes) 1)) (set! add_suffix_children (assoc add_suffix_children add_suffix_ch add_suffix_new_idx)))) (set! add_suffix_node (assoc add_suffix_node :children add_suffix_children)) (set! add_suffix_nodes (assoc add_suffix_nodes add_suffix_node_idx add_suffix_node)) (set! add_suffix_node_idx (get add_suffix_children add_suffix_ch)) (set! add_suffix_j (+ add_suffix_j 1)))) (set! add_suffix_node (nth add_suffix_nodes add_suffix_node_idx)) (set! add_suffix_node (assoc add_suffix_node :is_end_of_string true)) (set! add_suffix_node (assoc add_suffix_node :start add_suffix_index)) (set! add_suffix_node (assoc add_suffix_node :end (- (+ add_suffix_index (count add_suffix_suffix)) 1))) (set! add_suffix_nodes (assoc add_suffix_nodes add_suffix_node_idx add_suffix_node)) (set! add_suffix_tree (assoc add_suffix_tree :nodes add_suffix_nodes)) (throw (ex-info "return" {:v add_suffix_tree}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn build_suffix_tree [build_suffix_tree_tree]
  (binding [build_suffix_tree_i nil build_suffix_tree_k nil build_suffix_tree_n nil build_suffix_tree_suffix nil build_suffix_tree_t nil build_suffix_tree_text nil] (try (do (set! build_suffix_tree_text (:text build_suffix_tree_tree)) (set! build_suffix_tree_n (count build_suffix_tree_text)) (set! build_suffix_tree_i 0) (set! build_suffix_tree_t build_suffix_tree_tree) (while (< build_suffix_tree_i build_suffix_tree_n) (do (set! build_suffix_tree_suffix "") (set! build_suffix_tree_k build_suffix_tree_i) (while (< build_suffix_tree_k build_suffix_tree_n) (do (set! build_suffix_tree_suffix (str build_suffix_tree_suffix (subs build_suffix_tree_text build_suffix_tree_k (min (+ build_suffix_tree_k 1) (count build_suffix_tree_text))))) (set! build_suffix_tree_k (+ build_suffix_tree_k 1)))) (set! build_suffix_tree_t (add_suffix build_suffix_tree_t build_suffix_tree_suffix build_suffix_tree_i)) (set! build_suffix_tree_i (+ build_suffix_tree_i 1)))) (throw (ex-info "return" {:v build_suffix_tree_t}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn new_suffix_tree [new_suffix_tree_text]
  (binding [new_suffix_tree_tree nil] (try (do (set! new_suffix_tree_tree {:nodes [] :text new_suffix_tree_text}) (set! new_suffix_tree_tree (assoc new_suffix_tree_tree :nodes (conj (:nodes new_suffix_tree_tree) (new_node)))) (set! new_suffix_tree_tree (build_suffix_tree new_suffix_tree_tree)) (throw (ex-info "return" {:v new_suffix_tree_tree}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn search [search_tree search_pattern]
  (binding [search_ch nil search_children nil search_i nil search_node nil search_node_idx nil search_nodes nil] (try (do (set! search_node_idx 0) (set! search_i 0) (set! search_nodes (:nodes search_tree)) (while (< search_i (count search_pattern)) (do (set! search_ch (subs search_pattern search_i (min (+ search_i 1) (count search_pattern)))) (set! search_node (nth search_nodes search_node_idx)) (set! search_children (:children search_node)) (when (not (has_key search_children search_ch)) (throw (ex-info "return" {:v false}))) (set! search_node_idx (get search_children search_ch)) (set! search_i (+ search_i 1)))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_st (new_suffix_tree "bananas"))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (search main_st "ana")))
      (println (str (search main_st "apple")))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
