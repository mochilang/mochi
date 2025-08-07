(ns main (:refer-clojure :exclude [new_node new_tree match_prefix insert_many insert find remove_key has_key delete print_tree test_trie pytests main]))

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

(declare new_node new_tree match_prefix insert_many insert find remove_key has_key delete print_tree test_trie pytests main)

(def ^:dynamic delete_child nil)

(def ^:dynamic delete_child_idx nil)

(def ^:dynamic delete_children nil)

(def ^:dynamic delete_deleted nil)

(def ^:dynamic delete_merge_idx nil)

(def ^:dynamic delete_merge_node nil)

(def ^:dynamic delete_node nil)

(def ^:dynamic delete_nodes nil)

(def ^:dynamic delete_only_key nil)

(def ^:dynamic delete_res nil)

(def ^:dynamic delete_tree nil)

(def ^:dynamic find_child nil)

(def ^:dynamic find_child_idx nil)

(def ^:dynamic find_children nil)

(def ^:dynamic find_node nil)

(def ^:dynamic find_nodes nil)

(def ^:dynamic find_res nil)

(def ^:dynamic first_v nil)

(def ^:dynamic insert_child nil)

(def ^:dynamic insert_child_idx nil)

(def ^:dynamic insert_children nil)

(def ^:dynamic insert_new_children nil)

(def ^:dynamic insert_new_idx nil)

(def ^:dynamic insert_node nil)

(def ^:dynamic insert_nodes nil)

(def ^:dynamic insert_res nil)

(def ^:dynamic insert_tree nil)

(def ^:dynamic main_tree nil)

(def ^:dynamic main_words nil)

(def ^:dynamic match_prefix_common nil)

(def ^:dynamic match_prefix_min_len nil)

(def ^:dynamic match_prefix_p nil)

(def ^:dynamic match_prefix_rem_prefix nil)

(def ^:dynamic match_prefix_rem_word nil)

(def ^:dynamic match_prefix_w nil)

(def ^:dynamic match_prefix_x nil)

(def ^:dynamic new_tree_nodes nil)

(def ^:dynamic print_tree_child_idx nil)

(def ^:dynamic print_tree_children nil)

(def ^:dynamic print_tree_i nil)

(def ^:dynamic print_tree_line nil)

(def ^:dynamic print_tree_node nil)

(def ^:dynamic print_tree_nodes nil)

(def ^:dynamic remove_key_out nil)

(def ^:dynamic test_trie_ok nil)

(def ^:dynamic test_trie_tree nil)

(def ^:dynamic test_trie_words nil)

(defn new_node [new_node_prefix new_node_is_leaf]
  (try (throw (ex-info "return" {:v {:children {} :is_leaf new_node_is_leaf :prefix new_node_prefix}})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn new_tree []
  (binding [new_tree_nodes nil] (try (do (set! new_tree_nodes [(new_node "" false)]) (throw (ex-info "return" {:v {:nodes new_tree_nodes}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn match_prefix [match_prefix_node match_prefix_word]
  (binding [match_prefix_common nil match_prefix_min_len nil match_prefix_p nil match_prefix_rem_prefix nil match_prefix_rem_word nil match_prefix_w nil match_prefix_x nil] (try (do (set! match_prefix_x 0) (set! match_prefix_p (:prefix match_prefix_node)) (set! match_prefix_w match_prefix_word) (set! match_prefix_min_len (count match_prefix_p)) (when (< (count match_prefix_w) match_prefix_min_len) (set! match_prefix_min_len (count match_prefix_w))) (loop [while_flag_1 true] (when (and while_flag_1 (< match_prefix_x match_prefix_min_len)) (cond (not= (subs match_prefix_p match_prefix_x (min (+ match_prefix_x 1) (count match_prefix_p))) (subs match_prefix_w match_prefix_x (min (+ match_prefix_x 1) (count match_prefix_w)))) (recur false) :else (do (set! match_prefix_x (+ match_prefix_x 1)) (recur while_flag_1))))) (set! match_prefix_common (subs match_prefix_p 0 (min match_prefix_x (count match_prefix_p)))) (set! match_prefix_rem_prefix (subs match_prefix_p match_prefix_x (min (count match_prefix_p) (count match_prefix_p)))) (set! match_prefix_rem_word (subs match_prefix_w match_prefix_x (min (count match_prefix_w) (count match_prefix_w)))) (throw (ex-info "return" {:v {:common match_prefix_common :rem_prefix match_prefix_rem_prefix :rem_word match_prefix_rem_word}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn insert_many [insert_many_tree insert_many_words]
  (doseq [w insert_many_words] (insert insert_many_tree 0 w)))

(defn insert [insert_tree_p insert_idx insert_word]
  (binding [first_v nil insert_child nil insert_child_idx nil insert_children nil insert_new_children nil insert_new_idx nil insert_node nil insert_nodes nil insert_res nil insert_tree nil] (try (do (set! insert_tree insert_tree_p) (set! insert_nodes (:nodes insert_tree)) (set! insert_node (nth insert_nodes insert_idx)) (when (and (= (:prefix insert_node) insert_word) (not (:is_leaf insert_node))) (do (set! insert_node (assoc insert_node :is_leaf true)) (set! insert_nodes (assoc insert_nodes insert_idx insert_node)) (set! insert_tree (assoc insert_tree :nodes insert_nodes)) (throw (ex-info "return" {:v nil})))) (set! first_v (subs insert_word 0 (min 1 (count insert_word)))) (set! insert_children (:children insert_node)) (when (not (has_key insert_children first_v)) (do (set! insert_new_idx (count insert_nodes)) (set! insert_nodes (conj insert_nodes (new_node insert_word true))) (set! insert_children (assoc insert_children first_v insert_new_idx)) (set! insert_node (assoc insert_node :children insert_children)) (set! insert_nodes (assoc insert_nodes insert_idx insert_node)) (set! insert_tree (assoc insert_tree :nodes insert_nodes)) (throw (ex-info "return" {:v nil})))) (set! insert_child_idx (get insert_children first_v)) (set! insert_child (nth insert_nodes insert_child_idx)) (set! insert_res (match_prefix insert_child insert_word)) (when (= (:rem_prefix insert_res) "") (do (insert insert_tree insert_child_idx (:rem_word insert_res)) (throw (ex-info "return" {:v nil})))) (set! insert_child (assoc insert_child :prefix (:rem_prefix insert_res))) (set! insert_nodes (assoc insert_nodes insert_child_idx insert_child)) (set! insert_new_children {}) (set! insert_new_children (assoc insert_new_children (subs (:rem_prefix insert_res) 0 (min 1 (count (:rem_prefix insert_res)))) insert_child_idx)) (set! insert_new_idx (count insert_nodes)) (set! insert_nodes (conj insert_nodes (new_node (:common insert_res) false))) (set! insert_nodes (assoc-in insert_nodes [insert_new_idx :children] insert_new_children)) (if (= (:rem_word insert_res) "") (set! insert_nodes (assoc-in insert_nodes [insert_new_idx :is_leaf] true)) (insert insert_tree insert_new_idx (:rem_word insert_res))) (set! insert_children (assoc insert_children first_v insert_new_idx)) (set! insert_node (assoc insert_node :children insert_children)) (set! insert_nodes (assoc insert_nodes insert_idx insert_node)) (set! insert_tree (assoc insert_tree :nodes insert_nodes))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn find [find_tree find_idx find_word]
  (binding [find_child nil find_child_idx nil find_children nil find_node nil find_nodes nil find_res nil first_v nil] (try (do (set! find_nodes (:nodes find_tree)) (set! find_node (nth find_nodes find_idx)) (set! first_v (subs find_word 0 (min 1 (count find_word)))) (set! find_children (:children find_node)) (when (not (has_key find_children first_v)) (throw (ex-info "return" {:v false}))) (set! find_child_idx (get find_children first_v)) (set! find_child (nth find_nodes find_child_idx)) (set! find_res (match_prefix find_child find_word)) (when (not= (:rem_prefix find_res) "") (throw (ex-info "return" {:v false}))) (if (= (:rem_word find_res) "") (:is_leaf find_child) (find find_tree find_child_idx (:rem_word find_res)))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn remove_key [remove_key_m remove_key_k]
  (binding [remove_key_out nil] (try (do (set! remove_key_out {}) (doseq [key remove_key_m] (when (not= key remove_key_k) (set! remove_key_out (assoc remove_key_out key (nth remove_key_m key))))) (throw (ex-info "return" {:v remove_key_out}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn has_key [has_key_m has_key_k]
  (try (do (doseq [key has_key_m] (when (= key has_key_k) (throw (ex-info "return" {:v true})))) (throw (ex-info "return" {:v false}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn delete [delete_tree_p delete_idx delete_word]
  (binding [delete_child nil delete_child_idx nil delete_children nil delete_deleted nil delete_merge_idx nil delete_merge_node nil delete_node nil delete_nodes nil delete_only_key nil delete_res nil delete_tree nil first_v nil] (try (do (set! delete_tree delete_tree_p) (set! delete_nodes (:nodes delete_tree)) (set! delete_node (nth delete_nodes delete_idx)) (set! first_v (subs delete_word 0 (min 1 (count delete_word)))) (set! delete_children (:children delete_node)) (when (not (has_key delete_children first_v)) (throw (ex-info "return" {:v false}))) (set! delete_child_idx (get delete_children first_v)) (set! delete_child (nth delete_nodes delete_child_idx)) (set! delete_res (match_prefix delete_child delete_word)) (when (not= (:rem_prefix delete_res) "") (throw (ex-info "return" {:v false}))) (when (not= (:rem_word delete_res) "") (do (set! delete_deleted (delete delete_tree delete_child_idx (:rem_word delete_res))) (when delete_deleted (do (set! delete_nodes (:nodes delete_tree)) (set! delete_node (nth delete_nodes delete_idx)))) (throw (ex-info "return" {:v delete_deleted})))) (when (not (:is_leaf delete_child)) (throw (ex-info "return" {:v false}))) (if (= (count (:children delete_child)) 0) (do (set! delete_children (remove_key delete_children first_v)) (set! delete_node (assoc delete_node :children delete_children)) (set! delete_nodes (assoc delete_nodes delete_idx delete_node)) (set! delete_tree (assoc delete_tree :nodes delete_nodes)) (when (and (= (count delete_children) 1) (not (:is_leaf delete_node))) (do (set! delete_only_key "") (doseq [k delete_children] (set! delete_only_key k)) (set! delete_merge_idx (get delete_children delete_only_key)) (set! delete_merge_node (nth delete_nodes delete_merge_idx)) (set! delete_node (assoc delete_node :is_leaf (:is_leaf delete_merge_node))) (set! delete_node (assoc delete_node :prefix (+ (:prefix delete_node) (:prefix delete_merge_node)))) (set! delete_node (assoc delete_node :children (:children delete_merge_node))) (set! delete_nodes (assoc delete_nodes delete_idx delete_node)) (set! delete_tree (assoc delete_tree :nodes delete_nodes))))) (if (> (count (:children delete_child)) 1) (do (set! delete_child (assoc delete_child :is_leaf false)) (set! delete_nodes (assoc delete_nodes delete_child_idx delete_child)) (set! delete_tree (assoc delete_tree :nodes delete_nodes))) (do (set! delete_only_key "") (doseq [k (keys (:children delete_child))] (set! delete_only_key k)) (set! delete_merge_idx (get (:children delete_child) delete_only_key)) (set! delete_merge_node (nth delete_nodes delete_merge_idx)) (set! delete_child (assoc delete_child :is_leaf (:is_leaf delete_merge_node))) (set! delete_child (assoc delete_child :prefix (+ (:prefix delete_child) (:prefix delete_merge_node)))) (set! delete_child (assoc delete_child :children (:children delete_merge_node))) (set! delete_nodes (assoc delete_nodes delete_child_idx delete_child)) (set! delete_tree (assoc delete_tree :nodes delete_nodes))))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn print_tree [print_tree_tree print_tree_idx print_tree_height]
  (binding [print_tree_child_idx nil print_tree_children nil print_tree_i nil print_tree_line nil print_tree_node nil print_tree_nodes nil] (do (set! print_tree_nodes (:nodes print_tree_tree)) (set! print_tree_node (nth print_tree_nodes print_tree_idx)) (when (not= (:prefix print_tree_node) "") (do (set! print_tree_line "") (set! print_tree_i 0) (while (< print_tree_i print_tree_height) (do (set! print_tree_line (str print_tree_line "-")) (set! print_tree_i (+ print_tree_i 1)))) (set! print_tree_line (str (str print_tree_line " ") (:prefix print_tree_node))) (when (:is_leaf print_tree_node) (set! print_tree_line (str print_tree_line "  (leaf)"))) (println print_tree_line))) (set! print_tree_children (:children print_tree_node)) (doseq [k print_tree_children] (do (set! print_tree_child_idx (nth print_tree_children k)) (print_tree print_tree_tree print_tree_child_idx (+ print_tree_height 1)))))))

(defn test_trie []
  (binding [test_trie_ok nil test_trie_tree nil test_trie_words nil] (try (do (set! test_trie_words ["banana" "bananas" "bandana" "band" "apple" "all" "beast"]) (set! test_trie_tree (new_tree)) (insert_many test_trie_tree test_trie_words) (set! test_trie_ok true) (doseq [w test_trie_words] (when (not (find test_trie_tree 0 w)) (set! test_trie_ok false))) (when (find test_trie_tree 0 "bandanas") (set! test_trie_ok false)) (when (find test_trie_tree 0 "apps") (set! test_trie_ok false)) (delete test_trie_tree 0 "all") (when (find test_trie_tree 0 "all") (set! test_trie_ok false)) (delete test_trie_tree 0 "banana") (when (find test_trie_tree 0 "banana") (set! test_trie_ok false)) (when (not (find test_trie_tree 0 "bananas")) (set! test_trie_ok false)) (throw (ex-info "return" {:v test_trie_ok}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn pytests []
  (when (not (test_trie)) (throw (Exception. "test failed"))))

(defn main []
  (binding [main_tree nil main_words nil] (do (set! main_tree (new_tree)) (set! main_words ["banana" "bananas" "bandanas" "bandana" "band" "apple" "all" "beast"]) (insert_many main_tree main_words) (println (str "Words: " (str main_words))) (println "Tree:") (print_tree main_tree 0 0))))

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
