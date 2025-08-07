(ns main (:refer-clojure :exclude [new_trie remove_key insert insert_many find delete print_words test_trie print_results]))

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

(declare new_trie remove_key insert insert_many find delete print_words test_trie print_results)

(def ^:dynamic _delete_ch nil)

(def ^:dynamic _delete_child_idx nil)

(def ^:dynamic _delete_children nil)

(def ^:dynamic _delete_new_children nil)

(def ^:dynamic _delete_node nil)

(def ^:dynamic _delete_should_delete nil)

(def ^:dynamic delete_nodes nil)

(def ^:dynamic delete_trie nil)

(def ^:dynamic dfs_node nil)

(def ^:dynamic find_ch nil)

(def ^:dynamic find_children nil)

(def ^:dynamic find_curr nil)

(def ^:dynamic find_i nil)

(def ^:dynamic find_node nil)

(def ^:dynamic find_nodes nil)

(def ^:dynamic insert_ch nil)

(def ^:dynamic insert_child_idx nil)

(def ^:dynamic insert_children nil)

(def ^:dynamic insert_curr nil)

(def ^:dynamic insert_i nil)

(def ^:dynamic insert_new_children nil)

(def ^:dynamic insert_new_node nil)

(def ^:dynamic insert_node nil)

(def ^:dynamic insert_nodes nil)

(def ^:dynamic insert_trie nil)

(def ^:dynamic remove_key_out nil)

(def ^:dynamic test_trie_ok nil)

(def ^:dynamic test_trie_t nil)

(def ^:dynamic test_trie_t2 nil)

(def ^:dynamic test_trie_t3 nil)

(def ^:dynamic test_trie_t4 nil)

(def ^:dynamic test_trie_trie nil)

(def ^:dynamic test_trie_words nil)

(defn new_trie []
  (try (throw (ex-info "return" {:v {:nodes [{:children {} :is_leaf false}]}})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn remove_key [remove_key_m remove_key_k]
  (binding [remove_key_out nil] (try (do (set! remove_key_out {}) (doseq [key remove_key_m] (when (not= key remove_key_k) (set! remove_key_out (assoc remove_key_out key (nth remove_key_m key))))) (throw (ex-info "return" {:v remove_key_out}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn insert [insert_trie_p insert_word]
  (binding [insert_ch nil insert_child_idx nil insert_children nil insert_curr nil insert_i nil insert_new_children nil insert_new_node nil insert_node nil insert_nodes nil insert_trie nil] (do (set! insert_trie insert_trie_p) (set! insert_nodes (:nodes insert_trie)) (set! insert_curr 0) (set! insert_i 0) (while (< insert_i (count insert_word)) (do (set! insert_ch (nth insert_word insert_i)) (set! insert_child_idx (- 1)) (set! insert_children (:children (nth insert_nodes insert_curr))) (if (in insert_ch insert_children) (set! insert_child_idx (nth insert_children insert_ch)) (do (set! insert_new_node {:children {} :is_leaf false}) (set! insert_nodes (conj insert_nodes insert_new_node)) (set! insert_child_idx (- (count insert_nodes) 1)) (set! insert_new_children insert_children) (set! insert_new_children (assoc insert_new_children insert_ch insert_child_idx)) (set! insert_node (nth insert_nodes insert_curr)) (set! insert_node (assoc insert_node :children insert_new_children)) (set! insert_nodes (assoc insert_nodes insert_curr insert_node)))) (set! insert_curr insert_child_idx) (set! insert_i (+ insert_i 1)))) (set! insert_node (nth insert_nodes insert_curr)) (set! insert_node (assoc insert_node :is_leaf true)) (set! insert_nodes (assoc insert_nodes insert_curr insert_node)) (set! insert_trie (assoc insert_trie :nodes insert_nodes)))))

(defn insert_many [insert_many_trie insert_many_words]
  (doseq [w insert_many_words] (insert insert_many_trie w)))

(defn find [find_trie find_word]
  (binding [find_ch nil find_children nil find_curr nil find_i nil find_node nil find_nodes nil] (try (do (set! find_nodes (:nodes find_trie)) (set! find_curr 0) (set! find_i 0) (while (< find_i (count find_word)) (do (set! find_ch (nth find_word find_i)) (set! find_children (:children (nth find_nodes find_curr))) (when (not (in find_ch find_children)) (throw (ex-info "return" {:v false}))) (set! find_curr (nth find_children find_ch)) (set! find_i (+ find_i 1)))) (set! find_node (nth find_nodes find_curr)) (throw (ex-info "return" {:v (:is_leaf find_node)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn _delete [delete_trie delete_word _delete_idx _delete_pos]
  (binding [_delete_ch nil _delete_child_idx nil _delete_children nil _delete_new_children nil _delete_node nil _delete_should_delete nil] (try (do (when (= _delete_pos (count delete_word)) (do (set! _delete_node (nth delete_nodes _delete_idx)) (when (= (:is_leaf _delete_node) false) (throw (ex-info "return" {:v false}))) (set! _delete_node (assoc _delete_node :is_leaf false)) (alter-var-root (var delete_nodes) (fn [_] (assoc delete_nodes _delete_idx _delete_node))) (throw (ex-info "return" {:v (= (count (:children _delete_node)) 0)})))) (set! _delete_node (nth delete_nodes _delete_idx)) (set! _delete_children (:children _delete_node)) (set! _delete_ch (nth delete_word _delete_pos)) (when (not (in _delete_ch _delete_children)) (throw (ex-info "return" {:v false}))) (set! _delete_child_idx (nth _delete_children _delete_ch)) (set! _delete_should_delete (_delete _delete_child_idx (+ _delete_pos 1))) (set! _delete_node (nth delete_nodes _delete_idx)) (when _delete_should_delete (do (set! _delete_new_children (remove_key (:children _delete_node) _delete_ch)) (set! _delete_node (assoc _delete_node :children _delete_new_children)) (alter-var-root (var delete_nodes) (fn [_] (assoc delete_nodes _delete_idx _delete_node))) (throw (ex-info "return" {:v (and (= (count _delete_new_children) 0) (= (:is_leaf _delete_node) false))})))) (alter-var-root (var delete_nodes) (fn [_] (assoc delete_nodes _delete_idx _delete_node))) (throw (ex-info "return" {:v false}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn delete [delete_trie_p delete_word]
  (binding [delete_nodes nil delete_trie nil] (try (do (set! delete_trie delete_trie_p) (set! delete_nodes (:nodes delete_trie)) (_delete delete_trie delete_word 0 0) (set! delete_trie (assoc delete_trie :nodes delete_nodes))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn dfs [print_words_trie dfs_idx dfs_word]
  (binding [dfs_node nil] (do (set! dfs_node (get (:nodes print_words_trie) dfs_idx)) (when (:is_leaf dfs_node) (println dfs_word)) (doseq [key (keys (:children dfs_node))] (dfs (get (:children dfs_node) key) (str dfs_word key))))))

(defn print_words [print_words_trie]
  (dfs print_words_trie 0 ""))

(defn test_trie []
  (binding [test_trie_ok nil test_trie_t nil test_trie_t2 nil test_trie_t3 nil test_trie_t4 nil test_trie_trie nil test_trie_words nil] (try (do (set! test_trie_words ["banana" "bananas" "bandana" "band" "apple" "all" "beast"]) (set! test_trie_trie (new_trie)) (insert_many test_trie_trie test_trie_words) (set! test_trie_ok true) (doseq [w test_trie_words] (set! test_trie_ok (and test_trie_ok (find test_trie_trie w)))) (set! test_trie_ok (and test_trie_ok (find test_trie_trie "banana"))) (set! test_trie_t (find test_trie_trie "bandanas")) (set! test_trie_ok (and test_trie_ok (= test_trie_t false))) (set! test_trie_t2 (find test_trie_trie "apps")) (set! test_trie_ok (and test_trie_ok (= test_trie_t2 false))) (set! test_trie_ok (and test_trie_ok (find test_trie_trie "apple"))) (set! test_trie_ok (and test_trie_ok (find test_trie_trie "all"))) (delete test_trie_trie "all") (set! test_trie_t3 (find test_trie_trie "all")) (set! test_trie_ok (and test_trie_ok (= test_trie_t3 false))) (delete test_trie_trie "banana") (set! test_trie_t4 (find test_trie_trie "banana")) (set! test_trie_ok (and test_trie_ok (= test_trie_t4 false))) (set! test_trie_ok (and test_trie_ok (find test_trie_trie "bananas"))) (throw (ex-info "return" {:v test_trie_ok}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn print_results [print_results_msg print_results_passes]
  (if print_results_passes (println (str print_results_msg " works!")) (println (str print_results_msg " doesn't work :("))))

(def ^:dynamic main_trie (new_trie))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (print_results "Testing trie functionality" (test_trie))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
