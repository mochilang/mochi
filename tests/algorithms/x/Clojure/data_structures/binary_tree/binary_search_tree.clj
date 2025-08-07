(ns main (:refer-clojure :exclude [create_node insert search inorder find_min find_max delete main]))

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

(declare create_node insert search inorder find_min find_max delete main)

(def ^:dynamic delete_min_val nil)

(def ^:dynamic delete_node nil)

(def ^:dynamic find_max_current nil)

(def ^:dynamic find_min_current nil)

(def ^:dynamic inorder_left_acc nil)

(def ^:dynamic inorder_with_node nil)

(def ^:dynamic insert_node nil)

(def ^:dynamic main_nums nil)

(def ^:dynamic main_root nil)

(defn create_node [create_node_value]
  (try (throw (ex-info "return" {:v [create_node_value nil nil]})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn insert [insert_node_p insert_value]
  (binding [insert_node nil] (try (do (set! insert_node insert_node_p) (when (= insert_node nil) (throw (ex-info "return" {:v (create_node insert_value)}))) (if (< insert_value (nth insert_node 0)) (set! insert_node (assoc insert_node 1 (insert (nth insert_node 1) insert_value))) (when (> insert_value (nth insert_node 0)) (set! insert_node (assoc insert_node 2 (insert (nth insert_node 2) insert_value))))) (throw (ex-info "return" {:v insert_node}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn search [search_node search_value]
  (try (do (when (= search_node nil) (throw (ex-info "return" {:v false}))) (when (= search_value (nth search_node 0)) (throw (ex-info "return" {:v true}))) (if (< search_value (nth search_node 0)) (search (nth search_node 1) search_value) (search (nth search_node 2) search_value))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn inorder [inorder_node inorder_acc]
  (binding [inorder_left_acc nil inorder_with_node nil] (try (do (when (= inorder_node nil) (throw (ex-info "return" {:v inorder_acc}))) (set! inorder_left_acc (inorder (nth inorder_node 1) inorder_acc)) (set! inorder_with_node (conj inorder_left_acc (nth inorder_node 0))) (throw (ex-info "return" {:v (inorder (nth inorder_node 2) inorder_with_node)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn find_min [find_min_node]
  (binding [find_min_current nil] (try (do (set! find_min_current find_min_node) (while (not= (nth find_min_current 1) nil) (set! find_min_current (nth find_min_current 1))) (throw (ex-info "return" {:v (nth find_min_current 0)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn find_max [find_max_node]
  (binding [find_max_current nil] (try (do (set! find_max_current find_max_node) (while (not= (nth find_max_current 2) nil) (set! find_max_current (nth find_max_current 2))) (throw (ex-info "return" {:v (nth find_max_current 0)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn delete [delete_node_p delete_value]
  (binding [delete_min_val nil delete_node nil] (try (do (set! delete_node delete_node_p) (when (= delete_node nil) (throw (ex-info "return" {:v nil}))) (if (< delete_value (nth delete_node 0)) (set! delete_node (assoc delete_node 1 (delete (nth delete_node 1) delete_value))) (if (> delete_value (nth delete_node 0)) (set! delete_node (assoc delete_node 2 (delete (nth delete_node 2) delete_value))) (do (when (= (nth delete_node 1) nil) (throw (ex-info "return" {:v (nth delete_node 2)}))) (when (= (nth delete_node 2) nil) (throw (ex-info "return" {:v (nth delete_node 1)}))) (set! delete_min_val (find_min (nth delete_node 2))) (set! delete_node (assoc delete_node 0 delete_min_val)) (set! delete_node (assoc delete_node 2 (delete (nth delete_node 2) delete_min_val)))))) (throw (ex-info "return" {:v delete_node}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_nums nil main_root nil] (do (set! main_root nil) (set! main_nums [8 3 6 1 10 14 13 4 7]) (doseq [v main_nums] (set! main_root (insert main_root v))) (println (str (inorder main_root []))) (println (search main_root 6)) (println (search main_root 20)) (println (find_min main_root)) (println (find_max main_root)) (set! main_root (delete main_root 6)) (println (str (inorder main_root []))))))

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
