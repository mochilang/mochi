(ns main (:refer-clojure :exclude [make_linked_list node_to_string main]))

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

(declare make_linked_list node_to_string main)

(def ^:dynamic main_head nil)

(def ^:dynamic main_list_data nil)

(def ^:dynamic make_linked_list_current nil)

(def ^:dynamic make_linked_list_head nil)

(def ^:dynamic make_linked_list_i nil)

(def ^:dynamic node_to_string_index nil)

(def ^:dynamic node_to_string_node nil)

(def ^:dynamic node_to_string_s nil)

(def ^:dynamic main_NIL (- 0 1))

(def ^:dynamic main_nodes [])

(defn make_linked_list [make_linked_list_elements]
  (binding [make_linked_list_current nil make_linked_list_head nil make_linked_list_i nil] (try (do (when (= (count make_linked_list_elements) 0) (throw (Exception. "The Elements List is empty"))) (alter-var-root (var main_nodes) (fn [_] [])) (alter-var-root (var main_nodes) (fn [_] (conj main_nodes {:data (nth make_linked_list_elements 0) :next main_NIL}))) (set! make_linked_list_head 0) (set! make_linked_list_current make_linked_list_head) (set! make_linked_list_i 1) (while (< make_linked_list_i (count make_linked_list_elements)) (do (alter-var-root (var main_nodes) (fn [_] (conj main_nodes {:data (nth make_linked_list_elements make_linked_list_i) :next main_NIL}))) (alter-var-root (var main_nodes) (fn [_] (assoc-in main_nodes [make_linked_list_current :next] (- (count main_nodes) 1)))) (set! make_linked_list_current (- (count main_nodes) 1)) (set! make_linked_list_i (+ make_linked_list_i 1)))) (throw (ex-info "return" {:v make_linked_list_head}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn node_to_string [node_to_string_head]
  (binding [node_to_string_index nil node_to_string_node nil node_to_string_s nil] (try (do (set! node_to_string_s "") (set! node_to_string_index node_to_string_head) (while (not= node_to_string_index main_NIL) (do (set! node_to_string_node (nth main_nodes node_to_string_index)) (set! node_to_string_s (str (str (str node_to_string_s "<") (str (:data node_to_string_node))) "> ---> ")) (set! node_to_string_index (:next node_to_string_node)))) (set! node_to_string_s (str node_to_string_s "<END>")) (throw (ex-info "return" {:v node_to_string_s}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_head nil main_list_data nil] (do (set! main_list_data [1 3 5 32 44 12 43]) (println (str "List: " (str main_list_data))) (println "Creating Linked List from List.") (set! main_head (make_linked_list main_list_data)) (println "Linked List:") (println (node_to_string main_head)))))

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
