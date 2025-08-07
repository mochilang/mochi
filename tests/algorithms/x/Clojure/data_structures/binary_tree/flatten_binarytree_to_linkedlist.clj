(ns main (:refer-clojure :exclude [new_node build_tree flatten display]))

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

(declare new_node build_tree flatten display)

(def ^:dynamic build_tree_left_child nil)

(def ^:dynamic build_tree_n2 nil)

(def ^:dynamic build_tree_n3 nil)

(def ^:dynamic build_tree_n4 nil)

(def ^:dynamic build_tree_n5 nil)

(def ^:dynamic build_tree_n6 nil)

(def ^:dynamic build_tree_right_child nil)

(def ^:dynamic build_tree_root nil)

(def ^:dynamic display_i nil)

(def ^:dynamic display_s nil)

(def ^:dynamic flatten_i nil)

(def ^:dynamic flatten_left_vals nil)

(def ^:dynamic flatten_res nil)

(def ^:dynamic flatten_right_vals nil)

(def ^:dynamic new_node_left_child nil)

(def ^:dynamic new_node_node_data nil)

(def ^:dynamic new_node_right_child nil)

(def ^:dynamic main_node_data [0])

(def ^:dynamic main_left_child [0])

(def ^:dynamic main_right_child [0])

(defn new_node [new_node_value]
  (binding [new_node_left_child nil new_node_node_data nil new_node_right_child nil] (try (do (set! new_node_node_data (conj main_node_data new_node_value)) (set! new_node_left_child (conj main_left_child 0)) (set! new_node_right_child (conj main_right_child 0)) (throw (ex-info "return" {:v (- (count new_node_node_data) 1)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn build_tree []
  (binding [build_tree_left_child nil build_tree_n2 nil build_tree_n3 nil build_tree_n4 nil build_tree_n5 nil build_tree_n6 nil build_tree_right_child nil build_tree_root nil] (try (do (set! build_tree_root (new_node 1)) (set! build_tree_n2 (new_node 2)) (set! build_tree_n5 (new_node 5)) (set! build_tree_n3 (new_node 3)) (set! build_tree_n4 (new_node 4)) (set! build_tree_n6 (new_node 6)) (set! build_tree_left_child (assoc build_tree_left_child build_tree_root build_tree_n2)) (set! build_tree_right_child (assoc build_tree_right_child build_tree_root build_tree_n5)) (set! build_tree_left_child (assoc build_tree_left_child build_tree_n2 build_tree_n3)) (set! build_tree_right_child (assoc build_tree_right_child build_tree_n2 build_tree_n4)) (set! build_tree_right_child (assoc build_tree_right_child build_tree_n5 build_tree_n6)) (throw (ex-info "return" {:v build_tree_root}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn flatten [flatten_root]
  (binding [flatten_i nil flatten_left_vals nil flatten_res nil flatten_right_vals nil] (try (do (when (= flatten_root 0) (throw (ex-info "return" {:v []}))) (set! flatten_res [(nth main_node_data flatten_root)]) (set! flatten_left_vals (flatten (nth main_left_child flatten_root))) (set! flatten_right_vals (flatten (nth main_right_child flatten_root))) (set! flatten_i 0) (while (< flatten_i (count flatten_left_vals)) (do (set! flatten_res (conj flatten_res (nth flatten_left_vals flatten_i))) (set! flatten_i (+ flatten_i 1)))) (set! flatten_i 0) (while (< flatten_i (count flatten_right_vals)) (do (set! flatten_res (conj flatten_res (nth flatten_right_vals flatten_i))) (set! flatten_i (+ flatten_i 1)))) (throw (ex-info "return" {:v flatten_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn display [display_values]
  (binding [display_i nil display_s nil] (do (set! display_s "") (set! display_i 0) (while (< display_i (count display_values)) (do (if (= display_i 0) (set! display_s (str (nth display_values display_i))) (set! display_s (str (str display_s " ") (str (nth display_values display_i))))) (set! display_i (+ display_i 1)))) (println display_s))))

(def ^:dynamic main_root (build_tree))

(def ^:dynamic main_vals (flatten main_root))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println "Flattened Linked List:")
      (display main_vals)
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
