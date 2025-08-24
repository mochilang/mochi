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

(defn toi [s]
  (int (Double/valueOf (str s))))

(defn _ord [s]
  (int (first s)))

(defn mochi_str [v]
  (cond (float? v) (let [s (str v)] (if (clojure.string/ends-with? s ".0") (subs s 0 (- (count s) 2)) s)) :else (str v)))

(defn _fetch [url]
  {:data [{:from "" :intensity {:actual 0 :forecast 0 :index ""} :to ""}]})

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare new_node build_tree flatten display)

(declare _read_file)

(def ^:dynamic build_tree_n2 nil)

(def ^:dynamic build_tree_n3 nil)

(def ^:dynamic build_tree_n4 nil)

(def ^:dynamic build_tree_n5 nil)

(def ^:dynamic build_tree_n6 nil)

(def ^:dynamic build_tree_root nil)

(def ^:dynamic display_i nil)

(def ^:dynamic display_s nil)

(def ^:dynamic flatten_i nil)

(def ^:dynamic flatten_left_vals nil)

(def ^:dynamic flatten_res nil)

(def ^:dynamic flatten_right_vals nil)

(def ^:dynamic main_node_data nil)

(def ^:dynamic main_left_child nil)

(def ^:dynamic main_right_child nil)

(defn new_node [new_node_value]
  (try (do (alter-var-root (var main_node_data) (fn [_] (conj main_node_data new_node_value))) (alter-var-root (var main_left_child) (fn [_] (conj main_left_child 0))) (alter-var-root (var main_right_child) (fn [_] (conj main_right_child 0))) (throw (ex-info "return" {:v (- (count main_node_data) 1)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn build_tree []
  (binding [build_tree_n2 nil build_tree_n3 nil build_tree_n4 nil build_tree_n5 nil build_tree_n6 nil build_tree_root nil] (try (do (set! build_tree_root (new_node 1)) (set! build_tree_n2 (new_node 2)) (set! build_tree_n5 (new_node 5)) (set! build_tree_n3 (new_node 3)) (set! build_tree_n4 (new_node 4)) (set! build_tree_n6 (new_node 6)) (alter-var-root (var main_left_child) (fn [_] (assoc main_left_child build_tree_root build_tree_n2))) (alter-var-root (var main_right_child) (fn [_] (assoc main_right_child build_tree_root build_tree_n5))) (alter-var-root (var main_left_child) (fn [_] (assoc main_left_child build_tree_n2 build_tree_n3))) (alter-var-root (var main_right_child) (fn [_] (assoc main_right_child build_tree_n2 build_tree_n4))) (alter-var-root (var main_right_child) (fn [_] (assoc main_right_child build_tree_n5 build_tree_n6))) (throw (ex-info "return" {:v build_tree_root}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn flatten [flatten_root]
  (binding [flatten_i nil flatten_left_vals nil flatten_res nil flatten_right_vals nil] (try (do (when (= flatten_root 0) (throw (ex-info "return" {:v []}))) (set! flatten_res [(nth main_node_data flatten_root)]) (set! flatten_left_vals (flatten (nth main_left_child flatten_root))) (set! flatten_right_vals (flatten (nth main_right_child flatten_root))) (set! flatten_i 0) (while (< flatten_i (count flatten_left_vals)) (do (set! flatten_res (conj flatten_res (nth flatten_left_vals flatten_i))) (set! flatten_i (+ flatten_i 1)))) (set! flatten_i 0) (while (< flatten_i (count flatten_right_vals)) (do (set! flatten_res (conj flatten_res (nth flatten_right_vals flatten_i))) (set! flatten_i (+ flatten_i 1)))) (throw (ex-info "return" {:v flatten_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn display [display_values]
  (binding [display_i nil display_s nil] (do (set! display_s "") (set! display_i 0) (while (< display_i (count display_values)) (do (if (= display_i 0) (set! display_s (mochi_str (nth display_values display_i))) (set! display_s (str (str display_s " ") (mochi_str (nth display_values display_i))))) (set! display_i (+ display_i 1)))) (println display_s))))

(def ^:dynamic main_root nil)

(def ^:dynamic main_vals nil)

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (alter-var-root (var main_node_data) (constantly [0]))
      (alter-var-root (var main_left_child) (constantly [0]))
      (alter-var-root (var main_right_child) (constantly [0]))
      (println "Flattened Linked List:")
      (alter-var-root (var main_root) (constantly (build_tree)))
      (alter-var-root (var main_vals) (constantly (flatten main_root)))
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
