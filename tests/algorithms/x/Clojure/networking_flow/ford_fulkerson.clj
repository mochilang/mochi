(ns main (:refer-clojure :exclude [breadth_first_search ford_fulkerson]))

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

(declare breadth_first_search ford_fulkerson)

(declare _read_file)

(def ^:dynamic breadth_first_search_capacity nil)

(def ^:dynamic breadth_first_search_head nil)

(def ^:dynamic breadth_first_search_i nil)

(def ^:dynamic breadth_first_search_ind nil)

(def ^:dynamic breadth_first_search_parent nil)

(def ^:dynamic breadth_first_search_queue nil)

(def ^:dynamic breadth_first_search_row nil)

(def ^:dynamic breadth_first_search_u nil)

(def ^:dynamic breadth_first_search_visited nil)

(def ^:dynamic ford_fulkerson_cap nil)

(def ^:dynamic ford_fulkerson_graph nil)

(def ^:dynamic ford_fulkerson_i nil)

(def ^:dynamic ford_fulkerson_j nil)

(def ^:dynamic ford_fulkerson_max_flow nil)

(def ^:dynamic ford_fulkerson_parent nil)

(def ^:dynamic ford_fulkerson_path_flow nil)

(def ^:dynamic ford_fulkerson_prev nil)

(def ^:dynamic ford_fulkerson_s nil)

(def ^:dynamic ford_fulkerson_u nil)

(def ^:dynamic ford_fulkerson_v nil)

(def ^:dynamic main_INF nil)

(defn breadth_first_search [breadth_first_search_graph breadth_first_search_source breadth_first_search_sink breadth_first_search_parent_p]
  (binding [breadth_first_search_parent breadth_first_search_parent_p breadth_first_search_capacity nil breadth_first_search_head nil breadth_first_search_i nil breadth_first_search_ind nil breadth_first_search_queue nil breadth_first_search_row nil breadth_first_search_u nil breadth_first_search_visited nil] (try (do (set! breadth_first_search_visited []) (set! breadth_first_search_i 0) (while (< breadth_first_search_i (count breadth_first_search_graph)) (do (set! breadth_first_search_visited (conj breadth_first_search_visited false)) (set! breadth_first_search_i (+' breadth_first_search_i 1)))) (set! breadth_first_search_queue []) (set! breadth_first_search_queue (conj breadth_first_search_queue breadth_first_search_source)) (set! breadth_first_search_visited (assoc breadth_first_search_visited breadth_first_search_source true)) (set! breadth_first_search_head 0) (while (< breadth_first_search_head (count breadth_first_search_queue)) (do (set! breadth_first_search_u (nth breadth_first_search_queue breadth_first_search_head)) (set! breadth_first_search_head (+' breadth_first_search_head 1)) (set! breadth_first_search_row (nth breadth_first_search_graph breadth_first_search_u)) (set! breadth_first_search_ind 0) (while (< breadth_first_search_ind (count breadth_first_search_row)) (do (set! breadth_first_search_capacity (nth breadth_first_search_row breadth_first_search_ind)) (when (and (= (nth breadth_first_search_visited breadth_first_search_ind) false) (> breadth_first_search_capacity 0)) (do (set! breadth_first_search_queue (conj breadth_first_search_queue breadth_first_search_ind)) (set! breadth_first_search_visited (assoc breadth_first_search_visited breadth_first_search_ind true)) (set! breadth_first_search_parent (assoc breadth_first_search_parent breadth_first_search_ind breadth_first_search_u)))) (set! breadth_first_search_ind (+' breadth_first_search_ind 1)))))) (throw (ex-info "return" {:v (nth breadth_first_search_visited breadth_first_search_sink)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))) (finally (alter-var-root (var breadth_first_search_parent) (constantly breadth_first_search_parent))))))

(defn ford_fulkerson [ford_fulkerson_graph_p ford_fulkerson_source ford_fulkerson_sink]
  (binding [ford_fulkerson_graph ford_fulkerson_graph_p ford_fulkerson_cap nil ford_fulkerson_i nil ford_fulkerson_j nil ford_fulkerson_max_flow nil ford_fulkerson_parent nil ford_fulkerson_path_flow nil ford_fulkerson_prev nil ford_fulkerson_s nil ford_fulkerson_u nil ford_fulkerson_v nil] (try (do (set! ford_fulkerson_parent []) (set! ford_fulkerson_i 0) (while (< ford_fulkerson_i (count ford_fulkerson_graph)) (do (set! ford_fulkerson_parent (conj ford_fulkerson_parent (- 1))) (set! ford_fulkerson_i (+' ford_fulkerson_i 1)))) (set! ford_fulkerson_max_flow 0) (while (let [__res (breadth_first_search ford_fulkerson_graph ford_fulkerson_source ford_fulkerson_sink ford_fulkerson_parent)] (do (set! ford_fulkerson_parent breadth_first_search_parent) __res)) (do (set! ford_fulkerson_path_flow main_INF) (set! ford_fulkerson_s ford_fulkerson_sink) (while (not= ford_fulkerson_s ford_fulkerson_source) (do (set! ford_fulkerson_prev (nth ford_fulkerson_parent ford_fulkerson_s)) (set! ford_fulkerson_cap (nth (nth ford_fulkerson_graph ford_fulkerson_prev) ford_fulkerson_s)) (when (< ford_fulkerson_cap ford_fulkerson_path_flow) (set! ford_fulkerson_path_flow ford_fulkerson_cap)) (set! ford_fulkerson_s ford_fulkerson_prev))) (set! ford_fulkerson_max_flow (+' ford_fulkerson_max_flow ford_fulkerson_path_flow)) (set! ford_fulkerson_v ford_fulkerson_sink) (while (not= ford_fulkerson_v ford_fulkerson_source) (do (set! ford_fulkerson_u (nth ford_fulkerson_parent ford_fulkerson_v)) (set! ford_fulkerson_graph (assoc-in ford_fulkerson_graph [ford_fulkerson_u ford_fulkerson_v] (- (nth (nth ford_fulkerson_graph ford_fulkerson_u) ford_fulkerson_v) ford_fulkerson_path_flow))) (set! ford_fulkerson_graph (assoc-in ford_fulkerson_graph [ford_fulkerson_v ford_fulkerson_u] (+' (nth (nth ford_fulkerson_graph ford_fulkerson_v) ford_fulkerson_u) ford_fulkerson_path_flow))) (set! ford_fulkerson_v ford_fulkerson_u))) (set! ford_fulkerson_j 0) (while (< ford_fulkerson_j (count ford_fulkerson_parent)) (do (set! ford_fulkerson_parent (assoc ford_fulkerson_parent ford_fulkerson_j (- 1))) (set! ford_fulkerson_j (+' ford_fulkerson_j 1)))))) (throw (ex-info "return" {:v ford_fulkerson_max_flow}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))) (finally (alter-var-root (var ford_fulkerson_graph) (constantly ford_fulkerson_graph))))))

(def ^:dynamic main_graph nil)

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (alter-var-root (var main_INF) (constantly 1000000000))
      (alter-var-root (var main_graph) (constantly [[0 16 13 0 0 0] [0 0 10 12 0 0] [0 4 0 0 14 0] [0 0 9 0 0 20] [0 0 0 7 0 4] [0 0 0 0 0 0]]))
      (println (mochi_str (let [__res (ford_fulkerson main_graph 0 5)] (do (alter-var-root (var main_graph) (constantly ford_fulkerson_graph)) __res))))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
