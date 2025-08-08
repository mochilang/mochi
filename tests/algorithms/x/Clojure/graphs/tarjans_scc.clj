(ns main (:refer-clojure :exclude [tarjan create_graph main]))

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
  (Integer/parseInt (str s)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare tarjan create_graph main)

(def ^:dynamic create_graph_g nil)

(def ^:dynamic create_graph_i nil)

(def ^:dynamic create_graph_u nil)

(def ^:dynamic create_graph_v nil)

(def ^:dynamic main_edges nil)

(def ^:dynamic main_g nil)

(def ^:dynamic main_i nil)

(def ^:dynamic main_n_vertices nil)

(def ^:dynamic main_source nil)

(def ^:dynamic main_target nil)

(def ^:dynamic strong_connect_component nil)

(def ^:dynamic strong_connect_current_index nil)

(def ^:dynamic strong_connect_w nil)

(def ^:dynamic tarjan_components nil)

(def ^:dynamic tarjan_i nil)

(def ^:dynamic tarjan_index_of nil)

(def ^:dynamic tarjan_lowlink_of nil)

(def ^:dynamic tarjan_n nil)

(def ^:dynamic tarjan_on_stack nil)

(def ^:dynamic tarjan_stack nil)

(def ^:dynamic tarjan_v nil)

(defn strong_connect [tarjan_g strong_connect_v strong_connect_index]
  (binding [strong_connect_component nil strong_connect_current_index nil strong_connect_w nil] (try (do (alter-var-root (var tarjan_index_of) (fn [_] (assoc tarjan_index_of strong_connect_v strong_connect_index))) (alter-var-root (var tarjan_lowlink_of) (fn [_] (assoc tarjan_lowlink_of strong_connect_v strong_connect_index))) (set! strong_connect_current_index (+ strong_connect_index 1)) (alter-var-root (var tarjan_stack) (fn [_] (conj tarjan_stack strong_connect_v))) (alter-var-root (var tarjan_on_stack) (fn [_] (assoc tarjan_on_stack strong_connect_v true))) (doseq [w (nth tarjan_g strong_connect_v)] (if (= (nth tarjan_index_of w) (- 0 1)) (do (set! strong_connect_current_index (strong_connect w strong_connect_current_index)) (when (< (nth tarjan_lowlink_of w) (nth tarjan_lowlink_of strong_connect_v)) (alter-var-root (var tarjan_lowlink_of) (fn [_] (assoc tarjan_lowlink_of strong_connect_v (nth tarjan_lowlink_of w)))))) (when (nth tarjan_on_stack w) (when (< (nth tarjan_lowlink_of w) (nth tarjan_lowlink_of strong_connect_v)) (alter-var-root (var tarjan_lowlink_of) (fn [_] (assoc tarjan_lowlink_of strong_connect_v (nth tarjan_lowlink_of w)))))))) (when (= (nth tarjan_lowlink_of strong_connect_v) (nth tarjan_index_of strong_connect_v)) (do (set! strong_connect_component []) (set! strong_connect_w (nth tarjan_stack (- (count tarjan_stack) 1))) (alter-var-root (var tarjan_stack) (fn [_] (subvec tarjan_stack 0 (- (count tarjan_stack) 1)))) (alter-var-root (var tarjan_on_stack) (fn [_] (assoc tarjan_on_stack strong_connect_w false))) (set! strong_connect_component (conj strong_connect_component strong_connect_w)) (while (not= strong_connect_w strong_connect_v) (do (set! strong_connect_w (nth tarjan_stack (- (count tarjan_stack) 1))) (alter-var-root (var tarjan_stack) (fn [_] (subvec tarjan_stack 0 (- (count tarjan_stack) 1)))) (alter-var-root (var tarjan_on_stack) (fn [_] (assoc tarjan_on_stack strong_connect_w false))) (set! strong_connect_component (conj strong_connect_component strong_connect_w)))) (alter-var-root (var tarjan_components) (fn [_] (conj tarjan_components strong_connect_component))))) (throw (ex-info "return" {:v strong_connect_current_index}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn tarjan [tarjan_g]
  (binding [tarjan_components nil tarjan_i nil tarjan_index_of nil tarjan_lowlink_of nil tarjan_n nil tarjan_on_stack nil tarjan_stack nil tarjan_v nil] (try (do (set! tarjan_n (count tarjan_g)) (set! tarjan_stack []) (set! tarjan_on_stack []) (set! tarjan_index_of []) (set! tarjan_lowlink_of []) (set! tarjan_i 0) (while (< tarjan_i tarjan_n) (do (set! tarjan_on_stack (conj tarjan_on_stack false)) (set! tarjan_index_of (conj tarjan_index_of (- 0 1))) (set! tarjan_lowlink_of (conj tarjan_lowlink_of (- 0 1))) (set! tarjan_i (+ tarjan_i 1)))) (set! tarjan_components []) (set! tarjan_v 0) (while (< tarjan_v tarjan_n) (do (when (= (nth tarjan_index_of tarjan_v) (- 0 1)) (strong_connect tarjan_g tarjan_v 0)) (set! tarjan_v (+ tarjan_v 1)))) (throw (ex-info "return" {:v tarjan_components}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn create_graph [create_graph_n create_graph_edges]
  (binding [create_graph_g nil create_graph_i nil create_graph_u nil create_graph_v nil] (try (do (set! create_graph_g []) (set! create_graph_i 0) (while (< create_graph_i create_graph_n) (do (set! create_graph_g (conj create_graph_g [])) (set! create_graph_i (+ create_graph_i 1)))) (doseq [e create_graph_edges] (do (set! create_graph_u (nth e 0)) (set! create_graph_v (nth e 1)) (set! create_graph_g (assoc create_graph_g create_graph_u (conj (nth create_graph_g create_graph_u) create_graph_v))))) (throw (ex-info "return" {:v create_graph_g}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_edges nil main_g nil main_i nil main_n_vertices nil main_source nil main_target nil] (do (set! main_n_vertices 7) (set! main_source [0 0 1 2 3 3 4 4 6]) (set! main_target [1 3 2 0 1 4 5 6 5]) (set! main_edges []) (set! main_i 0) (while (< main_i (count main_source)) (do (set! main_edges (conj main_edges [(nth main_source main_i) (nth main_target main_i)])) (set! main_i (+ main_i 1)))) (set! main_g (create_graph main_n_vertices main_edges)) (println (str (tarjan main_g))))))

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
