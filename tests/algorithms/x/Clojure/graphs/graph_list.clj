(ns main (:refer-clojure :exclude [make_graph contains_vertex add_edge graph_to_string]))

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

(declare make_graph contains_vertex add_edge graph_to_string)

(def ^:dynamic add_edge_adj nil)

(def ^:dynamic add_edge_g nil)

(def ^:dynamic main_d_graph nil)

(def ^:dynamic make_graph_m nil)

(defn make_graph [make_graph_directed]
  (binding [make_graph_m nil] (try (do (set! make_graph_m {}) (throw (ex-info "return" {:v {:adj_list make_graph_m :directed make_graph_directed}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn contains_vertex [contains_vertex_m contains_vertex_v]
  (try (throw (ex-info "return" {:v (in contains_vertex_v contains_vertex_m)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn add_edge [add_edge_g_p add_edge_s add_edge_d]
  (binding [add_edge_adj nil add_edge_g nil] (try (do (set! add_edge_g add_edge_g_p) (set! add_edge_adj (:adj_list add_edge_g)) (if (not (:directed add_edge_g)) (if (and (contains_vertex add_edge_adj add_edge_s) (contains_vertex add_edge_adj add_edge_d)) (do (set! add_edge_adj (assoc add_edge_adj add_edge_s (conj (get add_edge_adj add_edge_s) add_edge_d))) (set! add_edge_adj (assoc add_edge_adj add_edge_d (conj (get add_edge_adj add_edge_d) add_edge_s)))) (if (contains_vertex add_edge_adj add_edge_s) (do (set! add_edge_adj (assoc add_edge_adj add_edge_s (conj (get add_edge_adj add_edge_s) add_edge_d))) (set! add_edge_adj (assoc add_edge_adj add_edge_d [add_edge_s]))) (if (contains_vertex add_edge_adj add_edge_d) (do (set! add_edge_adj (assoc add_edge_adj add_edge_d (conj (get add_edge_adj add_edge_d) add_edge_s))) (set! add_edge_adj (assoc add_edge_adj add_edge_s [add_edge_d]))) (do (set! add_edge_adj (assoc add_edge_adj add_edge_s [add_edge_d])) (set! add_edge_adj (assoc add_edge_adj add_edge_d [add_edge_s])))))) (if (and (contains_vertex add_edge_adj add_edge_s) (contains_vertex add_edge_adj add_edge_d)) (set! add_edge_adj (assoc add_edge_adj add_edge_s (conj (get add_edge_adj add_edge_s) add_edge_d))) (if (contains_vertex add_edge_adj add_edge_s) (do (set! add_edge_adj (assoc add_edge_adj add_edge_s (conj (get add_edge_adj add_edge_s) add_edge_d))) (set! add_edge_adj (assoc add_edge_adj add_edge_d []))) (if (contains_vertex add_edge_adj add_edge_d) (set! add_edge_adj (assoc add_edge_adj add_edge_s [add_edge_d])) (do (set! add_edge_adj (assoc add_edge_adj add_edge_s [add_edge_d])) (set! add_edge_adj (assoc add_edge_adj add_edge_d []))))))) (set! add_edge_g (assoc add_edge_g :adj_list add_edge_adj)) (throw (ex-info "return" {:v add_edge_g}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn graph_to_string [graph_to_string_g]
  (try (throw (ex-info "return" {:v (str (:adj_list graph_to_string_g))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(def ^:dynamic main_d_graph (make_graph true))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (def main_d_graph (add_edge main_d_graph (str 0) (str 1)))
      (println (graph_to_string main_d_graph))
      (def main_d_graph (add_edge main_d_graph (str 1) (str 2)))
      (def main_d_graph (add_edge main_d_graph (str 1) (str 4)))
      (def main_d_graph (add_edge main_d_graph (str 1) (str 5)))
      (println (graph_to_string main_d_graph))
      (def main_d_graph (add_edge main_d_graph (str 2) (str 0)))
      (def main_d_graph (add_edge main_d_graph (str 2) (str 6)))
      (def main_d_graph (add_edge main_d_graph (str 2) (str 7)))
      (println (graph_to_string main_d_graph))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
