(ns main (:refer-clojure :exclude [new_graph add_edge floyd_warshall show_min]))

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

(declare new_graph add_edge floyd_warshall show_min)

(def ^:dynamic add_edge_dp nil)

(def ^:dynamic add_edge_g nil)

(def ^:dynamic add_edge_row nil)

(def ^:dynamic floyd_warshall_alt nil)

(def ^:dynamic floyd_warshall_dp nil)

(def ^:dynamic floyd_warshall_g nil)

(def ^:dynamic floyd_warshall_i nil)

(def ^:dynamic floyd_warshall_j nil)

(def ^:dynamic floyd_warshall_k nil)

(def ^:dynamic floyd_warshall_row nil)

(def ^:dynamic new_graph_dp nil)

(def ^:dynamic new_graph_i nil)

(def ^:dynamic new_graph_j nil)

(def ^:dynamic new_graph_row nil)

(def ^:dynamic main_INF 1000000000)

(defn new_graph [new_graph_n]
  (binding [new_graph_dp nil new_graph_i nil new_graph_j nil new_graph_row nil] (try (do (set! new_graph_dp []) (set! new_graph_i 0) (while (< new_graph_i new_graph_n) (do (set! new_graph_row []) (set! new_graph_j 0) (while (< new_graph_j new_graph_n) (do (if (= new_graph_i new_graph_j) (set! new_graph_row (conj new_graph_row 0)) (set! new_graph_row (conj new_graph_row main_INF))) (set! new_graph_j (+ new_graph_j 1)))) (set! new_graph_dp (conj new_graph_dp new_graph_row)) (set! new_graph_i (+ new_graph_i 1)))) (throw (ex-info "return" {:v {:dp new_graph_dp :n new_graph_n}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn add_edge [add_edge_g_p add_edge_u add_edge_v add_edge_w]
  (binding [add_edge_dp nil add_edge_g nil add_edge_row nil] (do (set! add_edge_g add_edge_g_p) (set! add_edge_dp (:dp add_edge_g)) (set! add_edge_row (get add_edge_dp add_edge_u)) (set! add_edge_row (assoc add_edge_row add_edge_v add_edge_w)) (set! add_edge_dp (assoc add_edge_dp add_edge_u add_edge_row)) (set! add_edge_g (assoc add_edge_g :dp add_edge_dp)))))

(defn floyd_warshall [floyd_warshall_g_p]
  (binding [floyd_warshall_alt nil floyd_warshall_dp nil floyd_warshall_g nil floyd_warshall_i nil floyd_warshall_j nil floyd_warshall_k nil floyd_warshall_row nil] (do (set! floyd_warshall_g floyd_warshall_g_p) (set! floyd_warshall_dp (:dp floyd_warshall_g)) (set! floyd_warshall_k 0) (while (< floyd_warshall_k (:n floyd_warshall_g)) (do (set! floyd_warshall_i 0) (while (< floyd_warshall_i (:n floyd_warshall_g)) (do (set! floyd_warshall_j 0) (while (< floyd_warshall_j (:n floyd_warshall_g)) (do (set! floyd_warshall_alt (+ (nth (get floyd_warshall_dp floyd_warshall_i) floyd_warshall_k) (nth (get floyd_warshall_dp floyd_warshall_k) floyd_warshall_j))) (set! floyd_warshall_row (get floyd_warshall_dp floyd_warshall_i)) (when (< floyd_warshall_alt (nth floyd_warshall_row floyd_warshall_j)) (do (set! floyd_warshall_row (assoc floyd_warshall_row floyd_warshall_j floyd_warshall_alt)) (set! floyd_warshall_dp (assoc floyd_warshall_dp floyd_warshall_i floyd_warshall_row)))) (set! floyd_warshall_j (+ floyd_warshall_j 1)))) (set! floyd_warshall_i (+ floyd_warshall_i 1)))) (set! floyd_warshall_k (+ floyd_warshall_k 1)))) (set! floyd_warshall_g (assoc floyd_warshall_g :dp floyd_warshall_dp)))))

(defn show_min [show_min_g show_min_u show_min_v]
  (try (throw (ex-info "return" {:v (nth (get (:dp show_min_g) show_min_u) show_min_v)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(def ^:dynamic main_graph (new_graph 5))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (add_edge main_graph 0 2 9)
      (add_edge main_graph 0 4 10)
      (add_edge main_graph 1 3 5)
      (add_edge main_graph 2 3 7)
      (add_edge main_graph 3 0 10)
      (add_edge main_graph 3 1 2)
      (add_edge main_graph 3 2 1)
      (add_edge main_graph 3 4 6)
      (add_edge main_graph 4 1 3)
      (add_edge main_graph 4 2 4)
      (add_edge main_graph 4 3 9)
      (floyd_warshall main_graph)
      (println (str (show_min main_graph 1 4)))
      (println (str (show_min main_graph 0 3)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
