(ns main (:refer-clojure :exclude [pairs_to_string prim_mst]))

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

(declare pairs_to_string prim_mst)

(def ^:dynamic count_v nil)

(def ^:dynamic pairs_to_string_e nil)

(def ^:dynamic pairs_to_string_i nil)

(def ^:dynamic pairs_to_string_s nil)

(def ^:dynamic prim_mst_dist nil)

(def ^:dynamic prim_mst_i nil)

(def ^:dynamic prim_mst_min_val nil)

(def ^:dynamic prim_mst_n nil)

(def ^:dynamic prim_mst_parent nil)

(def ^:dynamic prim_mst_result nil)

(def ^:dynamic prim_mst_u nil)

(def ^:dynamic prim_mst_v nil)

(def ^:dynamic prim_mst_visited nil)

(def ^:dynamic main_INF 1000000000)

(defn pairs_to_string [pairs_to_string_edges]
  (binding [pairs_to_string_e nil pairs_to_string_i nil pairs_to_string_s nil] (try (do (set! pairs_to_string_s "[") (set! pairs_to_string_i 0) (while (< pairs_to_string_i (count pairs_to_string_edges)) (do (set! pairs_to_string_e (nth pairs_to_string_edges pairs_to_string_i)) (set! pairs_to_string_s (str (str (str (str (str pairs_to_string_s "(") (str (:u pairs_to_string_e))) ", ") (str (:v pairs_to_string_e))) ")")) (when (< pairs_to_string_i (- (count pairs_to_string_edges) 1)) (set! pairs_to_string_s (str pairs_to_string_s ", "))) (set! pairs_to_string_i (+ pairs_to_string_i 1)))) (throw (ex-info "return" {:v (str pairs_to_string_s "]")}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn prim_mst [prim_mst_graph]
  (binding [count_v nil prim_mst_dist nil prim_mst_i nil prim_mst_min_val nil prim_mst_n nil prim_mst_parent nil prim_mst_result nil prim_mst_u nil prim_mst_v nil prim_mst_visited nil] (try (do (set! prim_mst_n (count prim_mst_graph)) (set! prim_mst_visited []) (set! prim_mst_dist []) (set! prim_mst_parent []) (set! prim_mst_i 0) (while (< prim_mst_i prim_mst_n) (do (set! prim_mst_visited (conj prim_mst_visited false)) (set! prim_mst_dist (conj prim_mst_dist main_INF)) (set! prim_mst_parent (conj prim_mst_parent (- 1))) (set! prim_mst_i (+ prim_mst_i 1)))) (set! prim_mst_dist (assoc prim_mst_dist 0 0)) (set! prim_mst_result []) (set! count_v 0) (loop [while_flag_1 true] (when (and while_flag_1 (< count_v prim_mst_n)) (do (set! prim_mst_min_val main_INF) (set! prim_mst_u 0) (set! prim_mst_v 0) (while (< prim_mst_v prim_mst_n) (do (when (and (= (nth prim_mst_visited prim_mst_v) false) (< (nth prim_mst_dist prim_mst_v) prim_mst_min_val)) (do (set! prim_mst_min_val (nth prim_mst_dist prim_mst_v)) (set! prim_mst_u prim_mst_v))) (set! prim_mst_v (+ prim_mst_v 1)))) (cond (= prim_mst_min_val main_INF) (recur false) :else (do (set! prim_mst_visited (assoc prim_mst_visited prim_mst_u true)) (when (not= prim_mst_u 0) (set! prim_mst_result (conj prim_mst_result {:u (nth prim_mst_parent prim_mst_u) :v prim_mst_u}))) (doseq [e (nth prim_mst_graph prim_mst_u)] (when (and (= (nth prim_mst_visited (:to e)) false) (< (:weight e) (nth prim_mst_dist (:to e)))) (do (set! prim_mst_dist (assoc prim_mst_dist (:to e) (:weight e))) (set! prim_mst_parent (assoc prim_mst_parent (:to e) prim_mst_u))))) (set! count_v (+ count_v 1)) (recur while_flag_1)))))) (throw (ex-info "return" {:v prim_mst_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_adjacency_list [[{:to 1 :weight 1} {:to 3 :weight 3}] [{:to 0 :weight 1} {:to 2 :weight 6} {:to 3 :weight 5} {:to 4 :weight 1}] [{:to 1 :weight 6} {:to 4 :weight 5} {:to 5 :weight 2}] [{:to 0 :weight 3} {:to 1 :weight 5} {:to 4 :weight 1}] [{:to 1 :weight 1} {:to 2 :weight 5} {:to 3 :weight 1} {:to 5 :weight 4}] [{:to 2 :weight 2} {:to 4 :weight 4}]])

(def ^:dynamic main_mst_edges (prim_mst main_adjacency_list))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (pairs_to_string main_mst_edges))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
