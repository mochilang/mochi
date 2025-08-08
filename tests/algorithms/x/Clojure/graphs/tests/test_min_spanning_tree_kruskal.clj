(ns main (:refer-clojure :exclude [sort_edges find kruskal edges_equal main]))

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

(declare sort_edges find kruskal edges_equal main)

(def ^:dynamic edges_equal_e1 nil)

(def ^:dynamic edges_equal_e2 nil)

(def ^:dynamic edges_equal_i nil)

(def ^:dynamic find_r nil)

(def ^:dynamic kruskal_e nil)

(def ^:dynamic kruskal_edge nil)

(def ^:dynamic kruskal_i nil)

(def ^:dynamic kruskal_mst nil)

(def ^:dynamic kruskal_parent nil)

(def ^:dynamic kruskal_ru nil)

(def ^:dynamic kruskal_rv nil)

(def ^:dynamic kruskal_sorted nil)

(def ^:dynamic kruskal_u nil)

(def ^:dynamic kruskal_v nil)

(def ^:dynamic kruskal_w nil)

(def ^:dynamic main_edges nil)

(def ^:dynamic main_expected nil)

(def ^:dynamic main_num_nodes nil)

(def ^:dynamic main_result nil)

(def ^:dynamic main_sorted_expected nil)

(def ^:dynamic main_sorted_result nil)

(def ^:dynamic sort_edges_es nil)

(def ^:dynamic sort_edges_i nil)

(def ^:dynamic sort_edges_j nil)

(def ^:dynamic sort_edges_tmp nil)

(defn sort_edges [sort_edges_edges]
  (binding [sort_edges_es nil sort_edges_i nil sort_edges_j nil sort_edges_tmp nil] (try (do (set! sort_edges_es sort_edges_edges) (set! sort_edges_i 0) (while (< sort_edges_i (count sort_edges_es)) (do (set! sort_edges_j 0) (while (< sort_edges_j (- (- (count sort_edges_es) sort_edges_i) 1)) (do (when (> (nth (nth sort_edges_es sort_edges_j) 2) (nth (nth sort_edges_es (+ sort_edges_j 1)) 2)) (do (set! sort_edges_tmp (nth sort_edges_es sort_edges_j)) (set! sort_edges_es (assoc sort_edges_es sort_edges_j (nth sort_edges_es (+ sort_edges_j 1)))) (set! sort_edges_es (assoc sort_edges_es (+ sort_edges_j 1) sort_edges_tmp)))) (set! sort_edges_j (+ sort_edges_j 1)))) (set! sort_edges_i (+ sort_edges_i 1)))) (throw (ex-info "return" {:v sort_edges_es}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn find [find_parent find_x]
  (binding [find_r nil] (try (do (set! find_r find_x) (while (not= (nth find_parent find_r) find_r) (set! find_r (nth find_parent find_r))) (throw (ex-info "return" {:v find_r}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn kruskal [kruskal_n kruskal_edges]
  (binding [kruskal_e nil kruskal_edge nil kruskal_i nil kruskal_mst nil kruskal_parent nil kruskal_ru nil kruskal_rv nil kruskal_sorted nil kruskal_u nil kruskal_v nil kruskal_w nil] (try (do (set! kruskal_parent []) (set! kruskal_i 0) (while (< kruskal_i kruskal_n) (do (set! kruskal_parent (conj kruskal_parent kruskal_i)) (set! kruskal_i (+ kruskal_i 1)))) (set! kruskal_sorted (sort_edges kruskal_edges)) (set! kruskal_mst []) (set! kruskal_e 0) (loop [while_flag_1 true] (when (and while_flag_1 (< kruskal_e (count kruskal_sorted))) (cond (= (count kruskal_mst) (- kruskal_n 1)) (recur false) :else (do (set! kruskal_edge (nth kruskal_sorted kruskal_e)) (set! kruskal_e (+ kruskal_e 1)) (set! kruskal_u (nth kruskal_edge 0)) (set! kruskal_v (nth kruskal_edge 1)) (set! kruskal_w (nth kruskal_edge 2)) (set! kruskal_ru (find kruskal_parent kruskal_u)) (set! kruskal_rv (find kruskal_parent kruskal_v)) (when (not= kruskal_ru kruskal_rv) (do (set! kruskal_parent (assoc kruskal_parent kruskal_ru kruskal_rv)) (set! kruskal_mst (conj kruskal_mst [kruskal_u kruskal_v kruskal_w])))) (recur while_flag_1))))) (throw (ex-info "return" {:v kruskal_mst}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn edges_equal [edges_equal_a edges_equal_b]
  (binding [edges_equal_e1 nil edges_equal_e2 nil edges_equal_i nil] (try (do (when (not= (count edges_equal_a) (count edges_equal_b)) (throw (ex-info "return" {:v false}))) (set! edges_equal_i 0) (while (< edges_equal_i (count edges_equal_a)) (do (set! edges_equal_e1 (nth edges_equal_a edges_equal_i)) (set! edges_equal_e2 (nth edges_equal_b edges_equal_i)) (when (or (or (not= (nth edges_equal_e1 0) (nth edges_equal_e2 0)) (not= (nth edges_equal_e1 1) (nth edges_equal_e2 1))) (not= (nth edges_equal_e1 2) (nth edges_equal_e2 2))) (throw (ex-info "return" {:v false}))) (set! edges_equal_i (+ edges_equal_i 1)))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_edges nil main_expected nil main_num_nodes nil main_result nil main_sorted_expected nil main_sorted_result nil] (do (set! main_num_nodes 9) (set! main_edges [[0 1 4] [0 7 8] [1 2 8] [7 8 7] [7 6 1] [2 8 2] [8 6 6] [2 3 7] [2 5 4] [6 5 2] [3 5 14] [3 4 9] [5 4 10] [1 7 11]]) (set! main_expected [[7 6 1] [2 8 2] [6 5 2] [0 1 4] [2 5 4] [2 3 7] [0 7 8] [3 4 9]]) (set! main_result (kruskal main_num_nodes main_edges)) (set! main_sorted_result (sort_edges main_result)) (set! main_sorted_expected (sort_edges main_expected)) (println (str main_sorted_result)) (if (edges_equal main_sorted_expected main_sorted_result) (println true) (println false)))))

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
