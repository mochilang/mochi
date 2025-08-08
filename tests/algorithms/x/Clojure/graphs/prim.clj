(ns main (:refer-clojure :exclude [connect in_list prim sort_heap prim_heap print_edges test_vector]))

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

(declare connect in_list prim sort_heap prim_heap print_edges test_vector)

(def ^:dynamic connect_g nil)

(def ^:dynamic connect_u nil)

(def ^:dynamic connect_v nil)

(def ^:dynamic in_list_i nil)

(def ^:dynamic prim_cur nil)

(def ^:dynamic prim_d nil)

(def ^:dynamic prim_dist nil)

(def ^:dynamic prim_edges nil)

(def ^:dynamic prim_heap_cur nil)

(def ^:dynamic prim_heap_dist nil)

(def ^:dynamic prim_heap_edges nil)

(def ^:dynamic prim_heap_h nil)

(def ^:dynamic prim_heap_i nil)

(def ^:dynamic prim_heap_j nil)

(def ^:dynamic prim_heap_known nil)

(def ^:dynamic prim_heap_parent nil)

(def ^:dynamic prim_heap_u nil)

(def ^:dynamic prim_heap_v nil)

(def ^:dynamic prim_heap_w nil)

(def ^:dynamic prim_i nil)

(def ^:dynamic prim_j nil)

(def ^:dynamic prim_k nil)

(def ^:dynamic prim_keys nil)

(def ^:dynamic prim_known nil)

(def ^:dynamic prim_mini nil)

(def ^:dynamic prim_parent nil)

(def ^:dynamic prim_u nil)

(def ^:dynamic prim_v nil)

(def ^:dynamic prim_w nil)

(def ^:dynamic print_edges_e nil)

(def ^:dynamic print_edges_i nil)

(def ^:dynamic sort_heap_a nil)

(def ^:dynamic sort_heap_dj nil)

(def ^:dynamic sort_heap_dj1 nil)

(def ^:dynamic sort_heap_i nil)

(def ^:dynamic sort_heap_j nil)

(def ^:dynamic sort_heap_t nil)

(def ^:dynamic test_vector_G nil)

(def ^:dynamic test_vector_i nil)

(def ^:dynamic test_vector_mst nil)

(def ^:dynamic test_vector_mst_heap nil)

(def ^:dynamic test_vector_x nil)

(def ^:dynamic main_INF 1000000000)

(defn connect [connect_graph connect_a connect_b connect_w]
  (binding [connect_g nil connect_u nil connect_v nil] (try (do (set! connect_u (- connect_a 1)) (set! connect_v (- connect_b 1)) (set! connect_g connect_graph) (set! connect_g (assoc connect_g connect_u (conj (get connect_g connect_u) [connect_v connect_w]))) (set! connect_g (assoc connect_g connect_v (conj (get connect_g connect_v) [connect_u connect_w]))) (throw (ex-info "return" {:v connect_g}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn in_list [in_list_arr in_list_x]
  (binding [in_list_i nil] (try (do (set! in_list_i 0) (while (< in_list_i (count in_list_arr)) (do (when (= (nth in_list_arr in_list_i) in_list_x) (throw (ex-info "return" {:v true}))) (set! in_list_i (+ in_list_i 1)))) (throw (ex-info "return" {:v false}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn prim [prim_graph prim_s prim_n]
  (binding [prim_cur nil prim_d nil prim_dist nil prim_edges nil prim_i nil prim_j nil prim_k nil prim_keys nil prim_known nil prim_mini nil prim_parent nil prim_u nil prim_v nil prim_w nil] (try (do (set! prim_dist {}) (set! prim_parent {}) (set! prim_dist (assoc prim_dist prim_s 0)) (set! prim_parent (assoc prim_parent prim_s (- 1))) (set! prim_known []) (set! prim_keys [prim_s]) (while (< (count prim_known) prim_n) (do (set! prim_mini main_INF) (set! prim_u (- 1)) (set! prim_i 0) (while (< prim_i (count prim_keys)) (do (set! prim_k (nth prim_keys prim_i)) (set! prim_d (get prim_dist prim_k)) (when (and (not (in_list prim_known prim_k)) (< prim_d prim_mini)) (do (set! prim_mini prim_d) (set! prim_u prim_k))) (set! prim_i (+ prim_i 1)))) (set! prim_known (conj prim_known prim_u)) (doseq [e (get prim_graph prim_u)] (do (set! prim_v (nth e 0)) (set! prim_w (nth e 1)) (when (not (in_list prim_keys prim_v)) (set! prim_keys (conj prim_keys prim_v))) (set! prim_cur (if (in prim_v prim_dist) (get prim_dist prim_v) main_INF)) (when (and (not (in_list prim_known prim_v)) (< prim_w prim_cur)) (do (set! prim_dist (assoc prim_dist prim_v prim_w)) (set! prim_parent (assoc prim_parent prim_v prim_u)))))))) (set! prim_edges []) (set! prim_j 0) (while (< prim_j (count prim_keys)) (do (set! prim_v (nth prim_keys prim_j)) (when (not= prim_v prim_s) (set! prim_edges (conj prim_edges [(+ prim_v 1) (+ (get prim_parent prim_v) 1)]))) (set! prim_j (+ prim_j 1)))) (throw (ex-info "return" {:v prim_edges}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn sort_heap [sort_heap_h sort_heap_dist]
  (binding [sort_heap_a nil sort_heap_dj nil sort_heap_dj1 nil sort_heap_i nil sort_heap_j nil sort_heap_t nil] (try (do (set! sort_heap_a sort_heap_h) (set! sort_heap_i 0) (while (< sort_heap_i (count sort_heap_a)) (do (set! sort_heap_j 0) (while (< sort_heap_j (- (- (count sort_heap_a) sort_heap_i) 1)) (do (set! sort_heap_dj (if (in (nth sort_heap_a sort_heap_j) sort_heap_dist) (get sort_heap_dist (nth sort_heap_a sort_heap_j)) main_INF)) (set! sort_heap_dj1 (if (in (nth sort_heap_a (+ sort_heap_j 1)) sort_heap_dist) (get sort_heap_dist (nth sort_heap_a (+ sort_heap_j 1))) main_INF)) (when (> sort_heap_dj sort_heap_dj1) (do (set! sort_heap_t (nth sort_heap_a sort_heap_j)) (set! sort_heap_a (assoc sort_heap_a sort_heap_j (nth sort_heap_a (+ sort_heap_j 1)))) (set! sort_heap_a (assoc sort_heap_a (+ sort_heap_j 1) sort_heap_t)))) (set! sort_heap_j (+ sort_heap_j 1)))) (set! sort_heap_i (+ sort_heap_i 1)))) (throw (ex-info "return" {:v sort_heap_a}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn prim_heap [prim_heap_graph prim_heap_s prim_heap_n]
  (binding [prim_heap_cur nil prim_heap_dist nil prim_heap_edges nil prim_heap_h nil prim_heap_i nil prim_heap_j nil prim_heap_known nil prim_heap_parent nil prim_heap_u nil prim_heap_v nil prim_heap_w nil] (try (do (set! prim_heap_dist {}) (set! prim_heap_parent {}) (set! prim_heap_dist (assoc prim_heap_dist prim_heap_s 0)) (set! prim_heap_parent (assoc prim_heap_parent prim_heap_s (- 1))) (set! prim_heap_h []) (set! prim_heap_i 0) (while (< prim_heap_i prim_heap_n) (do (set! prim_heap_h (conj prim_heap_h prim_heap_i)) (set! prim_heap_i (+ prim_heap_i 1)))) (set! prim_heap_h (sort_heap prim_heap_h prim_heap_dist)) (set! prim_heap_known []) (while (> (count prim_heap_h) 0) (do (set! prim_heap_u (nth prim_heap_h 0)) (set! prim_heap_h (subvec prim_heap_h 1 (min (count prim_heap_h) (count prim_heap_h)))) (set! prim_heap_known (conj prim_heap_known prim_heap_u)) (doseq [e (get prim_heap_graph prim_heap_u)] (do (set! prim_heap_v (nth e 0)) (set! prim_heap_w (nth e 1)) (set! prim_heap_cur (if (in prim_heap_v prim_heap_dist) (get prim_heap_dist prim_heap_v) main_INF)) (when (and (not (in_list prim_heap_known prim_heap_v)) (< prim_heap_w prim_heap_cur)) (do (set! prim_heap_dist (assoc prim_heap_dist prim_heap_v prim_heap_w)) (set! prim_heap_parent (assoc prim_heap_parent prim_heap_v prim_heap_u)))))) (set! prim_heap_h (sort_heap prim_heap_h prim_heap_dist)))) (set! prim_heap_edges []) (set! prim_heap_j 0) (while (< prim_heap_j prim_heap_n) (do (when (not= prim_heap_j prim_heap_s) (set! prim_heap_edges (conj prim_heap_edges [(+ prim_heap_j 1) (+ (get prim_heap_parent prim_heap_j) 1)]))) (set! prim_heap_j (+ prim_heap_j 1)))) (throw (ex-info "return" {:v prim_heap_edges}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn print_edges [print_edges_edges]
  (binding [print_edges_e nil print_edges_i nil] (do (set! print_edges_i 0) (while (< print_edges_i (count print_edges_edges)) (do (set! print_edges_e (nth print_edges_edges print_edges_i)) (println (str (str (str (str "(" (str (nth print_edges_e 0))) ", ") (str (nth print_edges_e 1))) ")")) (set! print_edges_i (+ print_edges_i 1)))) print_edges_edges)))

(defn test_vector []
  (binding [test_vector_G nil test_vector_i nil test_vector_mst nil test_vector_mst_heap nil test_vector_x nil] (do (set! test_vector_x 5) (set! test_vector_G {}) (set! test_vector_i 0) (while (< test_vector_i test_vector_x) (do (set! test_vector_G (assoc test_vector_G test_vector_i [])) (set! test_vector_i (+ test_vector_i 1)))) (set! test_vector_G (connect test_vector_G 1 2 15)) (set! test_vector_G (connect test_vector_G 1 3 12)) (set! test_vector_G (connect test_vector_G 2 4 13)) (set! test_vector_G (connect test_vector_G 2 5 5)) (set! test_vector_G (connect test_vector_G 3 2 6)) (set! test_vector_G (connect test_vector_G 3 4 6)) (set! test_vector_mst (prim test_vector_G 0 test_vector_x)) (set! test_vector_mst (print_edges test_vector_mst)) (set! test_vector_mst_heap (prim_heap test_vector_G 0 test_vector_x)) (set! test_vector_mst_heap (print_edges test_vector_mst_heap)))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (test_vector)
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
