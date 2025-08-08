(ns main (:refer-clojure :exclude [sort_edges find_parent kruskal edges_to_string]))

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

(declare sort_edges find_parent kruskal edges_to_string)

(def ^:dynamic edges_to_string_e nil)

(def ^:dynamic edges_to_string_i nil)

(def ^:dynamic edges_to_string_s nil)

(def ^:dynamic find_parent_parent nil)

(def ^:dynamic kruskal_e nil)

(def ^:dynamic kruskal_es nil)

(def ^:dynamic kruskal_i nil)

(def ^:dynamic kruskal_idx nil)

(def ^:dynamic kruskal_mst nil)

(def ^:dynamic kruskal_pa nil)

(def ^:dynamic kruskal_parent nil)

(def ^:dynamic kruskal_pb nil)

(def ^:dynamic sort_edges_es nil)

(def ^:dynamic sort_edges_i nil)

(def ^:dynamic sort_edges_j nil)

(def ^:dynamic sort_edges_temp nil)

(defn sort_edges [sort_edges_edges]
  (binding [sort_edges_es nil sort_edges_i nil sort_edges_j nil sort_edges_temp nil] (try (do (set! sort_edges_es sort_edges_edges) (set! sort_edges_i 0) (while (< sort_edges_i (count sort_edges_es)) (do (set! sort_edges_j 0) (while (< sort_edges_j (- (- (count sort_edges_es) sort_edges_i) 1)) (do (when (> (nth (nth sort_edges_es sort_edges_j) 2) (nth (nth sort_edges_es (+ sort_edges_j 1)) 2)) (do (set! sort_edges_temp (nth sort_edges_es sort_edges_j)) (set! sort_edges_es (assoc sort_edges_es sort_edges_j (nth sort_edges_es (+ sort_edges_j 1)))) (set! sort_edges_es (assoc sort_edges_es (+ sort_edges_j 1) sort_edges_temp)))) (set! sort_edges_j (+ sort_edges_j 1)))) (set! sort_edges_i (+ sort_edges_i 1)))) (throw (ex-info "return" {:v sort_edges_es}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn find_parent [find_parent_parent_p find_parent_i]
  (binding [find_parent_parent nil] (try (do (set! find_parent_parent find_parent_parent_p) (when (not= (nth find_parent_parent find_parent_i) find_parent_i) (set! find_parent_parent (assoc find_parent_parent find_parent_i (find_parent find_parent_parent (nth find_parent_parent find_parent_i))))) (throw (ex-info "return" {:v (nth find_parent_parent find_parent_i)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn kruskal [kruskal_num_nodes kruskal_edges]
  (binding [kruskal_e nil kruskal_es nil kruskal_i nil kruskal_idx nil kruskal_mst nil kruskal_pa nil kruskal_parent nil kruskal_pb nil] (try (do (set! kruskal_es (sort_edges kruskal_edges)) (set! kruskal_parent []) (set! kruskal_i 0) (while (< kruskal_i kruskal_num_nodes) (do (set! kruskal_parent (conj kruskal_parent kruskal_i)) (set! kruskal_i (+ kruskal_i 1)))) (set! kruskal_mst []) (set! kruskal_idx 0) (while (< kruskal_idx (count kruskal_es)) (do (set! kruskal_e (nth kruskal_es kruskal_idx)) (set! kruskal_pa (find_parent kruskal_parent (nth kruskal_e 0))) (set! kruskal_pb (find_parent kruskal_parent (nth kruskal_e 1))) (when (not= kruskal_pa kruskal_pb) (do (set! kruskal_mst (conj kruskal_mst kruskal_e)) (set! kruskal_parent (assoc kruskal_parent kruskal_pa kruskal_pb)))) (set! kruskal_idx (+ kruskal_idx 1)))) (throw (ex-info "return" {:v kruskal_mst}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn edges_to_string [edges_to_string_es]
  (binding [edges_to_string_e nil edges_to_string_i nil edges_to_string_s nil] (try (do (set! edges_to_string_s "[") (set! edges_to_string_i 0) (while (< edges_to_string_i (count edges_to_string_es)) (do (set! edges_to_string_e (nth edges_to_string_es edges_to_string_i)) (set! edges_to_string_s (str (str (str (str (str (str (str edges_to_string_s "(") (str (nth edges_to_string_e 0))) ", ") (str (nth edges_to_string_e 1))) ", ") (str (nth edges_to_string_e 2))) ")")) (when (< edges_to_string_i (- (count edges_to_string_es) 1)) (set! edges_to_string_s (str edges_to_string_s ", "))) (set! edges_to_string_i (+ edges_to_string_i 1)))) (set! edges_to_string_s (str edges_to_string_s "]")) (throw (ex-info "return" {:v edges_to_string_s}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_edges1 [[0 1 3] [1 2 5] [2 3 1]])

(def ^:dynamic main_edges2 [[0 1 3] [1 2 5] [2 3 1] [0 2 1] [0 3 2]])

(def ^:dynamic main_edges3 [[0 1 3] [1 2 5] [2 3 1] [0 2 1] [0 3 2] [2 1 1]])

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (edges_to_string (kruskal 4 main_edges1)))
      (println (edges_to_string (kruskal 4 main_edges2)))
      (println (edges_to_string (kruskal 4 main_edges3)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
