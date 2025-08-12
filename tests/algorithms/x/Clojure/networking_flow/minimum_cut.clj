(ns main (:refer-clojure :exclude [bfs mincut]))

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

(defn _fetch [url]
  {:data [{:from "" :intensity {:actual 0 :forecast 0 :index ""} :to ""}]})

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare bfs mincut)

(def ^:dynamic bfs_head nil)

(def ^:dynamic bfs_i nil)

(def ^:dynamic bfs_ind nil)

(def ^:dynamic bfs_parent nil)

(def ^:dynamic bfs_queue nil)

(def ^:dynamic bfs_u nil)

(def ^:dynamic bfs_visited nil)

(def ^:dynamic mincut_cap nil)

(def ^:dynamic mincut_g nil)

(def ^:dynamic mincut_i nil)

(def ^:dynamic mincut_j nil)

(def ^:dynamic mincut_p nil)

(def ^:dynamic mincut_parent nil)

(def ^:dynamic mincut_path_flow nil)

(def ^:dynamic mincut_res nil)

(def ^:dynamic mincut_row nil)

(def ^:dynamic mincut_s nil)

(def ^:dynamic mincut_temp nil)

(def ^:dynamic mincut_u nil)

(def ^:dynamic mincut_v nil)

(defn bfs [bfs_graph bfs_s bfs_t bfs_parent_p]
  (binding [bfs_head nil bfs_i nil bfs_ind nil bfs_parent nil bfs_queue nil bfs_u nil bfs_visited nil] (try (do (set! bfs_parent bfs_parent_p) (set! bfs_visited []) (set! bfs_i 0) (while (< bfs_i (count bfs_graph)) (do (set! bfs_visited (conj bfs_visited false)) (set! bfs_i (+ bfs_i 1)))) (set! bfs_queue [bfs_s]) (set! bfs_head 0) (set! bfs_visited (assoc bfs_visited bfs_s true)) (while (< bfs_head (count bfs_queue)) (do (set! bfs_u (nth bfs_queue bfs_head)) (set! bfs_head (+ bfs_head 1)) (set! bfs_ind 0) (while (< bfs_ind (count (nth bfs_graph bfs_u))) (do (when (and (= (nth bfs_visited bfs_ind) false) (> (nth (nth bfs_graph bfs_u) bfs_ind) 0)) (do (set! bfs_queue (conj bfs_queue bfs_ind)) (set! bfs_visited (assoc bfs_visited bfs_ind true)) (set! bfs_parent (assoc bfs_parent bfs_ind bfs_u)))) (set! bfs_ind (+ bfs_ind 1)))))) (throw (ex-info "return" {:v (nth bfs_visited bfs_t)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn mincut [mincut_graph mincut_source mincut_sink]
  (binding [mincut_cap nil mincut_g nil mincut_i nil mincut_j nil mincut_p nil mincut_parent nil mincut_path_flow nil mincut_res nil mincut_row nil mincut_s nil mincut_temp nil mincut_u nil mincut_v nil] (try (do (set! mincut_g mincut_graph) (set! mincut_parent []) (set! mincut_i 0) (while (< mincut_i (count mincut_g)) (do (set! mincut_parent (conj mincut_parent (- 1))) (set! mincut_i (+ mincut_i 1)))) (set! mincut_temp []) (set! mincut_i 0) (while (< mincut_i (count mincut_g)) (do (set! mincut_row []) (set! mincut_j 0) (while (< mincut_j (count (nth mincut_g mincut_i))) (do (set! mincut_row (conj mincut_row (nth (nth mincut_g mincut_i) mincut_j))) (set! mincut_j (+ mincut_j 1)))) (set! mincut_temp (conj mincut_temp mincut_row)) (set! mincut_i (+ mincut_i 1)))) (while (bfs mincut_g mincut_source mincut_sink mincut_parent) (do (set! mincut_path_flow 1000000000) (set! mincut_s mincut_sink) (while (not= mincut_s mincut_source) (do (set! mincut_p (nth mincut_parent mincut_s)) (set! mincut_cap (nth (nth mincut_g mincut_p) mincut_s)) (when (< mincut_cap mincut_path_flow) (set! mincut_path_flow mincut_cap)) (set! mincut_s mincut_p))) (set! mincut_v mincut_sink) (while (not= mincut_v mincut_source) (do (set! mincut_u (nth mincut_parent mincut_v)) (set! mincut_g (assoc-in mincut_g [mincut_u mincut_v] (- (nth (nth mincut_g mincut_u) mincut_v) mincut_path_flow))) (set! mincut_g (assoc-in mincut_g [mincut_v mincut_u] (+ (nth (nth mincut_g mincut_v) mincut_u) mincut_path_flow))) (set! mincut_v mincut_u))))) (set! mincut_res []) (set! mincut_i 0) (while (< mincut_i (count mincut_g)) (do (set! mincut_j 0) (while (< mincut_j (count (nth mincut_g 0))) (do (when (and (= (nth (nth mincut_g mincut_i) mincut_j) 0) (> (nth (nth mincut_temp mincut_i) mincut_j) 0)) (set! mincut_res (conj mincut_res [mincut_i mincut_j]))) (set! mincut_j (+ mincut_j 1)))) (set! mincut_i (+ mincut_i 1)))) (throw (ex-info "return" {:v mincut_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_test_graph [[0 16 13 0 0 0] [0 0 10 12 0 0] [0 4 0 0 14 0] [0 0 9 0 0 20] [0 0 0 7 0 4] [0 0 0 0 0 0]])

(def ^:dynamic main_result (mincut main_test_graph 0 5))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str main_result))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
