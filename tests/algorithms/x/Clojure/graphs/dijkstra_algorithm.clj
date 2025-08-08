(ns main (:refer-clojure :exclude [make_int_list make_bool_list dijkstra]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(defn indexOf [s sub]
  (let [idx (clojure.string/index-of s sub)] (if (nil? idx) -1 idx)))

(defn split [s sep]
  (clojure.string/split s (re-pattern sep)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare make_int_list make_bool_list dijkstra)

(def ^:dynamic count_v nil)

(def ^:dynamic dijkstra_dist nil)

(def ^:dynamic dijkstra_e nil)

(def ^:dynamic dijkstra_i nil)

(def ^:dynamic dijkstra_j nil)

(def ^:dynamic dijkstra_min_dist nil)

(def ^:dynamic dijkstra_n nil)

(def ^:dynamic dijkstra_new_dist nil)

(def ^:dynamic dijkstra_u nil)

(def ^:dynamic dijkstra_v nil)

(def ^:dynamic dijkstra_visited nil)

(def ^:dynamic dijkstra_w nil)

(def ^:dynamic make_bool_list_i nil)

(def ^:dynamic make_bool_list_lst nil)

(def ^:dynamic make_int_list_i nil)

(def ^:dynamic make_int_list_lst nil)

(defn make_int_list [make_int_list_n make_int_list_value]
  (binding [make_int_list_i nil make_int_list_lst nil] (try (do (set! make_int_list_lst []) (set! make_int_list_i 0) (while (< make_int_list_i make_int_list_n) (do (set! make_int_list_lst (conj make_int_list_lst make_int_list_value)) (set! make_int_list_i (+ make_int_list_i 1)))) (throw (ex-info "return" {:v make_int_list_lst}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn make_bool_list [make_bool_list_n]
  (binding [make_bool_list_i nil make_bool_list_lst nil] (try (do (set! make_bool_list_lst []) (set! make_bool_list_i 0) (while (< make_bool_list_i make_bool_list_n) (do (set! make_bool_list_lst (conj make_bool_list_lst false)) (set! make_bool_list_i (+ make_bool_list_i 1)))) (throw (ex-info "return" {:v make_bool_list_lst}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn dijkstra [dijkstra_graph dijkstra_src]
  (binding [count_v nil dijkstra_dist nil dijkstra_e nil dijkstra_i nil dijkstra_j nil dijkstra_min_dist nil dijkstra_n nil dijkstra_new_dist nil dijkstra_u nil dijkstra_v nil dijkstra_visited nil dijkstra_w nil] (try (do (set! dijkstra_n (count dijkstra_graph)) (set! dijkstra_dist (make_int_list dijkstra_n 1000000000)) (set! dijkstra_visited (make_bool_list dijkstra_n)) (set! dijkstra_dist (assoc dijkstra_dist dijkstra_src 0)) (set! count_v 0) (loop [while_flag_1 true] (when (and while_flag_1 (< count_v dijkstra_n)) (do (set! dijkstra_u (- 1)) (set! dijkstra_min_dist 1000000000) (set! dijkstra_i 0) (while (< dijkstra_i dijkstra_n) (do (when (and (not (nth dijkstra_visited dijkstra_i)) (< (nth dijkstra_dist dijkstra_i) dijkstra_min_dist)) (do (set! dijkstra_min_dist (nth dijkstra_dist dijkstra_i)) (set! dijkstra_u dijkstra_i))) (set! dijkstra_i (+ dijkstra_i 1)))) (cond (< dijkstra_u 0) (recur false) :else (do (set! dijkstra_visited (assoc dijkstra_visited dijkstra_u true)) (set! dijkstra_j 0) (while (< dijkstra_j (count (nth dijkstra_graph dijkstra_u))) (do (set! dijkstra_e (nth (nth dijkstra_graph dijkstra_u) dijkstra_j)) (set! dijkstra_v (:node dijkstra_e)) (set! dijkstra_w (:weight dijkstra_e)) (when (not (nth dijkstra_visited dijkstra_v)) (do (set! dijkstra_new_dist (+ (nth dijkstra_dist dijkstra_u) dijkstra_w)) (when (< dijkstra_new_dist (nth dijkstra_dist dijkstra_v)) (set! dijkstra_dist (assoc dijkstra_dist dijkstra_v dijkstra_new_dist))))) (set! dijkstra_j (+ dijkstra_j 1)))) (set! count_v (+ count_v 1)) (recur while_flag_1)))))) (throw (ex-info "return" {:v dijkstra_dist}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_graph [[{:node 1 :weight 10} {:node 3 :weight 5}] [{:node 2 :weight 1} {:node 3 :weight 2}] [{:node 4 :weight 4}] [{:node 1 :weight 3} {:node 2 :weight 9} {:node 4 :weight 2}] [{:node 0 :weight 7} {:node 2 :weight 6}]])

(def ^:dynamic main_dist (dijkstra main_graph 0))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (nth main_dist 0)))
      (println (str (nth main_dist 1)))
      (println (str (nth main_dist 2)))
      (println (str (nth main_dist 3)))
      (println (str (nth main_dist 4)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
