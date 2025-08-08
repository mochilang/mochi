(ns main (:refer-clojure :exclude [list_to_string check_negative_cycle bellman_ford]))

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

(declare list_to_string check_negative_cycle bellman_ford)

(def ^:dynamic bellman_ford_distance nil)

(def ^:dynamic bellman_ford_e nil)

(def ^:dynamic bellman_ford_i nil)

(def ^:dynamic bellman_ford_j nil)

(def ^:dynamic bellman_ford_k nil)

(def ^:dynamic bellman_ford_u nil)

(def ^:dynamic bellman_ford_v nil)

(def ^:dynamic bellman_ford_w nil)

(def ^:dynamic check_negative_cycle_e nil)

(def ^:dynamic check_negative_cycle_j nil)

(def ^:dynamic check_negative_cycle_u nil)

(def ^:dynamic check_negative_cycle_v nil)

(def ^:dynamic check_negative_cycle_w nil)

(def ^:dynamic list_to_string_i nil)

(def ^:dynamic list_to_string_s nil)

(def ^:dynamic main_INF 1000000000.0)

(defn list_to_string [list_to_string_arr]
  (binding [list_to_string_i nil list_to_string_s nil] (try (do (set! list_to_string_s "[") (set! list_to_string_i 0) (while (< list_to_string_i (count list_to_string_arr)) (do (set! list_to_string_s (str list_to_string_s (str (nth list_to_string_arr list_to_string_i)))) (when (< list_to_string_i (- (count list_to_string_arr) 1)) (set! list_to_string_s (str list_to_string_s ", "))) (set! list_to_string_i (+ list_to_string_i 1)))) (throw (ex-info "return" {:v (str list_to_string_s "]")}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn check_negative_cycle [check_negative_cycle_graph check_negative_cycle_distance check_negative_cycle_edge_count]
  (binding [check_negative_cycle_e nil check_negative_cycle_j nil check_negative_cycle_u nil check_negative_cycle_v nil check_negative_cycle_w nil] (try (do (set! check_negative_cycle_j 0) (while (< check_negative_cycle_j check_negative_cycle_edge_count) (do (set! check_negative_cycle_e (nth check_negative_cycle_graph check_negative_cycle_j)) (set! check_negative_cycle_u (:src check_negative_cycle_e)) (set! check_negative_cycle_v (:dst check_negative_cycle_e)) (set! check_negative_cycle_w (double (:weight check_negative_cycle_e))) (when (and (< (nth check_negative_cycle_distance check_negative_cycle_u) main_INF) (< (+ (nth check_negative_cycle_distance check_negative_cycle_u) check_negative_cycle_w) (nth check_negative_cycle_distance check_negative_cycle_v))) (throw (ex-info "return" {:v true}))) (set! check_negative_cycle_j (+ check_negative_cycle_j 1)))) (throw (ex-info "return" {:v false}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn bellman_ford [bellman_ford_graph bellman_ford_vertex_count bellman_ford_edge_count bellman_ford_src]
  (binding [bellman_ford_distance nil bellman_ford_e nil bellman_ford_i nil bellman_ford_j nil bellman_ford_k nil bellman_ford_u nil bellman_ford_v nil bellman_ford_w nil] (try (do (set! bellman_ford_distance []) (set! bellman_ford_i 0) (while (< bellman_ford_i bellman_ford_vertex_count) (do (set! bellman_ford_distance (conj bellman_ford_distance main_INF)) (set! bellman_ford_i (+ bellman_ford_i 1)))) (set! bellman_ford_distance (assoc bellman_ford_distance bellman_ford_src 0.0)) (set! bellman_ford_k 0) (while (< bellman_ford_k (- bellman_ford_vertex_count 1)) (do (set! bellman_ford_j 0) (while (< bellman_ford_j bellman_ford_edge_count) (do (set! bellman_ford_e (nth bellman_ford_graph bellman_ford_j)) (set! bellman_ford_u (:src bellman_ford_e)) (set! bellman_ford_v (:dst bellman_ford_e)) (set! bellman_ford_w (double (:weight bellman_ford_e))) (when (and (< (nth bellman_ford_distance bellman_ford_u) main_INF) (< (+ (nth bellman_ford_distance bellman_ford_u) bellman_ford_w) (nth bellman_ford_distance bellman_ford_v))) (set! bellman_ford_distance (assoc bellman_ford_distance bellman_ford_v (+ (nth bellman_ford_distance bellman_ford_u) bellman_ford_w)))) (set! bellman_ford_j (+ bellman_ford_j 1)))) (set! bellman_ford_k (+ bellman_ford_k 1)))) (when (check_negative_cycle bellman_ford_graph bellman_ford_distance bellman_ford_edge_count) (throw (Exception. "Negative cycle found"))) (throw (ex-info "return" {:v bellman_ford_distance}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_edges [{:dst 1 :src 2 :weight (- 10)} {:dst 2 :src 3 :weight 3} {:dst 3 :src 0 :weight 5} {:dst 1 :src 0 :weight 4}])

(def ^:dynamic main_distances (bellman_ford main_edges 4 (count main_edges) 0))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (list_to_string main_distances))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
