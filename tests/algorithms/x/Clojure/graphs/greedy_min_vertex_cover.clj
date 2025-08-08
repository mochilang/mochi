(ns main (:refer-clojure :exclude [remove_value greedy_min_vertex_cover]))

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

(declare remove_value greedy_min_vertex_cover)

(def ^:dynamic greedy_min_vertex_cover_cover nil)

(def ^:dynamic greedy_min_vertex_cover_deg nil)

(def ^:dynamic greedy_min_vertex_cover_g nil)

(def ^:dynamic greedy_min_vertex_cover_i nil)

(def ^:dynamic greedy_min_vertex_cover_key nil)

(def ^:dynamic greedy_min_vertex_cover_max_deg nil)

(def ^:dynamic greedy_min_vertex_cover_max_v nil)

(def ^:dynamic greedy_min_vertex_cover_n nil)

(def ^:dynamic greedy_min_vertex_cover_neighbors nil)

(def ^:dynamic remove_value_i nil)

(def ^:dynamic remove_value_res nil)

(defn remove_value [remove_value_lst remove_value_val]
  (binding [remove_value_i nil remove_value_res nil] (try (do (set! remove_value_res []) (set! remove_value_i 0) (while (< remove_value_i (count remove_value_lst)) (do (when (not= (nth remove_value_lst remove_value_i) remove_value_val) (set! remove_value_res (conj remove_value_res (nth remove_value_lst remove_value_i)))) (set! remove_value_i (+ remove_value_i 1)))) (throw (ex-info "return" {:v remove_value_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn greedy_min_vertex_cover [greedy_min_vertex_cover_graph]
  (binding [greedy_min_vertex_cover_cover nil greedy_min_vertex_cover_deg nil greedy_min_vertex_cover_g nil greedy_min_vertex_cover_i nil greedy_min_vertex_cover_key nil greedy_min_vertex_cover_max_deg nil greedy_min_vertex_cover_max_v nil greedy_min_vertex_cover_n nil greedy_min_vertex_cover_neighbors nil] (try (do (set! greedy_min_vertex_cover_g greedy_min_vertex_cover_graph) (set! greedy_min_vertex_cover_cover []) (loop [while_flag_1 true] (when (and while_flag_1 true) (do (set! greedy_min_vertex_cover_max_v 0) (set! greedy_min_vertex_cover_max_deg 0) (doseq [v (keys greedy_min_vertex_cover_g)] (do (set! greedy_min_vertex_cover_key (long v)) (set! greedy_min_vertex_cover_deg (count (get greedy_min_vertex_cover_g greedy_min_vertex_cover_key))) (when (> greedy_min_vertex_cover_deg greedy_min_vertex_cover_max_deg) (do (set! greedy_min_vertex_cover_max_deg greedy_min_vertex_cover_deg) (set! greedy_min_vertex_cover_max_v greedy_min_vertex_cover_key))))) (cond (= greedy_min_vertex_cover_max_deg 0) (recur false) :else (do (set! greedy_min_vertex_cover_cover (conj greedy_min_vertex_cover_cover greedy_min_vertex_cover_max_v)) (set! greedy_min_vertex_cover_neighbors (get greedy_min_vertex_cover_g greedy_min_vertex_cover_max_v)) (set! greedy_min_vertex_cover_i 0) (while (< greedy_min_vertex_cover_i (count greedy_min_vertex_cover_neighbors)) (do (set! greedy_min_vertex_cover_n (nth greedy_min_vertex_cover_neighbors greedy_min_vertex_cover_i)) (set! greedy_min_vertex_cover_g (assoc greedy_min_vertex_cover_g greedy_min_vertex_cover_n (remove_value (get greedy_min_vertex_cover_g greedy_min_vertex_cover_n) greedy_min_vertex_cover_max_v))) (set! greedy_min_vertex_cover_i (+ greedy_min_vertex_cover_i 1)))) (set! greedy_min_vertex_cover_g (assoc greedy_min_vertex_cover_g greedy_min_vertex_cover_max_v [])) (recur while_flag_1)))))) (throw (ex-info "return" {:v greedy_min_vertex_cover_cover}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_graph {0 [1 3] 1 [0 3] 2 [0 3 4] 3 [0 1 2] 4 [2 3]})

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (greedy_min_vertex_cover main_graph))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
