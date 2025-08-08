(ns main (:refer-clojure :exclude [depth_first_search check_cycle print_bool]))

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

(declare depth_first_search check_cycle print_bool)

(def ^:dynamic check_cycle_i nil)

(def ^:dynamic check_cycle_n nil)

(def ^:dynamic check_cycle_rec_stk nil)

(def ^:dynamic check_cycle_visited nil)

(def ^:dynamic depth_first_search_rec_stk nil)

(def ^:dynamic depth_first_search_visited nil)

(defn depth_first_search [depth_first_search_graph depth_first_search_vertex depth_first_search_visited_p depth_first_search_rec_stk_p]
  (binding [depth_first_search_rec_stk nil depth_first_search_visited nil] (try (do (set! depth_first_search_visited depth_first_search_visited_p) (set! depth_first_search_rec_stk depth_first_search_rec_stk_p) (set! depth_first_search_visited (assoc depth_first_search_visited depth_first_search_vertex true)) (set! depth_first_search_rec_stk (assoc depth_first_search_rec_stk depth_first_search_vertex true)) (doseq [node (nth depth_first_search_graph depth_first_search_vertex)] (if (not (nth depth_first_search_visited node)) (when (depth_first_search depth_first_search_graph node depth_first_search_visited depth_first_search_rec_stk) (throw (ex-info "return" {:v true}))) (when (nth depth_first_search_rec_stk node) (throw (ex-info "return" {:v true}))))) (set! depth_first_search_rec_stk (assoc depth_first_search_rec_stk depth_first_search_vertex false)) (throw (ex-info "return" {:v false}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn check_cycle [check_cycle_graph]
  (binding [check_cycle_i nil check_cycle_n nil check_cycle_rec_stk nil check_cycle_visited nil] (try (do (set! check_cycle_n (count check_cycle_graph)) (set! check_cycle_visited []) (set! check_cycle_rec_stk []) (set! check_cycle_i 0) (while (< check_cycle_i check_cycle_n) (do (set! check_cycle_visited (conj check_cycle_visited false)) (set! check_cycle_rec_stk (conj check_cycle_rec_stk false)) (set! check_cycle_i (+ check_cycle_i 1)))) (set! check_cycle_i 0) (while (< check_cycle_i check_cycle_n) (do (when (not (nth check_cycle_visited check_cycle_i)) (when (depth_first_search check_cycle_graph check_cycle_i check_cycle_visited check_cycle_rec_stk) (throw (ex-info "return" {:v true})))) (set! check_cycle_i (+ check_cycle_i 1)))) (throw (ex-info "return" {:v false}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn print_bool [print_bool_b]
  (if print_bool_b (println true) (println false)))

(def ^:dynamic main_g1 [[] [0 3] [0 4] [5] [5] []])

(def ^:dynamic main_g2 [[1 2] [2] [0 3] [3]])

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (print_bool (check_cycle main_g1))
      (print_bool (check_cycle main_g2))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
