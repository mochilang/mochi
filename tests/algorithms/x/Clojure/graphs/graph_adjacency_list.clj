(ns main (:refer-clojure :exclude [create_graph add_vertex remove_from_list remove_key add_edge remove_edge remove_vertex contains_vertex contains_edge clear_graph to_string main]))

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

(declare create_graph add_vertex remove_from_list remove_key add_edge remove_edge remove_vertex contains_vertex contains_edge clear_graph to_string main)

(def ^:dynamic add_edge_adj nil)

(def ^:dynamic add_edge_list_d nil)

(def ^:dynamic add_edge_list_s nil)

(def ^:dynamic add_vertex_adj nil)

(def ^:dynamic create_graph_adj nil)

(def ^:dynamic create_graph_d nil)

(def ^:dynamic create_graph_s nil)

(def ^:dynamic main_edges nil)

(def ^:dynamic main_g nil)

(def ^:dynamic main_vertices nil)

(def ^:dynamic remove_edge_adj nil)

(def ^:dynamic remove_from_list_i nil)

(def ^:dynamic remove_from_list_res nil)

(def ^:dynamic remove_key_res nil)

(def ^:dynamic remove_vertex_adj nil)

(defn create_graph [create_graph_vertices create_graph_edges create_graph_directed]
  (binding [create_graph_adj nil create_graph_d nil create_graph_s nil] (try (do (set! create_graph_adj {}) (doseq [v create_graph_vertices] (set! create_graph_adj (assoc create_graph_adj v []))) (doseq [e create_graph_edges] (do (set! create_graph_s (nth e 0)) (set! create_graph_d (nth e 1)) (when (not (in create_graph_s create_graph_adj)) (set! create_graph_adj (assoc create_graph_adj create_graph_s []))) (when (not (in create_graph_d create_graph_adj)) (set! create_graph_adj (assoc create_graph_adj create_graph_d []))) (set! create_graph_adj (assoc create_graph_adj create_graph_s (conj (get create_graph_adj create_graph_s) create_graph_d))) (when (not create_graph_directed) (set! create_graph_adj (assoc create_graph_adj create_graph_d (conj (get create_graph_adj create_graph_d) create_graph_s)))))) (throw (ex-info "return" {:v {:adj create_graph_adj :directed create_graph_directed}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn add_vertex [add_vertex_graph add_vertex_v]
  (binding [add_vertex_adj nil] (try (do (when (in add_vertex_v (:adj add_vertex_graph)) (throw (Exception. "vertex exists"))) (set! add_vertex_adj {}) (doseq [k (keys (:adj add_vertex_graph))] (set! add_vertex_adj (assoc add_vertex_adj k (get (:adj add_vertex_graph) k)))) (set! add_vertex_adj (assoc add_vertex_adj add_vertex_v [])) (throw (ex-info "return" {:v {:adj add_vertex_adj :directed (:directed add_vertex_graph)}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn remove_from_list [remove_from_list_lst remove_from_list_value]
  (binding [remove_from_list_i nil remove_from_list_res nil] (try (do (set! remove_from_list_res []) (set! remove_from_list_i 0) (while (< remove_from_list_i (count remove_from_list_lst)) (do (when (not= (nth remove_from_list_lst remove_from_list_i) remove_from_list_value) (set! remove_from_list_res (conj remove_from_list_res (nth remove_from_list_lst remove_from_list_i)))) (set! remove_from_list_i (+ remove_from_list_i 1)))) (throw (ex-info "return" {:v remove_from_list_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn remove_key [remove_key_m remove_key_key]
  (binding [remove_key_res nil] (try (do (set! remove_key_res {}) (doseq [k (keys remove_key_m)] (when (not= k remove_key_key) (set! remove_key_res (assoc remove_key_res k (get remove_key_m k))))) (throw (ex-info "return" {:v remove_key_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn add_edge [add_edge_graph add_edge_s add_edge_d]
  (binding [add_edge_adj nil add_edge_list_d nil add_edge_list_s nil] (try (do (when (or (not (in add_edge_s (:adj add_edge_graph))) (not (in add_edge_d (:adj add_edge_graph)))) (throw (Exception. "vertex missing"))) (when (contains_edge add_edge_graph add_edge_s add_edge_d) (throw (Exception. "edge exists"))) (set! add_edge_adj {}) (doseq [k (keys (:adj add_edge_graph))] (set! add_edge_adj (assoc add_edge_adj k (get (:adj add_edge_graph) k)))) (set! add_edge_list_s (get add_edge_adj add_edge_s)) (set! add_edge_list_s (conj add_edge_list_s add_edge_d)) (set! add_edge_adj (assoc add_edge_adj add_edge_s add_edge_list_s)) (when (not (:directed add_edge_graph)) (do (set! add_edge_list_d (get add_edge_adj add_edge_d)) (set! add_edge_list_d (conj add_edge_list_d add_edge_s)) (set! add_edge_adj (assoc add_edge_adj add_edge_d add_edge_list_d)))) (throw (ex-info "return" {:v {:adj add_edge_adj :directed (:directed add_edge_graph)}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn remove_edge [remove_edge_graph remove_edge_s remove_edge_d]
  (binding [remove_edge_adj nil] (try (do (when (or (not (in remove_edge_s (:adj remove_edge_graph))) (not (in remove_edge_d (:adj remove_edge_graph)))) (throw (Exception. "vertex missing"))) (when (not (contains_edge remove_edge_graph remove_edge_s remove_edge_d)) (throw (Exception. "edge missing"))) (set! remove_edge_adj {}) (doseq [k (keys (:adj remove_edge_graph))] (set! remove_edge_adj (assoc remove_edge_adj k (get (:adj remove_edge_graph) k)))) (set! remove_edge_adj (assoc remove_edge_adj remove_edge_s (remove_from_list (get remove_edge_adj remove_edge_s) remove_edge_d))) (when (not (:directed remove_edge_graph)) (set! remove_edge_adj (assoc remove_edge_adj remove_edge_d (remove_from_list (get remove_edge_adj remove_edge_d) remove_edge_s)))) (throw (ex-info "return" {:v {:adj remove_edge_adj :directed (:directed remove_edge_graph)}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn remove_vertex [remove_vertex_graph remove_vertex_v]
  (binding [remove_vertex_adj nil] (try (do (when (not (in remove_vertex_v (:adj remove_vertex_graph))) (throw (Exception. "vertex missing"))) (set! remove_vertex_adj {}) (doseq [k (keys (:adj remove_vertex_graph))] (when (not= k remove_vertex_v) (set! remove_vertex_adj (assoc remove_vertex_adj k (remove_from_list (get (:adj remove_vertex_graph) k) remove_vertex_v))))) (throw (ex-info "return" {:v {:adj remove_vertex_adj :directed (:directed remove_vertex_graph)}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn contains_vertex [contains_vertex_graph contains_vertex_v]
  (try (throw (ex-info "return" {:v (in contains_vertex_v (:adj contains_vertex_graph))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn contains_edge [contains_edge_graph contains_edge_s contains_edge_d]
  (try (do (when (or (not (in contains_edge_s (:adj contains_edge_graph))) (not (in contains_edge_d (:adj contains_edge_graph)))) (throw (Exception. "vertex missing"))) (doseq [x (get (:adj contains_edge_graph) contains_edge_s)] (when (= x contains_edge_d) (throw (ex-info "return" {:v true})))) (throw (ex-info "return" {:v false}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn clear_graph [clear_graph_graph]
  (try (throw (ex-info "return" {:v {:adj {} :directed (:directed clear_graph_graph)}})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn to_string [to_string_graph]
  (try (throw (ex-info "return" {:v (str (:adj to_string_graph))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (binding [main_edges nil main_g nil main_vertices nil] (do (set! main_vertices ["1" "2" "3" "4"]) (set! main_edges [["1" "2"] ["2" "3"] ["3" "4"]]) (set! main_g (create_graph main_vertices main_edges false)) (println (to_string main_g)) (set! main_g (add_vertex main_g "5")) (set! main_g (add_edge main_g "4" "5")) (println (str (contains_edge main_g "4" "5"))) (set! main_g (remove_edge main_g "1" "2")) (set! main_g (remove_vertex main_g "3")) (println (to_string main_g)))))

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
