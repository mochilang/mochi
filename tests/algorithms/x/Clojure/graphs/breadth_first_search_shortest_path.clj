(ns main (:refer-clojure :exclude [newGraph breath_first_search shortest_path]))

(require 'clojure.set)

(defrecord Graph1 [A B C D E F G])

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(defn indexOf [s sub]
  (let [idx (clojure.string/index-of s sub)] (if (nil? idx) -1 idx)))

(defn split [s sep]
  (clojure.string/split s (re-pattern sep)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(def ^:dynamic breath_first_search_g nil)

(def ^:dynamic breath_first_search_idx nil)

(def ^:dynamic breath_first_search_parent nil)

(def ^:dynamic breath_first_search_queue nil)

(def ^:dynamic breath_first_search_vertex nil)

(def ^:dynamic main_g nil)

(def ^:dynamic shortest_path_p nil)

(declare newGraph breath_first_search shortest_path)

(defn newGraph [newGraph_g newGraph_s]
  (try (throw (ex-info "return" {:v {:graph newGraph_g :parent {} :source newGraph_s}})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn breath_first_search [breath_first_search_g_p]
  (binding [breath_first_search_g nil breath_first_search_idx nil breath_first_search_parent nil breath_first_search_queue nil breath_first_search_vertex nil] (try (do (set! breath_first_search_g breath_first_search_g_p) (set! breath_first_search_parent (:parent breath_first_search_g)) (set! breath_first_search_parent (assoc breath_first_search_parent (:source breath_first_search_g) (:source breath_first_search_g))) (set! breath_first_search_queue [(:source breath_first_search_g)]) (set! breath_first_search_idx 0) (while (< breath_first_search_idx (count breath_first_search_queue)) (do (set! breath_first_search_vertex (nth breath_first_search_queue breath_first_search_idx)) (doseq [adj (get (:graph breath_first_search_g) breath_first_search_vertex)] (when (not (in adj breath_first_search_parent)) (do (set! breath_first_search_parent (assoc breath_first_search_parent adj breath_first_search_vertex)) (set! breath_first_search_queue (conj breath_first_search_queue adj))))) (set! breath_first_search_idx (+ breath_first_search_idx 1)))) (set! breath_first_search_g (assoc breath_first_search_g :parent breath_first_search_parent)) (throw (ex-info "return" {:v breath_first_search_g}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn shortest_path [shortest_path_g shortest_path_target]
  (binding [shortest_path_p nil] (try (do (when (= shortest_path_target (:source shortest_path_g)) (throw (ex-info "return" {:v (:source shortest_path_g)}))) (when (not (in shortest_path_target (:parent shortest_path_g))) (throw (ex-info "return" {:v (str (str (str "No path from vertex: " (:source shortest_path_g)) " to vertex: ") shortest_path_target)}))) (set! shortest_path_p (get (:parent shortest_path_g) shortest_path_target)) (throw (ex-info "return" {:v (str (str (shortest_path shortest_path_g shortest_path_p) "->") shortest_path_target)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_graph {"A" ["B" "C" "E"] "B" ["A" "D" "E"] "C" ["A" "F" "G"] "D" ["B"] "E" ["A" "B" "D"] "F" ["C"] "G" ["C"]})

(def ^:dynamic main_g (newGraph main_graph "G"))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (def main_g (breath_first_search main_g))
      (println (shortest_path main_g "D"))
      (println (shortest_path main_g "G"))
      (println (shortest_path main_g "Foo"))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
