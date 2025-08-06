(ns main (:refer-clojure :exclude [valid_coloring util_color color]))

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

(declare valid_coloring util_color color)

(def ^:dynamic color_colored_vertices nil)

(def ^:dynamic color_i nil)

(def ^:dynamic util_color_c nil)

(def ^:dynamic util_color_colored_vertices nil)

(def ^:dynamic valid_coloring_i nil)

(defn valid_coloring [valid_coloring_neighbours valid_coloring_colored_vertices color_v]
  (binding [valid_coloring_i nil] (try (do (set! valid_coloring_i 0) (while (< valid_coloring_i (count valid_coloring_neighbours)) (do (when (and (= (nth valid_coloring_neighbours valid_coloring_i) 1) (= (nth valid_coloring_colored_vertices valid_coloring_i) color_v)) (throw (ex-info "return" {:v false}))) (set! valid_coloring_i (+ valid_coloring_i 1)))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn util_color [util_color_graph util_color_max_colors util_color_colored_vertices_p util_color_index]
  (binding [util_color_c nil util_color_colored_vertices nil] (try (do (set! util_color_colored_vertices util_color_colored_vertices_p) (when (= util_color_index (count util_color_graph)) (throw (ex-info "return" {:v true}))) (set! util_color_c 0) (while (< util_color_c util_color_max_colors) (do (when (valid_coloring (nth util_color_graph util_color_index) util_color_colored_vertices util_color_c) (do (set! util_color_colored_vertices (assoc util_color_colored_vertices util_color_index util_color_c)) (when (util_color util_color_graph util_color_max_colors util_color_colored_vertices (+ util_color_index 1)) (throw (ex-info "return" {:v true}))) (set! util_color_colored_vertices (assoc util_color_colored_vertices util_color_index (- 1))))) (set! util_color_c (+ util_color_c 1)))) (throw (ex-info "return" {:v false}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn color [color_graph color_max_colors]
  (binding [color_colored_vertices nil color_i nil] (try (do (set! color_colored_vertices []) (set! color_i 0) (while (< color_i (count color_graph)) (do (set! color_colored_vertices (conj color_colored_vertices (- 1))) (set! color_i (+ color_i 1)))) (if (util_color color_graph color_max_colors color_colored_vertices 0) color_colored_vertices [])) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_graph [[0 1 0 0 0] [1 0 1 0 1] [0 1 0 1 0] [0 1 1 0 0] [0 1 0 0 0]])

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (color main_graph 3))
      (println "\n")
      (println (count (color main_graph 2)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
