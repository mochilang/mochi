(ns main (:refer-clojure :exclude [valid_connection util_hamilton_cycle hamilton_cycle]))

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

(declare valid_connection util_hamilton_cycle hamilton_cycle)

(def ^:dynamic hamilton_cycle_i nil)

(def ^:dynamic hamilton_cycle_last nil)

(def ^:dynamic hamilton_cycle_path nil)

(def ^:dynamic util_hamilton_cycle_next_ver nil)

(def ^:dynamic util_hamilton_cycle_path nil)

(defn valid_connection [valid_connection_graph valid_connection_next_ver valid_connection_curr_ind valid_connection_path]
  (try (do (when (= (nth (nth valid_connection_graph (nth valid_connection_path (- valid_connection_curr_ind 1))) valid_connection_next_ver) 0) (throw (ex-info "return" {:v false}))) (doseq [v valid_connection_path] (when (= v valid_connection_next_ver) (throw (ex-info "return" {:v false})))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn util_hamilton_cycle [util_hamilton_cycle_graph util_hamilton_cycle_path_p util_hamilton_cycle_curr_ind]
  (binding [util_hamilton_cycle_next_ver nil util_hamilton_cycle_path nil] (try (do (set! util_hamilton_cycle_path util_hamilton_cycle_path_p) (when (= util_hamilton_cycle_curr_ind (count util_hamilton_cycle_graph)) (throw (ex-info "return" {:v (= (nth (nth util_hamilton_cycle_graph (nth util_hamilton_cycle_path (- util_hamilton_cycle_curr_ind 1))) (nth util_hamilton_cycle_path 0)) 1)}))) (set! util_hamilton_cycle_next_ver 0) (while (< util_hamilton_cycle_next_ver (count util_hamilton_cycle_graph)) (do (when (valid_connection util_hamilton_cycle_graph util_hamilton_cycle_next_ver util_hamilton_cycle_curr_ind util_hamilton_cycle_path) (do (set! util_hamilton_cycle_path (assoc util_hamilton_cycle_path util_hamilton_cycle_curr_ind util_hamilton_cycle_next_ver)) (when (util_hamilton_cycle util_hamilton_cycle_graph util_hamilton_cycle_path (+ util_hamilton_cycle_curr_ind 1)) (throw (ex-info "return" {:v true}))) (set! util_hamilton_cycle_path (assoc util_hamilton_cycle_path util_hamilton_cycle_curr_ind (- 1))))) (set! util_hamilton_cycle_next_ver (+ util_hamilton_cycle_next_ver 1)))) (throw (ex-info "return" {:v false}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn hamilton_cycle [hamilton_cycle_graph hamilton_cycle_start_index]
  (binding [hamilton_cycle_i nil hamilton_cycle_last nil hamilton_cycle_path nil] (try (do (set! hamilton_cycle_path nil) (set! hamilton_cycle_i 0) (while (< hamilton_cycle_i (+ (count hamilton_cycle_graph) 1)) (do (set! hamilton_cycle_path (assoc hamilton_cycle_path hamilton_cycle_i (- 1))) (set! hamilton_cycle_i (+ hamilton_cycle_i 1)))) (set! hamilton_cycle_path (assoc hamilton_cycle_path 0 hamilton_cycle_start_index)) (set! hamilton_cycle_last (- (count hamilton_cycle_path) 1)) (set! hamilton_cycle_path (assoc hamilton_cycle_path hamilton_cycle_last hamilton_cycle_start_index)) (if (util_hamilton_cycle hamilton_cycle_graph hamilton_cycle_path 1) hamilton_cycle_path [])) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]

      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
