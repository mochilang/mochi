(ns main (:refer-clojure :exclude [dfs even_tree main]))

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

(declare dfs even_tree main)

(def ^:dynamic dfs_cuts nil)

(def ^:dynamic dfs_res nil)

(def ^:dynamic dfs_size nil)

(def ^:dynamic dfs_visited nil)

(def ^:dynamic even_tree_res nil)

(def ^:dynamic even_tree_visited nil)

(def ^:dynamic main_edges nil)

(def ^:dynamic main_i nil)

(def ^:dynamic main_u nil)

(def ^:dynamic main_v nil)

(def ^:dynamic main_tree {})

(defn dfs [dfs_start dfs_visited_p]
  (binding [dfs_cuts nil dfs_res nil dfs_size nil dfs_visited nil] (try (do (set! dfs_visited dfs_visited_p) (set! dfs_size 1) (set! dfs_cuts 0) (set! dfs_visited (assoc dfs_visited dfs_start true)) (doseq [v (get main_tree dfs_start)] (when (not (in v dfs_visited)) (do (set! dfs_res (dfs v dfs_visited)) (set! dfs_size (+ dfs_size (nth dfs_res 0))) (set! dfs_cuts (+ dfs_cuts (nth dfs_res 1)))))) (when (= (mod dfs_size 2) 0) (set! dfs_cuts (+ dfs_cuts 1))) (throw (ex-info "return" {:v [dfs_size dfs_cuts]}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn even_tree []
  (binding [even_tree_res nil even_tree_visited nil] (try (do (set! even_tree_visited {}) (set! even_tree_res (dfs 1 even_tree_visited)) (throw (ex-info "return" {:v (- (nth even_tree_res 1) 1)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_edges nil main_i nil main_u nil main_v nil] (do (set! main_edges [[2 1] [3 1] [4 3] [5 2] [6 1] [7 2] [8 6] [9 8] [10 8]]) (set! main_i 0) (while (< main_i (count main_edges)) (do (set! main_u (nth (nth main_edges main_i) 0)) (set! main_v (nth (nth main_edges main_i) 1)) (when (not (in main_u main_tree)) (alter-var-root (var main_tree) (fn [_] (assoc main_tree main_u [])))) (when (not (in main_v main_tree)) (alter-var-root (var main_tree) (fn [_] (assoc main_tree main_v [])))) (alter-var-root (var main_tree) (fn [_] (assoc main_tree main_u (conj (get main_tree main_u) main_v)))) (alter-var-root (var main_tree) (fn [_] (assoc main_tree main_v (conj (get main_tree main_v) main_u)))) (set! main_i (+ main_i 1)))) (println (str (even_tree))))))

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
