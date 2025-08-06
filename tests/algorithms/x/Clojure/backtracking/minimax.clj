(ns main (:refer-clojure :exclude [minimax tree_height main]))

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

(declare minimax tree_height main)

(def ^:dynamic main_height nil)

(def ^:dynamic main_scores nil)

(def ^:dynamic minimax_left nil)

(def ^:dynamic minimax_right nil)

(def ^:dynamic tree_height_h nil)

(def ^:dynamic tree_height_v nil)

(defn minimax [minimax_depth minimax_node_index minimax_is_max minimax_scores minimax_height]
  (binding [minimax_left nil minimax_right nil] (try (do (when (< minimax_depth 0) (throw (Exception. "Depth cannot be less than 0"))) (when (= (count minimax_scores) 0) (throw (Exception. "Scores cannot be empty"))) (when (= minimax_depth minimax_height) (throw (ex-info "return" {:v (nth minimax_scores minimax_node_index)}))) (when minimax_is_max (do (set! minimax_left (minimax (+ minimax_depth 1) (* minimax_node_index 2) false minimax_scores minimax_height)) (set! minimax_right (minimax (+ minimax_depth 1) (+ (* minimax_node_index 2) 1) false minimax_scores minimax_height)) (if (> minimax_left minimax_right) (throw (ex-info "return" {:v minimax_left})) (throw (ex-info "return" {:v minimax_right}))))) (set! minimax_left (minimax (+ minimax_depth 1) (* minimax_node_index 2) true minimax_scores minimax_height)) (set! minimax_right (minimax (+ minimax_depth 1) (+ (* minimax_node_index 2) 1) true minimax_scores minimax_height)) (if (< minimax_left minimax_right) (throw (ex-info "return" {:v minimax_left})) (throw (ex-info "return" {:v minimax_right})))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn tree_height [tree_height_n]
  (binding [tree_height_h nil tree_height_v nil] (try (do (set! tree_height_h 0) (set! tree_height_v tree_height_n) (while (> tree_height_v 1) (do (set! tree_height_v (quot tree_height_v 2)) (set! tree_height_h (+ tree_height_h 1)))) (throw (ex-info "return" {:v tree_height_h}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_height nil main_scores nil] (do (set! main_scores [90 23 6 33 21 65 123 34423]) (set! main_height (tree_height (count main_scores))) (println (str "Optimal value : " (str (minimax 0 0 true main_scores main_height)))))))

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
