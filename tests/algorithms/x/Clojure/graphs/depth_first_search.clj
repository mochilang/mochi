(ns main (:refer-clojure :exclude [contains depth_first_search]))

(require 'clojure.set)

(defrecord G [A B C D E F G])

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(defn indexOf [s sub]
  (let [idx (clojure.string/index-of s sub)] (if (nil? idx) -1 idx)))

(defn split [s sep]
  (clojure.string/split s (re-pattern sep)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(def ^:dynamic contains_i nil)

(def ^:dynamic depth_first_search_adj nil)

(def ^:dynamic depth_first_search_explored nil)

(def ^:dynamic depth_first_search_i nil)

(def ^:dynamic depth_first_search_idx nil)

(def ^:dynamic depth_first_search_neighbors nil)

(def ^:dynamic depth_first_search_stack nil)

(def ^:dynamic depth_first_search_v nil)

(declare contains depth_first_search)

(defn contains [contains_lst contains_v]
  (binding [contains_i nil] (try (do (set! contains_i 0) (while (< contains_i (count contains_lst)) (do (when (= (nth contains_lst contains_i) contains_v) (throw (ex-info "return" {:v true}))) (set! contains_i (+ contains_i 1)))) (throw (ex-info "return" {:v false}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn depth_first_search [depth_first_search_graph depth_first_search_start]
  (binding [depth_first_search_adj nil depth_first_search_explored nil depth_first_search_i nil depth_first_search_idx nil depth_first_search_neighbors nil depth_first_search_stack nil depth_first_search_v nil] (try (do (set! depth_first_search_explored []) (set! depth_first_search_stack []) (set! depth_first_search_stack (conj depth_first_search_stack depth_first_search_start)) (set! depth_first_search_explored (conj depth_first_search_explored depth_first_search_start)) (while (> (count depth_first_search_stack) 0) (do (set! depth_first_search_idx (- (count depth_first_search_stack) 1)) (set! depth_first_search_v (nth depth_first_search_stack depth_first_search_idx)) (set! depth_first_search_stack (subvec depth_first_search_stack 0 (min depth_first_search_idx (count depth_first_search_stack)))) (set! depth_first_search_neighbors (nth depth_first_search_graph depth_first_search_v)) (set! depth_first_search_i (- (count depth_first_search_neighbors) 1)) (while (>= depth_first_search_i 0) (do (set! depth_first_search_adj (nth depth_first_search_neighbors depth_first_search_i)) (when (not (contains depth_first_search_explored depth_first_search_adj)) (do (set! depth_first_search_explored (conj depth_first_search_explored depth_first_search_adj)) (set! depth_first_search_stack (conj depth_first_search_stack depth_first_search_adj)))) (set! depth_first_search_i (- depth_first_search_i 1)))))) (throw (ex-info "return" {:v depth_first_search_explored}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_G {"A" ["B" "C" "D"] "B" ["A" "D" "E"] "C" ["A" "F"] "D" ["B" "D"] "E" ["B" "F"] "F" ["C" "E" "G"] "G" ["F"]})

(def ^:dynamic main_result (depth_first_search main_G "A"))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println main_result)
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
