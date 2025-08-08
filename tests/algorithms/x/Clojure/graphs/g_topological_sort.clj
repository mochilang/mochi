(ns main (:refer-clojure :exclude [depth_first_search topological_sort print_stack format_list main]))

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

(declare depth_first_search topological_sort print_stack format_list main)

(def ^:dynamic depth_first_search_i nil)

(def ^:dynamic depth_first_search_stack nil)

(def ^:dynamic depth_first_search_v nil)

(def ^:dynamic depth_first_search_visited nil)

(def ^:dynamic format_list_i nil)

(def ^:dynamic format_list_res nil)

(def ^:dynamic main_clothes nil)

(def ^:dynamic main_graph nil)

(def ^:dynamic main_stack nil)

(def ^:dynamic print_stack_idx nil)

(def ^:dynamic print_stack_order nil)

(def ^:dynamic print_stack_s nil)

(def ^:dynamic topological_sort_i nil)

(def ^:dynamic topological_sort_stack nil)

(def ^:dynamic topological_sort_visited nil)

(defn depth_first_search [depth_first_search_u depth_first_search_visited_p depth_first_search_graph depth_first_search_stack_p]
  (binding [depth_first_search_i nil depth_first_search_stack nil depth_first_search_v nil depth_first_search_visited nil] (try (do (set! depth_first_search_visited depth_first_search_visited_p) (set! depth_first_search_stack depth_first_search_stack_p) (set! depth_first_search_visited (assoc depth_first_search_visited depth_first_search_u true)) (set! depth_first_search_i 0) (while (< depth_first_search_i (count (nth depth_first_search_graph depth_first_search_u))) (do (set! depth_first_search_v (nth (nth depth_first_search_graph depth_first_search_u) depth_first_search_i)) (when (not (nth depth_first_search_visited depth_first_search_v)) (set! depth_first_search_stack (depth_first_search depth_first_search_v depth_first_search_visited depth_first_search_graph depth_first_search_stack))) (set! depth_first_search_i (+ depth_first_search_i 1)))) (set! depth_first_search_stack (conj depth_first_search_stack depth_first_search_u)) (throw (ex-info "return" {:v depth_first_search_stack}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn topological_sort [topological_sort_graph]
  (binding [topological_sort_i nil topological_sort_stack nil topological_sort_visited nil] (try (do (set! topological_sort_visited []) (set! topological_sort_i 0) (while (< topological_sort_i (count topological_sort_graph)) (do (set! topological_sort_visited (conj topological_sort_visited false)) (set! topological_sort_i (+ topological_sort_i 1)))) (set! topological_sort_stack []) (set! topological_sort_i 0) (while (< topological_sort_i (count topological_sort_graph)) (do (when (not (nth topological_sort_visited topological_sort_i)) (set! topological_sort_stack (depth_first_search topological_sort_i topological_sort_visited topological_sort_graph topological_sort_stack))) (set! topological_sort_i (+ topological_sort_i 1)))) (throw (ex-info "return" {:v topological_sort_stack}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn print_stack [print_stack_stack print_stack_clothes]
  (binding [print_stack_idx nil print_stack_order nil print_stack_s nil] (do (set! print_stack_order 1) (set! print_stack_s print_stack_stack) (while (> (count print_stack_s) 0) (do (set! print_stack_idx (nth print_stack_s (- (count print_stack_s) 1))) (set! print_stack_s (subvec print_stack_s 0 (- (count print_stack_s) 1))) (println (str (str (str print_stack_order) " ") (get print_stack_clothes print_stack_idx))) (set! print_stack_order (+ print_stack_order 1)))))))

(defn format_list [format_list_xs]
  (binding [format_list_i nil format_list_res nil] (try (do (set! format_list_res "[") (set! format_list_i 0) (while (< format_list_i (count format_list_xs)) (do (set! format_list_res (str format_list_res (str (nth format_list_xs format_list_i)))) (when (< format_list_i (- (count format_list_xs) 1)) (set! format_list_res (str format_list_res ", "))) (set! format_list_i (+ format_list_i 1)))) (set! format_list_res (str format_list_res "]")) (throw (ex-info "return" {:v format_list_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_clothes nil main_graph nil main_stack nil] (do (set! main_clothes {0 "underwear" 1 "pants" 2 "belt" 3 "suit" 4 "shoe" 5 "socks" 6 "shirt" 7 "tie" 8 "watch"}) (set! main_graph [[1 4] [2 4] [3] [] [] [4] [2 7] [3] []]) (set! main_stack (topological_sort main_graph)) (println (format_list main_stack)) (print_stack main_stack main_clothes))))

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
