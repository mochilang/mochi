(ns main (:refer-clojure :exclude [empty_list push swap_nodes to_string main]))

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

(declare empty_list push swap_nodes to_string main)

(def ^:dynamic main_i nil)

(def ^:dynamic main_ll nil)

(def ^:dynamic push_res nil)

(def ^:dynamic swap_nodes_i nil)

(def ^:dynamic swap_nodes_idx1 nil)

(def ^:dynamic swap_nodes_idx2 nil)

(def ^:dynamic swap_nodes_res nil)

(def ^:dynamic swap_nodes_temp nil)

(defn empty_list []
  (try (throw (ex-info "return" {:v {:data []}})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn push [push_list push_value]
  (binding [push_res nil] (try (do (set! push_res [push_value]) (set! push_res (concat push_res (:data push_list))) (throw (ex-info "return" {:v {:data push_res}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn swap_nodes [swap_nodes_list swap_nodes_v1 swap_nodes_v2]
  (binding [swap_nodes_i nil swap_nodes_idx1 nil swap_nodes_idx2 nil swap_nodes_res nil swap_nodes_temp nil] (try (do (when (= swap_nodes_v1 swap_nodes_v2) (throw (ex-info "return" {:v swap_nodes_list}))) (set! swap_nodes_idx1 (- 0 1)) (set! swap_nodes_idx2 (- 0 1)) (set! swap_nodes_i 0) (while (< swap_nodes_i (count (:data swap_nodes_list))) (do (when (and (= (get (:data swap_nodes_list) swap_nodes_i) swap_nodes_v1) (= swap_nodes_idx1 (- 0 1))) (set! swap_nodes_idx1 swap_nodes_i)) (when (and (= (get (:data swap_nodes_list) swap_nodes_i) swap_nodes_v2) (= swap_nodes_idx2 (- 0 1))) (set! swap_nodes_idx2 swap_nodes_i)) (set! swap_nodes_i (+ swap_nodes_i 1)))) (when (or (= swap_nodes_idx1 (- 0 1)) (= swap_nodes_idx2 (- 0 1))) (throw (ex-info "return" {:v swap_nodes_list}))) (set! swap_nodes_res (:data swap_nodes_list)) (set! swap_nodes_temp (nth swap_nodes_res swap_nodes_idx1)) (set! swap_nodes_res (assoc swap_nodes_res swap_nodes_idx1 (nth swap_nodes_res swap_nodes_idx2))) (set! swap_nodes_res (assoc swap_nodes_res swap_nodes_idx2 swap_nodes_temp)) (throw (ex-info "return" {:v {:data swap_nodes_res}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn to_string [to_string_list]
  (try (throw (ex-info "return" {:v (str (:data to_string_list))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (binding [main_i nil main_ll nil] (do (set! main_ll (empty_list)) (set! main_i 5) (while (> main_i 0) (do (set! main_ll (push main_ll main_i)) (set! main_i (- main_i 1)))) (println (str "Original Linked List: " (to_string main_ll))) (set! main_ll (swap_nodes main_ll 1 4)) (println (str "Modified Linked List: " (to_string main_ll))) (println "After swapping the nodes whose data is 1 and 4."))))

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
