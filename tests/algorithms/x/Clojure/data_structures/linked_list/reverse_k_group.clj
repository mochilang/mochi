(ns main (:refer-clojure :exclude [to_string reverse_k_nodes main]))

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

(declare to_string reverse_k_nodes main)

(def ^:dynamic main_k nil)

(def ^:dynamic main_ll nil)

(def ^:dynamic reverse_k_nodes_g nil)

(def ^:dynamic reverse_k_nodes_group nil)

(def ^:dynamic reverse_k_nodes_i nil)

(def ^:dynamic reverse_k_nodes_j nil)

(def ^:dynamic reverse_k_nodes_res nil)

(def ^:dynamic to_string_i nil)

(def ^:dynamic to_string_s nil)

(defn to_string [to_string_list]
  (binding [to_string_i nil to_string_s nil] (try (do (when (= (count (:data to_string_list)) 0) (throw (ex-info "return" {:v ""}))) (set! to_string_s (str (get (:data to_string_list) 0))) (set! to_string_i 1) (while (< to_string_i (count (:data to_string_list))) (do (set! to_string_s (str (str to_string_s " -> ") (str (get (:data to_string_list) to_string_i)))) (set! to_string_i (+ to_string_i 1)))) (throw (ex-info "return" {:v to_string_s}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn reverse_k_nodes [reverse_k_nodes_list reverse_k_nodes_k]
  (binding [reverse_k_nodes_g nil reverse_k_nodes_group nil reverse_k_nodes_i nil reverse_k_nodes_j nil reverse_k_nodes_res nil] (try (do (when (<= reverse_k_nodes_k 1) (throw (ex-info "return" {:v reverse_k_nodes_list}))) (set! reverse_k_nodes_res []) (set! reverse_k_nodes_i 0) (while (< reverse_k_nodes_i (count (:data reverse_k_nodes_list))) (do (set! reverse_k_nodes_j 0) (set! reverse_k_nodes_group []) (while (and (< reverse_k_nodes_j reverse_k_nodes_k) (< (+ reverse_k_nodes_i reverse_k_nodes_j) (count (:data reverse_k_nodes_list)))) (do (set! reverse_k_nodes_group (conj reverse_k_nodes_group (get (:data reverse_k_nodes_list) (+ reverse_k_nodes_i reverse_k_nodes_j)))) (set! reverse_k_nodes_j (+ reverse_k_nodes_j 1)))) (if (= (count reverse_k_nodes_group) reverse_k_nodes_k) (do (set! reverse_k_nodes_g (- reverse_k_nodes_k 1)) (while (>= reverse_k_nodes_g 0) (do (set! reverse_k_nodes_res (conj reverse_k_nodes_res (nth reverse_k_nodes_group reverse_k_nodes_g))) (set! reverse_k_nodes_g (- reverse_k_nodes_g 1))))) (do (set! reverse_k_nodes_g 0) (while (< reverse_k_nodes_g (count reverse_k_nodes_group)) (do (set! reverse_k_nodes_res (conj reverse_k_nodes_res (nth reverse_k_nodes_group reverse_k_nodes_g))) (set! reverse_k_nodes_g (+ reverse_k_nodes_g 1)))))) (set! reverse_k_nodes_i (+ reverse_k_nodes_i reverse_k_nodes_k)))) (throw (ex-info "return" {:v {:data reverse_k_nodes_res}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_k nil main_ll nil] (do (set! main_ll {:data [1 2 3 4 5]}) (println (str "Original Linked List: " (to_string main_ll))) (set! main_k 2) (set! main_ll (reverse_k_nodes main_ll main_k)) (println (str (str (str "After reversing groups of size " (str main_k)) ": ") (to_string main_ll))))))

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
