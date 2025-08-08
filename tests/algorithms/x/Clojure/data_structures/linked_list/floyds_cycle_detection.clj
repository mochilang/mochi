(ns main (:refer-clojure :exclude [empty_list add_node set_next detect_cycle main]))

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

(declare empty_list add_node set_next detect_cycle main)

(def ^:dynamic add_node_i nil)

(def ^:dynamic add_node_last nil)

(def ^:dynamic add_node_new_index nil)

(def ^:dynamic add_node_new_nexts nil)

(def ^:dynamic add_node_nexts nil)

(def ^:dynamic detect_cycle_fast nil)

(def ^:dynamic detect_cycle_nexts nil)

(def ^:dynamic detect_cycle_slow nil)

(def ^:dynamic main_ll nil)

(def ^:dynamic set_next_i nil)

(def ^:dynamic set_next_new_nexts nil)

(def ^:dynamic set_next_nexts nil)

(def ^:dynamic main_NULL (- 0 1))

(defn empty_list []
  (try (throw (ex-info "return" {:v {:head main_NULL :next []}})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn add_node [add_node_list add_node_value]
  (binding [add_node_i nil add_node_last nil add_node_new_index nil add_node_new_nexts nil add_node_nexts nil] (try (do (set! add_node_nexts (:next add_node_list)) (set! add_node_new_index (count add_node_nexts)) (set! add_node_nexts (conj add_node_nexts main_NULL)) (when (= (:head add_node_list) main_NULL) (throw (ex-info "return" {:v {:head add_node_new_index :next add_node_nexts}}))) (set! add_node_last (:head add_node_list)) (while (not= (nth add_node_nexts add_node_last) main_NULL) (set! add_node_last (nth add_node_nexts add_node_last))) (set! add_node_new_nexts []) (set! add_node_i 0) (while (< add_node_i (count add_node_nexts)) (do (if (= add_node_i add_node_last) (set! add_node_new_nexts (conj add_node_new_nexts add_node_new_index)) (set! add_node_new_nexts (conj add_node_new_nexts (nth add_node_nexts add_node_i)))) (set! add_node_i (+ add_node_i 1)))) (throw (ex-info "return" {:v {:head (:head add_node_list) :next add_node_new_nexts}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn set_next [set_next_list set_next_index set_next_next_index]
  (binding [set_next_i nil set_next_new_nexts nil set_next_nexts nil] (try (do (set! set_next_nexts (:next set_next_list)) (set! set_next_new_nexts []) (set! set_next_i 0) (while (< set_next_i (count set_next_nexts)) (do (if (= set_next_i set_next_index) (set! set_next_new_nexts (conj set_next_new_nexts set_next_next_index)) (set! set_next_new_nexts (conj set_next_new_nexts (nth set_next_nexts set_next_i)))) (set! set_next_i (+ set_next_i 1)))) (throw (ex-info "return" {:v {:head (:head set_next_list) :next set_next_new_nexts}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn detect_cycle [detect_cycle_list]
  (binding [detect_cycle_fast nil detect_cycle_nexts nil detect_cycle_slow nil] (try (do (when (= (:head detect_cycle_list) main_NULL) (throw (ex-info "return" {:v false}))) (set! detect_cycle_nexts (:next detect_cycle_list)) (set! detect_cycle_slow (:head detect_cycle_list)) (set! detect_cycle_fast (:head detect_cycle_list)) (while (and (not= detect_cycle_fast main_NULL) (not= (nth detect_cycle_nexts detect_cycle_fast) main_NULL)) (do (set! detect_cycle_slow (nth detect_cycle_nexts detect_cycle_slow)) (set! detect_cycle_fast (nth detect_cycle_nexts (nth detect_cycle_nexts detect_cycle_fast))) (when (= detect_cycle_slow detect_cycle_fast) (throw (ex-info "return" {:v true}))))) (throw (ex-info "return" {:v false}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_ll nil] (do (set! main_ll (empty_list)) (set! main_ll (add_node main_ll 1)) (set! main_ll (add_node main_ll 2)) (set! main_ll (add_node main_ll 3)) (set! main_ll (add_node main_ll 4)) (set! main_ll (set_next main_ll 3 1)) (println (detect_cycle main_ll)))))

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
