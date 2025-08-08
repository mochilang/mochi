(ns main (:refer-clojure :exclude [random random_level empty_forward init insert find delete to_string main]))

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

(declare random random_level empty_forward init insert find delete to_string main)

(def ^:dynamic delete_i nil)

(def ^:dynamic delete_update nil)

(def ^:dynamic delete_x nil)

(def ^:dynamic empty_forward_f nil)

(def ^:dynamic empty_forward_i nil)

(def ^:dynamic find_i nil)

(def ^:dynamic find_x nil)

(def ^:dynamic insert_forwards nil)

(def ^:dynamic insert_i nil)

(def ^:dynamic insert_idx nil)

(def ^:dynamic insert_j nil)

(def ^:dynamic insert_lvl nil)

(def ^:dynamic insert_update nil)

(def ^:dynamic insert_x nil)

(def ^:dynamic random_level_lvl nil)

(def ^:dynamic to_string_s nil)

(def ^:dynamic to_string_x nil)

(def ^:dynamic main_NIL (- 0 1))

(def ^:dynamic main_MAX_LEVEL 6)

(def ^:dynamic main_P 0.5)

(def ^:dynamic main_seed 1)

(defn random []
  (try (do (alter-var-root (var main_seed) (fn [_] (mod (+ (* main_seed 13) 7) 100))) (throw (ex-info "return" {:v (/ (double main_seed) 100.0)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn random_level []
  (binding [random_level_lvl nil] (try (do (set! random_level_lvl 1) (while (and (< (random) main_P) (< random_level_lvl main_MAX_LEVEL)) (set! random_level_lvl (+ random_level_lvl 1))) (throw (ex-info "return" {:v random_level_lvl}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn empty_forward []
  (binding [empty_forward_f nil empty_forward_i nil] (try (do (set! empty_forward_f []) (set! empty_forward_i 0) (while (< empty_forward_i main_MAX_LEVEL) (do (set! empty_forward_f (conj empty_forward_f main_NIL)) (set! empty_forward_i (+ empty_forward_i 1)))) (throw (ex-info "return" {:v empty_forward_f}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_node_keys [])

(def ^:dynamic main_node_vals [])

(def ^:dynamic main_node_forwards [])

(def ^:dynamic main_level 1)

(defn init []
  (do (alter-var-root (var main_node_keys) (fn [_] [(- 1)])) (alter-var-root (var main_node_vals) (fn [_] [0])) (alter-var-root (var main_node_forwards) (fn [_] [(empty_forward)])) (alter-var-root (var main_level) (fn [_] 1))))

(defn insert [insert_key insert_value]
  (binding [insert_forwards nil insert_i nil insert_idx nil insert_j nil insert_lvl nil insert_update nil insert_x nil] (try (do (set! insert_update []) (set! insert_i 0) (while (< insert_i main_MAX_LEVEL) (do (set! insert_update (conj insert_update 0)) (set! insert_i (+ insert_i 1)))) (set! insert_x 0) (set! insert_i (- main_level 1)) (while (>= insert_i 0) (do (while (and (not= (nth (nth main_node_forwards insert_x) insert_i) main_NIL) (< (nth main_node_keys (nth (nth main_node_forwards insert_x) insert_i)) insert_key)) (set! insert_x (nth (nth main_node_forwards insert_x) insert_i))) (set! insert_update (assoc insert_update insert_i insert_x)) (set! insert_i (- insert_i 1)))) (set! insert_x (nth (nth main_node_forwards insert_x) 0)) (when (and (not= insert_x main_NIL) (= (nth main_node_keys insert_x) insert_key)) (do (alter-var-root (var main_node_vals) (fn [_] (assoc main_node_vals insert_x insert_value))) (throw (ex-info "return" {:v nil})))) (set! insert_lvl (random_level)) (when (> insert_lvl main_level) (do (set! insert_j main_level) (while (< insert_j insert_lvl) (do (set! insert_update (assoc insert_update insert_j 0)) (set! insert_j (+ insert_j 1)))) (alter-var-root (var main_level) (fn [_] insert_lvl)))) (alter-var-root (var main_node_keys) (fn [_] (conj main_node_keys insert_key))) (alter-var-root (var main_node_vals) (fn [_] (conj main_node_vals insert_value))) (set! insert_forwards (empty_forward)) (set! insert_idx (- (count main_node_keys) 1)) (set! insert_i 0) (while (< insert_i insert_lvl) (do (set! insert_forwards (assoc insert_forwards insert_i (nth (nth main_node_forwards (nth insert_update insert_i)) insert_i))) (alter-var-root (var main_node_forwards) (fn [_] (assoc-in main_node_forwards [(nth insert_update insert_i) insert_i] insert_idx))) (set! insert_i (+ insert_i 1)))) (alter-var-root (var main_node_forwards) (fn [_] (conj main_node_forwards insert_forwards)))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn find [find_key]
  (binding [find_i nil find_x nil] (try (do (set! find_x 0) (set! find_i (- main_level 1)) (while (>= find_i 0) (do (while (and (not= (nth (nth main_node_forwards find_x) find_i) main_NIL) (< (nth main_node_keys (nth (nth main_node_forwards find_x) find_i)) find_key)) (set! find_x (nth (nth main_node_forwards find_x) find_i))) (set! find_i (- find_i 1)))) (set! find_x (nth (nth main_node_forwards find_x) 0)) (if (and (not= find_x main_NIL) (= (nth main_node_keys find_x) find_key)) (nth main_node_vals find_x) (- 1))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn delete [delete_key]
  (binding [delete_i nil delete_update nil delete_x nil] (try (do (set! delete_update []) (set! delete_i 0) (while (< delete_i main_MAX_LEVEL) (do (set! delete_update (conj delete_update 0)) (set! delete_i (+ delete_i 1)))) (set! delete_x 0) (set! delete_i (- main_level 1)) (while (>= delete_i 0) (do (while (and (not= (nth (nth main_node_forwards delete_x) delete_i) main_NIL) (< (nth main_node_keys (nth (nth main_node_forwards delete_x) delete_i)) delete_key)) (set! delete_x (nth (nth main_node_forwards delete_x) delete_i))) (set! delete_update (assoc delete_update delete_i delete_x)) (set! delete_i (- delete_i 1)))) (set! delete_x (nth (nth main_node_forwards delete_x) 0)) (when (or (= delete_x main_NIL) (not= (nth main_node_keys delete_x) delete_key)) (throw (ex-info "return" {:v nil}))) (set! delete_i 0) (while (< delete_i main_level) (do (when (= (nth (nth main_node_forwards (nth delete_update delete_i)) delete_i) delete_x) (alter-var-root (var main_node_forwards) (fn [_] (assoc-in main_node_forwards [(nth delete_update delete_i) delete_i] (nth (nth main_node_forwards delete_x) delete_i))))) (set! delete_i (+ delete_i 1)))) (while (and (> main_level 1) (= (nth (nth main_node_forwards 0) (- main_level 1)) main_NIL)) (alter-var-root (var main_level) (fn [_] (- main_level 1))))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn to_string []
  (binding [to_string_s nil to_string_x nil] (try (do (set! to_string_s "") (set! to_string_x (nth (nth main_node_forwards 0) 0)) (while (not= to_string_x main_NIL) (do (when (not= to_string_s "") (set! to_string_s (str to_string_s " -> "))) (set! to_string_s (str (str (str to_string_s (str (nth main_node_keys to_string_x))) ":") (str (nth main_node_vals to_string_x)))) (set! to_string_x (nth (nth main_node_forwards to_string_x) 0)))) (throw (ex-info "return" {:v to_string_s}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (do (init) (insert 2 2) (insert 4 4) (insert 6 4) (insert 4 5) (insert 8 4) (insert 9 4) (delete 4) (println (to_string))))

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
