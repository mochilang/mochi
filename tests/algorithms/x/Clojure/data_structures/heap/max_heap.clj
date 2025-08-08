(ns main (:refer-clojure :exclude [swap_up insert swap_down shrink pop get_list len]))

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

(declare swap_up insert swap_down shrink pop get_list len)

(def ^:dynamic get_list_i nil)

(def ^:dynamic get_list_out nil)

(def ^:dynamic pop_max_value nil)

(def ^:dynamic shrink_i nil)

(def ^:dynamic shrink_new_heap nil)

(def ^:dynamic swap_down_bigger_child nil)

(def ^:dynamic swap_down_idx nil)

(def ^:dynamic swap_down_temp nil)

(def ^:dynamic swap_up_idx nil)

(def ^:dynamic swap_up_temp nil)

(def ^:dynamic main_heap [0])

(def ^:dynamic main_size 0)

(defn swap_up [swap_up_i]
  (binding [swap_up_idx nil swap_up_temp nil] (do (set! swap_up_temp (nth main_heap swap_up_i)) (set! swap_up_idx swap_up_i) (while (> (quot swap_up_idx 2) 0) (do (when (> (nth main_heap swap_up_idx) (nth main_heap (quot swap_up_idx 2))) (do (alter-var-root (var main_heap) (fn [_] (assoc main_heap swap_up_idx (nth main_heap (quot swap_up_idx 2))))) (alter-var-root (var main_heap) (fn [_] (assoc main_heap (quot swap_up_idx 2) swap_up_temp))))) (set! swap_up_idx (quot swap_up_idx 2)))))))

(defn insert [insert_value]
  (do (alter-var-root (var main_heap) (fn [_] (conj main_heap insert_value))) (alter-var-root (var main_size) (fn [_] (+ main_size 1))) (swap_up main_size)))

(defn swap_down [swap_down_i]
  (binding [swap_down_bigger_child nil swap_down_idx nil swap_down_temp nil] (do (set! swap_down_idx swap_down_i) (while (>= main_size (* 2 swap_down_idx)) (do (set! swap_down_bigger_child (if (> (+ (* 2 swap_down_idx) 1) main_size) (* 2 swap_down_idx) (if (> (nth main_heap (* 2 swap_down_idx)) (nth main_heap (+ (* 2 swap_down_idx) 1))) (* 2 swap_down_idx) (+ (* 2 swap_down_idx) 1)))) (set! swap_down_temp (nth main_heap swap_down_idx)) (when (< (nth main_heap swap_down_idx) (nth main_heap swap_down_bigger_child)) (do (alter-var-root (var main_heap) (fn [_] (assoc main_heap swap_down_idx (nth main_heap swap_down_bigger_child)))) (alter-var-root (var main_heap) (fn [_] (assoc main_heap swap_down_bigger_child swap_down_temp))))) (set! swap_down_idx swap_down_bigger_child))))))

(defn shrink []
  (binding [shrink_i nil shrink_new_heap nil] (do (set! shrink_new_heap []) (set! shrink_i 0) (while (<= shrink_i main_size) (do (set! shrink_new_heap (conj shrink_new_heap (nth main_heap shrink_i))) (set! shrink_i (+ shrink_i 1)))) (alter-var-root (var main_heap) (fn [_] shrink_new_heap)))))

(defn pop []
  (binding [pop_max_value nil] (try (do (set! pop_max_value (nth main_heap 1)) (alter-var-root (var main_heap) (fn [_] (assoc main_heap 1 (nth main_heap main_size)))) (alter-var-root (var main_size) (fn [_] (- main_size 1))) (shrink) (swap_down 1) (throw (ex-info "return" {:v pop_max_value}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn get_list []
  (binding [get_list_i nil get_list_out nil] (try (do (set! get_list_out []) (set! get_list_i 1) (while (<= get_list_i main_size) (do (set! get_list_out (conj get_list_out (nth main_heap get_list_i))) (set! get_list_i (+ get_list_i 1)))) (throw (ex-info "return" {:v get_list_out}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn len []
  (try (throw (ex-info "return" {:v main_size})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (insert 6)
      (insert 10)
      (insert 15)
      (insert 12)
      (println (pop))
      (println (pop))
      (println (get_list))
      (println (len))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
