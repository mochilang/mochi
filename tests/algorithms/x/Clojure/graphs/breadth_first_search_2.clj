(ns main (:refer-clojure :exclude [join breadth_first_search breadth_first_search_with_deque]))

(require 'clojure.set)

(defrecord G [A B C D E F])

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(defn indexOf [s sub]
  (let [idx (clojure.string/index-of s sub)] (if (nil? idx) -1 idx)))

(defn split [s sep]
  (clojure.string/split s (re-pattern sep)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(def ^:dynamic breadth_first_search_children nil)

(def ^:dynamic breadth_first_search_explored nil)

(def ^:dynamic breadth_first_search_i nil)

(def ^:dynamic breadth_first_search_queue nil)

(def ^:dynamic breadth_first_search_result nil)

(def ^:dynamic breadth_first_search_v nil)

(def ^:dynamic breadth_first_search_w nil)

(def ^:dynamic breadth_first_search_with_deque_child nil)

(def ^:dynamic breadth_first_search_with_deque_children nil)

(def ^:dynamic breadth_first_search_with_deque_head nil)

(def ^:dynamic breadth_first_search_with_deque_i nil)

(def ^:dynamic breadth_first_search_with_deque_queue nil)

(def ^:dynamic breadth_first_search_with_deque_result nil)

(def ^:dynamic breadth_first_search_with_deque_v nil)

(def ^:dynamic breadth_first_search_with_deque_visited nil)

(def ^:dynamic join_i nil)

(def ^:dynamic join_s nil)

(declare join breadth_first_search breadth_first_search_with_deque)

(defn join [join_xs]
  (binding [join_i nil join_s nil] (try (do (set! join_s "") (set! join_i 0) (while (< join_i (count join_xs)) (do (set! join_s (str join_s (nth join_xs join_i))) (set! join_i (+ join_i 1)))) (throw (ex-info "return" {:v join_s}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn breadth_first_search [breadth_first_search_graph breadth_first_search_start]
  (binding [breadth_first_search_children nil breadth_first_search_explored nil breadth_first_search_i nil breadth_first_search_queue nil breadth_first_search_result nil breadth_first_search_v nil breadth_first_search_w nil] (try (do (set! breadth_first_search_explored {}) (set! breadth_first_search_explored (assoc breadth_first_search_explored breadth_first_search_start true)) (set! breadth_first_search_result [breadth_first_search_start]) (set! breadth_first_search_queue [breadth_first_search_start]) (while (> (count breadth_first_search_queue) 0) (do (set! breadth_first_search_v (nth breadth_first_search_queue 0)) (set! breadth_first_search_queue (subvec breadth_first_search_queue 1 (count breadth_first_search_queue))) (set! breadth_first_search_children (get breadth_first_search_graph breadth_first_search_v)) (set! breadth_first_search_i 0) (while (< breadth_first_search_i (count breadth_first_search_children)) (do (set! breadth_first_search_w (nth breadth_first_search_children breadth_first_search_i)) (when (not (in breadth_first_search_w breadth_first_search_explored)) (do (set! breadth_first_search_explored (assoc breadth_first_search_explored breadth_first_search_w true)) (set! breadth_first_search_result (conj breadth_first_search_result breadth_first_search_w)) (set! breadth_first_search_queue (conj breadth_first_search_queue breadth_first_search_w)))) (set! breadth_first_search_i (+ breadth_first_search_i 1)))))) (throw (ex-info "return" {:v breadth_first_search_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn breadth_first_search_with_deque [breadth_first_search_with_deque_graph breadth_first_search_with_deque_start]
  (binding [breadth_first_search_with_deque_child nil breadth_first_search_with_deque_children nil breadth_first_search_with_deque_head nil breadth_first_search_with_deque_i nil breadth_first_search_with_deque_queue nil breadth_first_search_with_deque_result nil breadth_first_search_with_deque_v nil breadth_first_search_with_deque_visited nil] (try (do (set! breadth_first_search_with_deque_visited {}) (set! breadth_first_search_with_deque_visited (assoc breadth_first_search_with_deque_visited breadth_first_search_with_deque_start true)) (set! breadth_first_search_with_deque_result [breadth_first_search_with_deque_start]) (set! breadth_first_search_with_deque_queue [breadth_first_search_with_deque_start]) (set! breadth_first_search_with_deque_head 0) (while (< breadth_first_search_with_deque_head (count breadth_first_search_with_deque_queue)) (do (set! breadth_first_search_with_deque_v (nth breadth_first_search_with_deque_queue breadth_first_search_with_deque_head)) (set! breadth_first_search_with_deque_head (+ breadth_first_search_with_deque_head 1)) (set! breadth_first_search_with_deque_children (get breadth_first_search_with_deque_graph breadth_first_search_with_deque_v)) (set! breadth_first_search_with_deque_i 0) (while (< breadth_first_search_with_deque_i (count breadth_first_search_with_deque_children)) (do (set! breadth_first_search_with_deque_child (nth breadth_first_search_with_deque_children breadth_first_search_with_deque_i)) (when (not (in breadth_first_search_with_deque_child breadth_first_search_with_deque_visited)) (do (set! breadth_first_search_with_deque_visited (assoc breadth_first_search_with_deque_visited breadth_first_search_with_deque_child true)) (set! breadth_first_search_with_deque_result (conj breadth_first_search_with_deque_result breadth_first_search_with_deque_child)) (set! breadth_first_search_with_deque_queue (conj breadth_first_search_with_deque_queue breadth_first_search_with_deque_child)))) (set! breadth_first_search_with_deque_i (+ breadth_first_search_with_deque_i 1)))))) (throw (ex-info "return" {:v breadth_first_search_with_deque_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_G {"A" ["B" "C"] "B" ["A" "D" "E"] "C" ["A" "F"] "D" ["B"] "E" ["B" "F"] "F" ["C" "E"]})

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (join (breadth_first_search main_G "A")))
      (println (join (breadth_first_search_with_deque main_G "A")))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
