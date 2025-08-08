(ns main (:refer-clojure :exclude [rand random get_nodes transition get_transitions main]))

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

(declare rand random get_nodes transition get_transitions main)

(def ^:dynamic count_v nil)

(def ^:dynamic get_nodes_nodes nil)

(def ^:dynamic get_nodes_seen nil)

(def ^:dynamic get_transitions_i nil)

(def ^:dynamic get_transitions_node nil)

(def ^:dynamic get_transitions_one nil)

(def ^:dynamic get_transitions_visited nil)

(def ^:dynamic main_result nil)

(def ^:dynamic main_transitions nil)

(def ^:dynamic transition_current_probability nil)

(def ^:dynamic transition_random_value nil)

(def ^:dynamic main_seed 1)

(defn rand []
  (try (do (alter-var-root (var main_seed) (fn [_] (mod (+ (* main_seed 1103515245) 12345) 2147483648))) (throw (ex-info "return" {:v main_seed}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn random []
  (try (throw (ex-info "return" {:v (/ (* 1.0 (rand)) 2147483648.0)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn get_nodes [get_nodes_trans]
  (binding [get_nodes_nodes nil get_nodes_seen nil] (try (do (set! get_nodes_seen {}) (doseq [t get_nodes_trans] (do (set! get_nodes_seen (assoc get_nodes_seen (:src t) true)) (set! get_nodes_seen (assoc get_nodes_seen (:dst t) true)))) (set! get_nodes_nodes []) (doseq [k (keys get_nodes_seen)] (set! get_nodes_nodes (conj get_nodes_nodes k))) (throw (ex-info "return" {:v get_nodes_nodes}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn transition [transition_current transition_trans]
  (binding [transition_current_probability nil transition_random_value nil] (try (do (set! transition_current_probability 0.0) (set! transition_random_value (random)) (doseq [t transition_trans] (when (= (:src t) transition_current) (do (set! transition_current_probability (+ transition_current_probability (:prob t))) (when (> transition_current_probability transition_random_value) (throw (ex-info "return" {:v (:dst t)})))))) (throw (ex-info "return" {:v ""}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn get_transitions [get_transitions_start get_transitions_trans get_transitions_steps]
  (binding [count_v nil get_transitions_i nil get_transitions_node nil get_transitions_one nil get_transitions_visited nil] (try (do (set! get_transitions_visited {}) (doseq [node (get_nodes get_transitions_trans)] (do (set! get_transitions_one 1) (set! get_transitions_visited (assoc get_transitions_visited node get_transitions_one)))) (set! get_transitions_node get_transitions_start) (set! get_transitions_i 0) (while (< get_transitions_i get_transitions_steps) (do (set! get_transitions_node (transition get_transitions_node get_transitions_trans)) (set! count_v (get get_transitions_visited get_transitions_node)) (set! count_v (+ count_v 1)) (set! get_transitions_visited (assoc get_transitions_visited get_transitions_node count_v)) (set! get_transitions_i (+ get_transitions_i 1)))) (throw (ex-info "return" {:v get_transitions_visited}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_result nil main_transitions nil] (do (set! main_transitions [{:dst "a" :prob 0.9 :src "a"} {:dst "b" :prob 0.075 :src "a"} {:dst "c" :prob 0.025 :src "a"} {:dst "a" :prob 0.15 :src "b"} {:dst "b" :prob 0.8 :src "b"} {:dst "c" :prob 0.05 :src "b"} {:dst "a" :prob 0.25 :src "c"} {:dst "b" :prob 0.25 :src "c"} {:dst "c" :prob 0.5 :src "c"}]) (set! main_result (get_transitions "a" main_transitions 5000)) (println (str (str (str (str (str (get main_result "a")) " ") (str (get main_result "b"))) " ") (str (get main_result "c")))))))

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
