(ns main (:refer-clojure :exclude [left_child right_child build update_recursive update query_recursive query show_data main]))

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

(declare left_child right_child build update_recursive update query_recursive query show_data main)

(def ^:dynamic build_left_val nil)

(def ^:dynamic build_mid nil)

(def ^:dynamic build_right_val nil)

(def ^:dynamic build_st nil)

(def ^:dynamic main_A nil)

(def ^:dynamic main_N nil)

(def ^:dynamic main_i nil)

(def ^:dynamic main_st nil)

(def ^:dynamic query_recursive_mid nil)

(def ^:dynamic query_recursive_q1 nil)

(def ^:dynamic query_recursive_q2 nil)

(def ^:dynamic show_data_i nil)

(def ^:dynamic show_data_show_list nil)

(def ^:dynamic update_recursive_left_val nil)

(def ^:dynamic update_recursive_mid nil)

(def ^:dynamic update_recursive_right_val nil)

(def ^:dynamic update_recursive_st nil)

(def ^:dynamic main_A [])

(def ^:dynamic main_N 0)

(def ^:dynamic main_st [])

(defn left_child [left_child_idx]
  (try (throw (ex-info "return" {:v (* left_child_idx 2)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn right_child [right_child_idx]
  (try (throw (ex-info "return" {:v (+ (* right_child_idx 2) 1)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn build [build_idx build_left build_right]
  (binding [build_left_val nil build_mid nil build_right_val nil build_st nil] (if (= build_left build_right) (set! build_st (assoc build_st build_idx (nth main_A build_left))) (do (set! build_mid (quot (+ build_left build_right) 2)) (build (left_child build_idx) build_left build_mid) (build (right_child build_idx) (+ build_mid 1) build_right) (set! build_left_val (nth build_st (left_child build_idx))) (set! build_right_val (nth build_st (right_child build_idx))) (set! build_st (assoc build_st build_idx (if (> build_left_val build_right_val) build_left_val build_right_val)))))))

(defn update_recursive [update_recursive_idx update_recursive_left update_recursive_right update_recursive_a update_recursive_b update_recursive_val]
  (binding [update_recursive_left_val nil update_recursive_mid nil update_recursive_right_val nil update_recursive_st nil] (try (do (when (or (< update_recursive_right update_recursive_a) (> update_recursive_left update_recursive_b)) (throw (ex-info "return" {:v true}))) (when (= update_recursive_left update_recursive_right) (do (set! update_recursive_st (assoc update_recursive_st update_recursive_idx update_recursive_val)) (throw (ex-info "return" {:v true})))) (set! update_recursive_mid (quot (+ update_recursive_left update_recursive_right) 2)) (update_recursive (left_child update_recursive_idx) update_recursive_left update_recursive_mid update_recursive_a update_recursive_b update_recursive_val) (update_recursive (right_child update_recursive_idx) (+ update_recursive_mid 1) update_recursive_right update_recursive_a update_recursive_b update_recursive_val) (set! update_recursive_left_val (nth update_recursive_st (left_child update_recursive_idx))) (set! update_recursive_right_val (nth update_recursive_st (right_child update_recursive_idx))) (set! update_recursive_st (assoc update_recursive_st update_recursive_idx (if (> update_recursive_left_val update_recursive_right_val) update_recursive_left_val update_recursive_right_val))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn update [update_a update_b update_val]
  (try (throw (ex-info "return" {:v (update_recursive 1 0 (- main_N 1) (- update_a 1) (- update_b 1) update_val)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(def ^:dynamic main_NEG_INF (- 1000000000))

(defn query_recursive [query_recursive_idx query_recursive_left query_recursive_right query_recursive_a query_recursive_b]
  (binding [query_recursive_mid nil query_recursive_q1 nil query_recursive_q2 nil] (try (do (when (or (< query_recursive_right query_recursive_a) (> query_recursive_left query_recursive_b)) (throw (ex-info "return" {:v main_NEG_INF}))) (when (and (>= query_recursive_left query_recursive_a) (<= query_recursive_right query_recursive_b)) (throw (ex-info "return" {:v (nth main_st query_recursive_idx)}))) (set! query_recursive_mid (quot (+ query_recursive_left query_recursive_right) 2)) (set! query_recursive_q1 (query_recursive (left_child query_recursive_idx) query_recursive_left query_recursive_mid query_recursive_a query_recursive_b)) (set! query_recursive_q2 (query_recursive (right_child query_recursive_idx) (+ query_recursive_mid 1) query_recursive_right query_recursive_a query_recursive_b)) (throw (ex-info "return" {:v (if (> query_recursive_q1 query_recursive_q2) query_recursive_q1 query_recursive_q2)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn query [query_a query_b]
  (try (throw (ex-info "return" {:v (query_recursive 1 0 (- main_N 1) (- query_a 1) (- query_b 1))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn show_data []
  (binding [show_data_i nil show_data_show_list nil] (do (set! show_data_i 0) (set! show_data_show_list []) (while (< show_data_i main_N) (do (set! show_data_show_list (conj show_data_show_list (query (+ show_data_i 1) (+ show_data_i 1)))) (set! show_data_i (+ show_data_i 1)))) (println show_data_show_list))))

(defn main []
  (binding [main_A nil main_N nil main_i nil main_st nil] (do (set! main_A [1 2 (- 4) 7 3 (- 5) 6 11 (- 20) 9 14 15 5 2 (- 8)]) (set! main_N (count main_A)) (set! main_i 0) (while (< main_i (* 4 main_N)) (do (set! main_st (conj main_st 0)) (set! main_i (+ main_i 1)))) (when (> main_N 0) (build 1 0 (- main_N 1))) (println (query 4 6)) (println (query 7 11)) (println (query 7 12)) (update 1 3 111) (println (query 1 15)) (update 7 8 235) (show_data))))

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
