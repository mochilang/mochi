(ns main (:refer-clojure :exclude [digit to_int split serialize build deserialize five_tree main]))

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

(declare digit to_int split serialize build deserialize five_tree main)

(def ^:dynamic build_left_res nil)

(def ^:dynamic build_node nil)

(def ^:dynamic build_right_res nil)

(def ^:dynamic build_value nil)

(def ^:dynamic deserialize_nodes nil)

(def ^:dynamic deserialize_res nil)

(def ^:dynamic digit_digits nil)

(def ^:dynamic digit_i nil)

(def ^:dynamic five_tree_left_child nil)

(def ^:dynamic five_tree_right_child nil)

(def ^:dynamic five_tree_right_left nil)

(def ^:dynamic five_tree_right_right nil)

(def ^:dynamic main_rebuilt nil)

(def ^:dynamic main_root nil)

(def ^:dynamic main_serial nil)

(def ^:dynamic main_serial2 nil)

(def ^:dynamic split_ch nil)

(def ^:dynamic split_current nil)

(def ^:dynamic split_i nil)

(def ^:dynamic split_res nil)

(def ^:dynamic to_int_ch nil)

(def ^:dynamic to_int_i nil)

(def ^:dynamic to_int_num nil)

(def ^:dynamic to_int_sign nil)

(defn digit [digit_ch]
  (binding [digit_digits nil digit_i nil] (try (do (set! digit_digits "0123456789") (set! digit_i 0) (while (< digit_i (count digit_digits)) (do (when (= (subs digit_digits digit_i (min (+ digit_i 1) (count digit_digits))) digit_ch) (throw (ex-info "return" {:v digit_i}))) (set! digit_i (+ digit_i 1)))) (throw (ex-info "return" {:v 0}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn to_int [to_int_s]
  (binding [to_int_ch nil to_int_i nil to_int_num nil to_int_sign nil] (try (do (set! to_int_i 0) (set! to_int_sign 1) (when (and (> (count to_int_s) 0) (= (subs to_int_s 0 (min 1 (count to_int_s))) "-")) (do (set! to_int_sign (- 1)) (set! to_int_i 1))) (set! to_int_num 0) (while (< to_int_i (count to_int_s)) (do (set! to_int_ch (subs to_int_s to_int_i (min (+ to_int_i 1) (count to_int_s)))) (set! to_int_num (+ (* to_int_num 10) (digit to_int_ch))) (set! to_int_i (+ to_int_i 1)))) (throw (ex-info "return" {:v (* to_int_sign to_int_num)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn split [split_s split_sep]
  (binding [split_ch nil split_current nil split_i nil split_res nil] (try (do (set! split_res []) (set! split_current "") (set! split_i 0) (while (< split_i (count split_s)) (do (set! split_ch (subs split_s split_i (min (+ split_i 1) (count split_s)))) (if (= split_ch split_sep) (do (set! split_res (conj split_res split_current)) (set! split_current "")) (set! split_current (str split_current split_ch))) (set! split_i (+ split_i 1)))) (set! split_res (conj split_res split_current)) (throw (ex-info "return" {:v split_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn serialize [serialize_node]
  (try (throw (ex-info "return" {:v (cond (= serialize_node serialize_Empty) "null" (and (map? serialize_node) (= (:__tag serialize_node) "Node") (contains? serialize_node :left) (contains? serialize_node :value) (contains? serialize_node :right)) (let [serialize_l (:left serialize_node) serialize_v (:value serialize_node) serialize_r (:right serialize_node)] (str (str (str (str (str serialize_v) ",") (serialize serialize_l)) ",") (serialize serialize_r))))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn build [build_nodes build_idx]
  (binding [build_left_res nil build_node nil build_right_res nil build_value nil] (try (do (set! build_value (nth build_nodes build_idx)) (when (= build_value "null") (throw (ex-info "return" {:v {:node {:__tag "Empty"} :next (+ build_idx 1)}}))) (set! build_left_res (build build_nodes (+ build_idx 1))) (set! build_right_res (build build_nodes (:next build_left_res))) (set! build_node {:__tag "Node" :left (:node build_left_res) :value (to_int build_value) :right (:node build_right_res)}) (throw (ex-info "return" {:v {:node build_node :next (:next build_right_res)}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn deserialize [deserialize_data]
  (binding [deserialize_nodes nil deserialize_res nil] (try (do (set! deserialize_nodes (split deserialize_data ",")) (set! deserialize_res (build deserialize_nodes 0)) (throw (ex-info "return" {:v (:node deserialize_res)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn five_tree []
  (binding [five_tree_left_child nil five_tree_right_child nil five_tree_right_left nil five_tree_right_right nil] (try (do (set! five_tree_left_child {:__tag "Node" :left 2 :value {:__tag "Empty"} :right {:__tag "Empty"}}) (set! five_tree_right_left {:__tag "Node" :left 4 :value {:__tag "Empty"} :right {:__tag "Empty"}}) (set! five_tree_right_right {:__tag "Node" :left 5 :value {:__tag "Empty"} :right {:__tag "Empty"}}) (set! five_tree_right_child {:__tag "Node" :left 3 :value five_tree_right_left :right five_tree_right_right}) (throw (ex-info "return" {:v {:__tag "Node" :left 1 :value five_tree_left_child :right five_tree_right_child}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_rebuilt nil main_root nil main_serial nil main_serial2 nil] (do (set! main_root (five_tree)) (set! main_serial (serialize main_root)) (println main_serial) (set! main_rebuilt (deserialize main_serial)) (set! main_serial2 (serialize main_rebuilt)) (println main_serial2) (println (= main_serial main_serial2)))))

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
