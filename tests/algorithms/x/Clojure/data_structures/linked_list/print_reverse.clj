(ns main (:refer-clojure :exclude [empty_list append_value extend_list to_string make_linked_list in_reverse main]))

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

(declare empty_list append_value extend_list to_string make_linked_list in_reverse main)

(def ^:dynamic append_value_d nil)

(def ^:dynamic extend_list_i nil)

(def ^:dynamic extend_list_result nil)

(def ^:dynamic in_reverse_i nil)

(def ^:dynamic in_reverse_s nil)

(def ^:dynamic main_linked_list nil)

(def ^:dynamic make_linked_list_ll nil)

(def ^:dynamic to_string_i nil)

(def ^:dynamic to_string_s nil)

(defn empty_list []
  (try (throw (ex-info "return" {:v {:data []}})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn append_value [append_value_list append_value_value]
  (binding [append_value_d nil] (try (do (set! append_value_d (:data append_value_list)) (set! append_value_d (conj append_value_d append_value_value)) (throw (ex-info "return" {:v {:data append_value_d}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn extend_list [extend_list_list extend_list_items]
  (binding [extend_list_i nil extend_list_result nil] (try (do (set! extend_list_result extend_list_list) (set! extend_list_i 0) (while (< extend_list_i (count extend_list_items)) (do (set! extend_list_result (append_value extend_list_result (nth extend_list_items extend_list_i))) (set! extend_list_i (+ extend_list_i 1)))) (throw (ex-info "return" {:v extend_list_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn to_string [to_string_list]
  (binding [to_string_i nil to_string_s nil] (try (do (when (= (count (:data to_string_list)) 0) (throw (ex-info "return" {:v ""}))) (set! to_string_s (str (get (:data to_string_list) 0))) (set! to_string_i 1) (while (< to_string_i (count (:data to_string_list))) (do (set! to_string_s (str (str to_string_s " -> ") (str (get (:data to_string_list) to_string_i)))) (set! to_string_i (+ to_string_i 1)))) (throw (ex-info "return" {:v to_string_s}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn make_linked_list [make_linked_list_items]
  (binding [make_linked_list_ll nil] (try (do (when (= (count make_linked_list_items) 0) (throw (Exception. "The Elements List is empty"))) (set! make_linked_list_ll (empty_list)) (set! make_linked_list_ll (extend_list make_linked_list_ll make_linked_list_items)) (throw (ex-info "return" {:v make_linked_list_ll}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn in_reverse [in_reverse_list]
  (binding [in_reverse_i nil in_reverse_s nil] (try (do (when (= (count (:data in_reverse_list)) 0) (throw (ex-info "return" {:v ""}))) (set! in_reverse_i (- (count (:data in_reverse_list)) 1)) (set! in_reverse_s (str (get (:data in_reverse_list) in_reverse_i))) (set! in_reverse_i (- in_reverse_i 1)) (while (>= in_reverse_i 0) (do (set! in_reverse_s (str (str in_reverse_s " <- ") (str (get (:data in_reverse_list) in_reverse_i)))) (set! in_reverse_i (- in_reverse_i 1)))) (throw (ex-info "return" {:v in_reverse_s}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_linked_list nil] (do (set! main_linked_list (make_linked_list [14 52 14 12 43])) (println (str "Linked List:  " (to_string main_linked_list))) (println (str "Reverse List: " (in_reverse main_linked_list))))))

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
