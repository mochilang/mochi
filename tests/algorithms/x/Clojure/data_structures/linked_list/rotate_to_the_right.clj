(ns main (:refer-clojure :exclude [list_to_string insert_node rotate_to_the_right main]))

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

(declare list_to_string insert_node rotate_to_the_right main)

(def ^:dynamic list_to_string_i nil)

(def ^:dynamic list_to_string_s nil)

(def ^:dynamic main_head nil)

(def ^:dynamic main_new_head nil)

(def ^:dynamic main_places nil)

(def ^:dynamic rotate_to_the_right_i nil)

(def ^:dynamic rotate_to_the_right_j nil)

(def ^:dynamic rotate_to_the_right_k nil)

(def ^:dynamic rotate_to_the_right_n nil)

(def ^:dynamic rotate_to_the_right_res nil)

(def ^:dynamic rotate_to_the_right_split nil)

(defn list_to_string [list_to_string_xs]
  (binding [list_to_string_i nil list_to_string_s nil] (try (do (when (= (count list_to_string_xs) 0) (throw (ex-info "return" {:v ""}))) (set! list_to_string_s (str (nth list_to_string_xs 0))) (set! list_to_string_i 1) (while (< list_to_string_i (count list_to_string_xs)) (do (set! list_to_string_s (str (str list_to_string_s "->") (str (nth list_to_string_xs list_to_string_i)))) (set! list_to_string_i (+ list_to_string_i 1)))) (throw (ex-info "return" {:v list_to_string_s}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn insert_node [insert_node_xs insert_node_data]
  (try (throw (ex-info "return" {:v (conj insert_node_xs insert_node_data)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn rotate_to_the_right [rotate_to_the_right_xs rotate_to_the_right_places]
  (binding [rotate_to_the_right_i nil rotate_to_the_right_j nil rotate_to_the_right_k nil rotate_to_the_right_n nil rotate_to_the_right_res nil rotate_to_the_right_split nil] (try (do (when (= (count rotate_to_the_right_xs) 0) (throw (Exception. "The linked list is empty."))) (set! rotate_to_the_right_n (count rotate_to_the_right_xs)) (set! rotate_to_the_right_k (mod rotate_to_the_right_places rotate_to_the_right_n)) (when (= rotate_to_the_right_k 0) (throw (ex-info "return" {:v rotate_to_the_right_xs}))) (set! rotate_to_the_right_split (- rotate_to_the_right_n rotate_to_the_right_k)) (set! rotate_to_the_right_res []) (set! rotate_to_the_right_i rotate_to_the_right_split) (while (< rotate_to_the_right_i rotate_to_the_right_n) (do (set! rotate_to_the_right_res (conj rotate_to_the_right_res (nth rotate_to_the_right_xs rotate_to_the_right_i))) (set! rotate_to_the_right_i (+ rotate_to_the_right_i 1)))) (set! rotate_to_the_right_j 0) (while (< rotate_to_the_right_j rotate_to_the_right_split) (do (set! rotate_to_the_right_res (conj rotate_to_the_right_res (nth rotate_to_the_right_xs rotate_to_the_right_j))) (set! rotate_to_the_right_j (+ rotate_to_the_right_j 1)))) (throw (ex-info "return" {:v rotate_to_the_right_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_head nil main_new_head nil main_places nil] (do (set! main_head []) (set! main_head (insert_node main_head 5)) (set! main_head (insert_node main_head 1)) (set! main_head (insert_node main_head 2)) (set! main_head (insert_node main_head 4)) (set! main_head (insert_node main_head 3)) (println (str "Original list: " (list_to_string main_head))) (set! main_places 3) (set! main_new_head (rotate_to_the_right main_head main_places)) (println (str (str (str "After " (str main_places)) " iterations: ") (list_to_string main_new_head))))))

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
