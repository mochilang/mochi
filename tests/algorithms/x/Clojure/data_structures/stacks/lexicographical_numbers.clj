(ns main (:refer-clojure :exclude [lexical_order join_ints]))

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

(declare lexical_order join_ints)

(def ^:dynamic join_ints_i nil)

(def ^:dynamic join_ints_res nil)

(def ^:dynamic lexical_order_idx nil)

(def ^:dynamic lexical_order_num nil)

(def ^:dynamic lexical_order_result nil)

(def ^:dynamic lexical_order_stack nil)

(defn lexical_order [lexical_order_max_number]
  (binding [lexical_order_idx nil lexical_order_num nil lexical_order_result nil lexical_order_stack nil] (try (do (set! lexical_order_result []) (set! lexical_order_stack [1]) (loop [while_flag_1 true] (when (and while_flag_1 (> (count lexical_order_stack) 0)) (do (set! lexical_order_idx (- (count lexical_order_stack) 1)) (set! lexical_order_num (nth lexical_order_stack lexical_order_idx)) (set! lexical_order_stack (subvec lexical_order_stack 0 lexical_order_idx)) (cond (> lexical_order_num lexical_order_max_number) (recur true) :else (do (set! lexical_order_result (conj lexical_order_result lexical_order_num)) (when (not= (mod lexical_order_num 10) 9) (set! lexical_order_stack (conj lexical_order_stack (+ lexical_order_num 1)))) (set! lexical_order_stack (conj lexical_order_stack (* lexical_order_num 10))) (recur while_flag_1)))))) (throw (ex-info "return" {:v lexical_order_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn join_ints [join_ints_xs]
  (binding [join_ints_i nil join_ints_res nil] (try (do (set! join_ints_res "") (set! join_ints_i 0) (while (< join_ints_i (count join_ints_xs)) (do (when (> join_ints_i 0) (set! join_ints_res (str join_ints_res " "))) (set! join_ints_res (str join_ints_res (str (nth join_ints_xs join_ints_i)))) (set! join_ints_i (+ join_ints_i 1)))) (throw (ex-info "return" {:v join_ints_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (join_ints (lexical_order 13)))
      (println (str (lexical_order 1)))
      (println (join_ints (lexical_order 20)))
      (println (join_ints (lexical_order 25)))
      (println (str (lexical_order 12)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
