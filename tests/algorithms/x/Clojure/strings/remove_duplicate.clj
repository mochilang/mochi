(ns main (:refer-clojure :exclude [split_ws contains unique insertion_sort join_with_space remove_duplicates]))

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

(declare split_ws contains unique insertion_sort join_with_space remove_duplicates)

(def ^:dynamic contains_i nil)

(def ^:dynamic insertion_sort_a nil)

(def ^:dynamic insertion_sort_i nil)

(def ^:dynamic insertion_sort_j nil)

(def ^:dynamic insertion_sort_key nil)

(def ^:dynamic join_with_space_i nil)

(def ^:dynamic join_with_space_s nil)

(def ^:dynamic remove_duplicates_sorted_words nil)

(def ^:dynamic remove_duplicates_uniq nil)

(def ^:dynamic remove_duplicates_words nil)

(def ^:dynamic split_ws_ch nil)

(def ^:dynamic split_ws_i nil)

(def ^:dynamic split_ws_res nil)

(def ^:dynamic split_ws_word nil)

(def ^:dynamic unique_i nil)

(def ^:dynamic unique_res nil)

(def ^:dynamic unique_w nil)

(defn split_ws [split_ws_s]
  (binding [split_ws_ch nil split_ws_i nil split_ws_res nil split_ws_word nil] (try (do (set! split_ws_res []) (set! split_ws_word "") (set! split_ws_i 0) (while (< split_ws_i (count split_ws_s)) (do (set! split_ws_ch (subs split_ws_s split_ws_i (min (+ split_ws_i 1) (count split_ws_s)))) (if (= split_ws_ch " ") (when (not= split_ws_word "") (do (set! split_ws_res (conj split_ws_res split_ws_word)) (set! split_ws_word ""))) (set! split_ws_word (str split_ws_word split_ws_ch))) (set! split_ws_i (+ split_ws_i 1)))) (when (not= split_ws_word "") (set! split_ws_res (conj split_ws_res split_ws_word))) (throw (ex-info "return" {:v split_ws_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn contains [contains_xs contains_x]
  (binding [contains_i nil] (try (do (set! contains_i 0) (while (< contains_i (count contains_xs)) (do (when (= (nth contains_xs contains_i) contains_x) (throw (ex-info "return" {:v true}))) (set! contains_i (+ contains_i 1)))) (throw (ex-info "return" {:v false}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn unique [unique_xs]
  (binding [unique_i nil unique_res nil unique_w nil] (try (do (set! unique_res []) (set! unique_i 0) (while (< unique_i (count unique_xs)) (do (set! unique_w (nth unique_xs unique_i)) (when (not (contains unique_res unique_w)) (set! unique_res (conj unique_res unique_w))) (set! unique_i (+ unique_i 1)))) (throw (ex-info "return" {:v unique_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn insertion_sort [insertion_sort_arr]
  (binding [insertion_sort_a nil insertion_sort_i nil insertion_sort_j nil insertion_sort_key nil] (try (do (set! insertion_sort_a insertion_sort_arr) (set! insertion_sort_i 1) (while (< insertion_sort_i (count insertion_sort_a)) (do (set! insertion_sort_key (nth insertion_sort_a insertion_sort_i)) (set! insertion_sort_j (- insertion_sort_i 1)) (while (and (>= insertion_sort_j 0) (> (compare (nth insertion_sort_a insertion_sort_j) insertion_sort_key) 0)) (do (set! insertion_sort_a (assoc insertion_sort_a (+ insertion_sort_j 1) (nth insertion_sort_a insertion_sort_j))) (set! insertion_sort_j (- insertion_sort_j 1)))) (set! insertion_sort_a (assoc insertion_sort_a (+ insertion_sort_j 1) insertion_sort_key)) (set! insertion_sort_i (+ insertion_sort_i 1)))) (throw (ex-info "return" {:v insertion_sort_a}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn join_with_space [join_with_space_xs]
  (binding [join_with_space_i nil join_with_space_s nil] (try (do (set! join_with_space_s "") (set! join_with_space_i 0) (while (< join_with_space_i (count join_with_space_xs)) (do (when (> join_with_space_i 0) (set! join_with_space_s (str join_with_space_s " "))) (set! join_with_space_s (str join_with_space_s (nth join_with_space_xs join_with_space_i))) (set! join_with_space_i (+ join_with_space_i 1)))) (throw (ex-info "return" {:v join_with_space_s}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn remove_duplicates [remove_duplicates_sentence]
  (binding [remove_duplicates_sorted_words nil remove_duplicates_uniq nil remove_duplicates_words nil] (try (do (set! remove_duplicates_words (split_ws remove_duplicates_sentence)) (set! remove_duplicates_uniq (unique remove_duplicates_words)) (set! remove_duplicates_sorted_words (insertion_sort remove_duplicates_uniq)) (throw (ex-info "return" {:v (join_with_space remove_duplicates_sorted_words)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (remove_duplicates "Python is great and Java is also great"))
      (println (remove_duplicates "Python   is      great and Java is also great"))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
