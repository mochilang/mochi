(ns main (:refer-clojure :exclude [contains_int contains_string count_int count_string sort_int sort_string mode_int mode_string]))

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

(declare contains_int contains_string count_int count_string sort_int sort_string mode_int mode_string)

(def ^:dynamic contains_int_i nil)

(def ^:dynamic contains_string_i nil)

(def ^:dynamic count_int_cnt nil)

(def ^:dynamic count_int_i nil)

(def ^:dynamic count_string_cnt nil)

(def ^:dynamic count_string_i nil)

(def ^:dynamic mode_int_counts nil)

(def ^:dynamic mode_int_i nil)

(def ^:dynamic mode_int_max_count nil)

(def ^:dynamic mode_int_modes nil)

(def ^:dynamic mode_int_v nil)

(def ^:dynamic mode_string_counts nil)

(def ^:dynamic mode_string_i nil)

(def ^:dynamic mode_string_max_count nil)

(def ^:dynamic mode_string_modes nil)

(def ^:dynamic mode_string_v nil)

(def ^:dynamic sort_int_arr nil)

(def ^:dynamic sort_int_i nil)

(def ^:dynamic sort_int_j nil)

(def ^:dynamic sort_int_tmp nil)

(def ^:dynamic sort_string_arr nil)

(def ^:dynamic sort_string_i nil)

(def ^:dynamic sort_string_j nil)

(def ^:dynamic sort_string_tmp nil)

(defn contains_int [contains_int_xs contains_int_x]
  (binding [contains_int_i nil] (try (do (set! contains_int_i 0) (while (< contains_int_i (count contains_int_xs)) (do (when (= (nth contains_int_xs contains_int_i) contains_int_x) (throw (ex-info "return" {:v true}))) (set! contains_int_i (+ contains_int_i 1)))) (throw (ex-info "return" {:v false}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn contains_string [contains_string_xs contains_string_x]
  (binding [contains_string_i nil] (try (do (set! contains_string_i 0) (while (< contains_string_i (count contains_string_xs)) (do (when (= (nth contains_string_xs contains_string_i) contains_string_x) (throw (ex-info "return" {:v true}))) (set! contains_string_i (+ contains_string_i 1)))) (throw (ex-info "return" {:v false}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn count_int [count_int_xs count_int_x]
  (binding [count_int_cnt nil count_int_i nil] (try (do (set! count_int_cnt 0) (set! count_int_i 0) (while (< count_int_i (count count_int_xs)) (do (when (= (nth count_int_xs count_int_i) count_int_x) (set! count_int_cnt (+ count_int_cnt 1))) (set! count_int_i (+ count_int_i 1)))) (throw (ex-info "return" {:v count_int_cnt}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn count_string [count_string_xs count_string_x]
  (binding [count_string_cnt nil count_string_i nil] (try (do (set! count_string_cnt 0) (set! count_string_i 0) (while (< count_string_i (count count_string_xs)) (do (when (= (nth count_string_xs count_string_i) count_string_x) (set! count_string_cnt (+ count_string_cnt 1))) (set! count_string_i (+ count_string_i 1)))) (throw (ex-info "return" {:v count_string_cnt}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn sort_int [sort_int_xs]
  (binding [sort_int_arr nil sort_int_i nil sort_int_j nil sort_int_tmp nil] (try (do (set! sort_int_arr sort_int_xs) (set! sort_int_i 0) (while (< sort_int_i (count sort_int_arr)) (do (set! sort_int_j (+ sort_int_i 1)) (while (< sort_int_j (count sort_int_arr)) (do (when (< (nth sort_int_arr sort_int_j) (nth sort_int_arr sort_int_i)) (do (set! sort_int_tmp (nth sort_int_arr sort_int_i)) (set! sort_int_arr (assoc sort_int_arr sort_int_i (nth sort_int_arr sort_int_j))) (set! sort_int_arr (assoc sort_int_arr sort_int_j sort_int_tmp)))) (set! sort_int_j (+ sort_int_j 1)))) (set! sort_int_i (+ sort_int_i 1)))) (throw (ex-info "return" {:v sort_int_arr}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn sort_string [sort_string_xs]
  (binding [sort_string_arr nil sort_string_i nil sort_string_j nil sort_string_tmp nil] (try (do (set! sort_string_arr sort_string_xs) (set! sort_string_i 0) (while (< sort_string_i (count sort_string_arr)) (do (set! sort_string_j (+ sort_string_i 1)) (while (< sort_string_j (count sort_string_arr)) (do (when (< (compare (nth sort_string_arr sort_string_j) (nth sort_string_arr sort_string_i)) 0) (do (set! sort_string_tmp (nth sort_string_arr sort_string_i)) (set! sort_string_arr (assoc sort_string_arr sort_string_i (nth sort_string_arr sort_string_j))) (set! sort_string_arr (assoc sort_string_arr sort_string_j sort_string_tmp)))) (set! sort_string_j (+ sort_string_j 1)))) (set! sort_string_i (+ sort_string_i 1)))) (throw (ex-info "return" {:v sort_string_arr}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn mode_int [mode_int_lst]
  (binding [mode_int_counts nil mode_int_i nil mode_int_max_count nil mode_int_modes nil mode_int_v nil] (try (do (when (= (count mode_int_lst) 0) (throw (ex-info "return" {:v []}))) (set! mode_int_counts []) (set! mode_int_i 0) (while (< mode_int_i (count mode_int_lst)) (do (set! mode_int_counts (conj mode_int_counts (count_int mode_int_lst (nth mode_int_lst mode_int_i)))) (set! mode_int_i (+ mode_int_i 1)))) (set! mode_int_max_count 0) (set! mode_int_i 0) (while (< mode_int_i (count mode_int_counts)) (do (when (> (nth mode_int_counts mode_int_i) mode_int_max_count) (set! mode_int_max_count (nth mode_int_counts mode_int_i))) (set! mode_int_i (+ mode_int_i 1)))) (set! mode_int_modes []) (set! mode_int_i 0) (while (< mode_int_i (count mode_int_lst)) (do (when (= (nth mode_int_counts mode_int_i) mode_int_max_count) (do (set! mode_int_v (nth mode_int_lst mode_int_i)) (when (not (contains_int mode_int_modes mode_int_v)) (set! mode_int_modes (conj mode_int_modes mode_int_v))))) (set! mode_int_i (+ mode_int_i 1)))) (throw (ex-info "return" {:v (sort_int mode_int_modes)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn mode_string [mode_string_lst]
  (binding [mode_string_counts nil mode_string_i nil mode_string_max_count nil mode_string_modes nil mode_string_v nil] (try (do (when (= (count mode_string_lst) 0) (throw (ex-info "return" {:v []}))) (set! mode_string_counts []) (set! mode_string_i 0) (while (< mode_string_i (count mode_string_lst)) (do (set! mode_string_counts (conj mode_string_counts (count_string mode_string_lst (nth mode_string_lst mode_string_i)))) (set! mode_string_i (+ mode_string_i 1)))) (set! mode_string_max_count 0) (set! mode_string_i 0) (while (< mode_string_i (count mode_string_counts)) (do (when (> (nth mode_string_counts mode_string_i) mode_string_max_count) (set! mode_string_max_count (nth mode_string_counts mode_string_i))) (set! mode_string_i (+ mode_string_i 1)))) (set! mode_string_modes []) (set! mode_string_i 0) (while (< mode_string_i (count mode_string_lst)) (do (when (= (nth mode_string_counts mode_string_i) mode_string_max_count) (do (set! mode_string_v (nth mode_string_lst mode_string_i)) (when (not (contains_string mode_string_modes mode_string_v)) (set! mode_string_modes (conj mode_string_modes mode_string_v))))) (set! mode_string_i (+ mode_string_i 1)))) (throw (ex-info "return" {:v (sort_string mode_string_modes)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (mode_int [2 3 4 5 3 4 2 5 2 2 4 2 2 2]))
      (println (mode_int [3 4 5 3 4 2 5 2 2 4 4 2 2 2]))
      (println (mode_int [3 4 5 3 4 2 5 2 2 4 4 4 2 2 4 2]))
      (println (mode_string ["x" "y" "y" "z"]))
      (println (mode_string ["x" "x" "y" "y" "z"]))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
