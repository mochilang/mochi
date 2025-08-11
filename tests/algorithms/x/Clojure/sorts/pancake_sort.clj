(ns main (:refer-clojure :exclude [flip find_max_index pancake_sort main]))

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

(defn _fetch [url]
  {:data [{:from "" :intensity {:actual 0 :forecast 0 :index ""} :to ""}]})

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare flip find_max_index pancake_sort main)

(def ^:dynamic find_max_index_i nil)

(def ^:dynamic find_max_index_mi nil)

(def ^:dynamic flip_arr nil)

(def ^:dynamic flip_end nil)

(def ^:dynamic flip_start nil)

(def ^:dynamic flip_temp nil)

(def ^:dynamic main_data nil)

(def ^:dynamic main_sorted nil)

(def ^:dynamic pancake_sort_arr nil)

(def ^:dynamic pancake_sort_cur nil)

(def ^:dynamic pancake_sort_mi nil)

(defn flip [flip_arr_p flip_k]
  (binding [flip_arr nil flip_end nil flip_start nil flip_temp nil] (try (do (set! flip_arr flip_arr_p) (set! flip_start 0) (set! flip_end flip_k) (while (< flip_start flip_end) (do (set! flip_temp (nth flip_arr flip_start)) (set! flip_arr (assoc flip_arr flip_start (nth flip_arr flip_end))) (set! flip_arr (assoc flip_arr flip_end flip_temp)) (set! flip_start (+ flip_start 1)) (set! flip_end (- flip_end 1)))) (throw (ex-info "return" {:v flip_arr}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn find_max_index [find_max_index_arr find_max_index_n]
  (binding [find_max_index_i nil find_max_index_mi nil] (try (do (set! find_max_index_mi 0) (set! find_max_index_i 1) (while (< find_max_index_i find_max_index_n) (do (when (> (nth find_max_index_arr find_max_index_i) (nth find_max_index_arr find_max_index_mi)) (set! find_max_index_mi find_max_index_i)) (set! find_max_index_i (+ find_max_index_i 1)))) (throw (ex-info "return" {:v find_max_index_mi}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn pancake_sort [pancake_sort_arr_p]
  (binding [pancake_sort_arr nil pancake_sort_cur nil pancake_sort_mi nil] (try (do (set! pancake_sort_arr pancake_sort_arr_p) (set! pancake_sort_cur (count pancake_sort_arr)) (while (> pancake_sort_cur 1) (do (set! pancake_sort_mi (find_max_index pancake_sort_arr pancake_sort_cur)) (set! pancake_sort_arr (flip pancake_sort_arr pancake_sort_mi)) (set! pancake_sort_arr (flip pancake_sort_arr (- pancake_sort_cur 1))) (set! pancake_sort_cur (- pancake_sort_cur 1)))) (throw (ex-info "return" {:v pancake_sort_arr}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_data nil main_sorted nil] (do (set! main_data [3 6 1 10 2]) (set! main_sorted (pancake_sort main_data)) (println (str main_sorted)))))

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
