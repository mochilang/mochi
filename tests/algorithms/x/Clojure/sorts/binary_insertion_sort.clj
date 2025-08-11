(ns main (:refer-clojure :exclude [binary_insertion_sort main]))

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

(declare binary_insertion_sort main)

(def ^:dynamic binary_insertion_sort_arr nil)

(def ^:dynamic binary_insertion_sort_high nil)

(def ^:dynamic binary_insertion_sort_i nil)

(def ^:dynamic binary_insertion_sort_j nil)

(def ^:dynamic binary_insertion_sort_low nil)

(def ^:dynamic binary_insertion_sort_mid nil)

(def ^:dynamic binary_insertion_sort_value nil)

(def ^:dynamic main_example1 nil)

(def ^:dynamic main_example2 nil)

(def ^:dynamic main_example3 nil)

(defn binary_insertion_sort [binary_insertion_sort_arr_p]
  (binding [binary_insertion_sort_arr nil binary_insertion_sort_high nil binary_insertion_sort_i nil binary_insertion_sort_j nil binary_insertion_sort_low nil binary_insertion_sort_mid nil binary_insertion_sort_value nil] (try (do (set! binary_insertion_sort_arr binary_insertion_sort_arr_p) (set! binary_insertion_sort_i 1) (while (< binary_insertion_sort_i (count binary_insertion_sort_arr)) (do (set! binary_insertion_sort_value (nth binary_insertion_sort_arr binary_insertion_sort_i)) (set! binary_insertion_sort_low 0) (set! binary_insertion_sort_high (- binary_insertion_sort_i 1)) (while (<= binary_insertion_sort_low binary_insertion_sort_high) (do (set! binary_insertion_sort_mid (/ (+ binary_insertion_sort_low binary_insertion_sort_high) 2)) (if (< binary_insertion_sort_value (nth binary_insertion_sort_arr binary_insertion_sort_mid)) (set! binary_insertion_sort_high (- binary_insertion_sort_mid 1)) (set! binary_insertion_sort_low (+ binary_insertion_sort_mid 1))))) (set! binary_insertion_sort_j binary_insertion_sort_i) (while (> binary_insertion_sort_j binary_insertion_sort_low) (do (set! binary_insertion_sort_arr (assoc binary_insertion_sort_arr binary_insertion_sort_j (nth binary_insertion_sort_arr (- binary_insertion_sort_j 1)))) (set! binary_insertion_sort_j (- binary_insertion_sort_j 1)))) (set! binary_insertion_sort_arr (assoc binary_insertion_sort_arr binary_insertion_sort_low binary_insertion_sort_value)) (set! binary_insertion_sort_i (+ binary_insertion_sort_i 1)))) (throw (ex-info "return" {:v binary_insertion_sort_arr}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_example1 nil main_example2 nil main_example3 nil] (do (set! main_example1 [5 2 4 6 1 3]) (println (str (binary_insertion_sort main_example1))) (set! main_example2 []) (println (str (binary_insertion_sort main_example2))) (set! main_example3 [4 2 4 1 3]) (println (str (binary_insertion_sort main_example3))))))

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
