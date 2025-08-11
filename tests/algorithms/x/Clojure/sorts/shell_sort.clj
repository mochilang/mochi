(ns main (:refer-clojure :exclude [shell_sort]))

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

(declare shell_sort)

(def ^:dynamic shell_sort_arr nil)

(def ^:dynamic shell_sort_g nil)

(def ^:dynamic shell_sort_gap nil)

(def ^:dynamic shell_sort_gaps nil)

(def ^:dynamic shell_sort_i nil)

(def ^:dynamic shell_sort_insert_value nil)

(def ^:dynamic shell_sort_j nil)

(defn shell_sort [shell_sort_collection]
  (binding [shell_sort_arr nil shell_sort_g nil shell_sort_gap nil shell_sort_gaps nil shell_sort_i nil shell_sort_insert_value nil shell_sort_j nil] (try (do (set! shell_sort_arr shell_sort_collection) (set! shell_sort_gaps [701 301 132 57 23 10 4 1]) (set! shell_sort_g 0) (while (< shell_sort_g (count shell_sort_gaps)) (do (set! shell_sort_gap (nth shell_sort_gaps shell_sort_g)) (set! shell_sort_i shell_sort_gap) (while (< shell_sort_i (count shell_sort_arr)) (do (set! shell_sort_insert_value (nth shell_sort_arr shell_sort_i)) (set! shell_sort_j shell_sort_i) (while (and (>= shell_sort_j shell_sort_gap) (> (nth shell_sort_arr (- shell_sort_j shell_sort_gap)) shell_sort_insert_value)) (do (set! shell_sort_arr (assoc shell_sort_arr shell_sort_j (nth shell_sort_arr (- shell_sort_j shell_sort_gap)))) (set! shell_sort_j (- shell_sort_j shell_sort_gap)))) (when (not= shell_sort_j shell_sort_i) (set! shell_sort_arr (assoc shell_sort_arr shell_sort_j shell_sort_insert_value))) (set! shell_sort_i (+ shell_sort_i 1)))) (set! shell_sort_g (+ shell_sort_g 1)))) (throw (ex-info "return" {:v shell_sort_arr}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (shell_sort [0 5 3 2 2])))
      (println (str (shell_sort [])))
      (println (str (shell_sort [(- 2) (- 5) (- 45)])))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
