(ns main (:refer-clojure :exclude [shell_sort main]))

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

(declare shell_sort main)

(def ^:dynamic shell_sort_collection nil)

(def ^:dynamic shell_sort_gap nil)

(def ^:dynamic shell_sort_i nil)

(def ^:dynamic shell_sort_j nil)

(def ^:dynamic shell_sort_temp nil)

(def ^:dynamic shell_sort_ten nil)

(def ^:dynamic shell_sort_thirteen nil)

(defn shell_sort [shell_sort_collection_p]
  (binding [shell_sort_collection nil shell_sort_gap nil shell_sort_i nil shell_sort_j nil shell_sort_temp nil shell_sort_ten nil shell_sort_thirteen nil] (try (do (set! shell_sort_collection shell_sort_collection_p) (set! shell_sort_gap (count shell_sort_collection)) (set! shell_sort_ten 10) (set! shell_sort_thirteen 13) (while (> shell_sort_gap 1) (do (set! shell_sort_gap (/ (* shell_sort_gap shell_sort_ten) shell_sort_thirteen)) (set! shell_sort_i shell_sort_gap) (while (< shell_sort_i (count shell_sort_collection)) (do (set! shell_sort_temp (nth shell_sort_collection shell_sort_i)) (set! shell_sort_j shell_sort_i) (while (and (>= shell_sort_j shell_sort_gap) (> (nth shell_sort_collection (- shell_sort_j shell_sort_gap)) shell_sort_temp)) (do (set! shell_sort_collection (assoc shell_sort_collection shell_sort_j (nth shell_sort_collection (- shell_sort_j shell_sort_gap)))) (set! shell_sort_j (- shell_sort_j shell_sort_gap)))) (set! shell_sort_collection (assoc shell_sort_collection shell_sort_j shell_sort_temp)) (set! shell_sort_i (+ shell_sort_i 1)))))) (throw (ex-info "return" {:v shell_sort_collection}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (do (println (str (shell_sort [3 2 1]))) (println (str (shell_sort []))) (println (str (shell_sort [1])))))

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
