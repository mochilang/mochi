(ns main (:refer-clojure :exclude [print_max_activities]))

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

(declare print_max_activities)

(def ^:dynamic print_max_activities_i nil)

(def ^:dynamic print_max_activities_j nil)

(def ^:dynamic print_max_activities_n nil)

(def ^:dynamic print_max_activities_result nil)

(defn print_max_activities [print_max_activities_start print_max_activities_finish]
  (binding [print_max_activities_i nil print_max_activities_j nil print_max_activities_n nil print_max_activities_result nil] (do (set! print_max_activities_n (count print_max_activities_finish)) (println "The following activities are selected:") (set! print_max_activities_i 0) (set! print_max_activities_result "0,") (set! print_max_activities_j 1) (while (< print_max_activities_j print_max_activities_n) (do (when (>= (nth print_max_activities_start print_max_activities_j) (nth print_max_activities_finish print_max_activities_i)) (do (set! print_max_activities_result (str (str print_max_activities_result (str print_max_activities_j)) ",")) (set! print_max_activities_i print_max_activities_j))) (set! print_max_activities_j (+ print_max_activities_j 1)))) (println print_max_activities_result))))

(def ^:dynamic main_start [1 3 0 5 8 5])

(def ^:dynamic main_finish [2 4 6 7 9 9])

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (print_max_activities main_start main_finish)
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
