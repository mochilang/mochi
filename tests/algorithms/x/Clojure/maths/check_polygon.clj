(ns main (:refer-clojure :exclude [check_polygon]))

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

(declare check_polygon)

(def ^:dynamic check_polygon_i nil)

(def ^:dynamic check_polygon_max_side nil)

(def ^:dynamic check_polygon_total nil)

(def ^:dynamic check_polygon_v nil)

(defn check_polygon [check_polygon_nums]
  (binding [check_polygon_i nil check_polygon_max_side nil check_polygon_total nil check_polygon_v nil] (try (do (when (< (count check_polygon_nums) 2) (throw (Exception. "Monogons and Digons are not polygons in the Euclidean space"))) (set! check_polygon_i 0) (while (< check_polygon_i (count check_polygon_nums)) (do (when (<= (nth check_polygon_nums check_polygon_i) 0.0) (throw (Exception. "All values must be greater than 0"))) (set! check_polygon_i (+ check_polygon_i 1)))) (set! check_polygon_total 0.0) (set! check_polygon_max_side 0.0) (set! check_polygon_i 0) (while (< check_polygon_i (count check_polygon_nums)) (do (set! check_polygon_v (nth check_polygon_nums check_polygon_i)) (set! check_polygon_total (+ check_polygon_total check_polygon_v)) (when (> check_polygon_v check_polygon_max_side) (set! check_polygon_max_side check_polygon_v)) (set! check_polygon_i (+ check_polygon_i 1)))) (throw (ex-info "return" {:v (< check_polygon_max_side (- check_polygon_total check_polygon_max_side))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_nums [3.0 7.0 13.0 2.0])

(def ^:dynamic main__ (check_polygon main_nums))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (check_polygon [6.0 10.0 5.0])))
      (println (str (check_polygon [3.0 7.0 13.0 2.0])))
      (println (str (check_polygon [1.0 4.3 5.2 12.2])))
      (println (str main_nums))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
