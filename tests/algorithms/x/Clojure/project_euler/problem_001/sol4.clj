(ns main (:refer-clojure :exclude [contains solution test_solution main]))

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

(declare contains solution test_solution main)

(def ^:dynamic contains_i nil)

(def ^:dynamic solution_collection nil)

(def ^:dynamic solution_i nil)

(def ^:dynamic solution_result nil)

(def ^:dynamic solution_temp nil)

(def ^:dynamic solution_total nil)

(def ^:dynamic solution_v nil)

(def ^:dynamic solution_xmulti nil)

(def ^:dynamic solution_zmulti nil)

(defn contains [contains_xs contains_value]
  (binding [contains_i nil] (try (do (set! contains_i 0) (while (< contains_i (count contains_xs)) (do (when (= (nth contains_xs contains_i) contains_value) (throw (ex-info "return" {:v true}))) (set! contains_i (+ contains_i 1)))) (throw (ex-info "return" {:v false}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn solution [solution_n]
  (binding [solution_collection nil solution_i nil solution_result nil solution_temp nil solution_total nil solution_v nil solution_xmulti nil solution_zmulti nil] (try (do (set! solution_zmulti []) (set! solution_xmulti []) (set! solution_temp 1) (loop [while_flag_1 true] (when (and while_flag_1 true) (do (set! solution_result (* 3 solution_temp)) (if (< solution_result solution_n) (do (set! solution_zmulti (conj solution_zmulti solution_result)) (set! solution_temp (+ solution_temp 1)) (recur while_flag_1)) (recur false))))) (set! solution_temp 1) (loop [while_flag_2 true] (when (and while_flag_2 true) (do (set! solution_result (* 5 solution_temp)) (if (< solution_result solution_n) (do (set! solution_xmulti (conj solution_xmulti solution_result)) (set! solution_temp (+ solution_temp 1)) (recur while_flag_2)) (recur false))))) (set! solution_collection []) (set! solution_i 0) (while (< solution_i (count solution_zmulti)) (do (set! solution_v (nth solution_zmulti solution_i)) (when (not (contains solution_collection solution_v)) (set! solution_collection (conj solution_collection solution_v))) (set! solution_i (+ solution_i 1)))) (set! solution_i 0) (while (< solution_i (count solution_xmulti)) (do (set! solution_v (nth solution_xmulti solution_i)) (when (not (contains solution_collection solution_v)) (set! solution_collection (conj solution_collection solution_v))) (set! solution_i (+ solution_i 1)))) (set! solution_total 0) (set! solution_i 0) (while (< solution_i (count solution_collection)) (do (set! solution_total (+ solution_total (nth solution_collection solution_i))) (set! solution_i (+ solution_i 1)))) (throw (ex-info "return" {:v solution_total}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn test_solution []
  (do (when (not= (solution 3) 0) (throw (Exception. "solution(3) failed"))) (when (not= (solution 4) 3) (throw (Exception. "solution(4) failed"))) (when (not= (solution 10) 23) (throw (Exception. "solution(10) failed"))) (when (not= (solution 600) 83700) (throw (Exception. "solution(600) failed")))))

(defn main []
  (do (test_solution) (println (str "solution() = " (str (solution 1000))))))

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
