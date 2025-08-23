(ns main (:refer-clojure :exclude [solution test_solution main]))

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
  (int (Double/valueOf (str s))))

(defn _ord [s]
  (int (first s)))

(defn mochi_str [v]
  (cond (float? v) (let [s (str v)] (if (clojure.string/ends-with? s ".0") (subs s 0 (- (count s) 2)) s)) :else (str v)))

(defn _fetch [url]
  {:data [{:from "" :intensity {:actual 0 :forecast 0 :index ""} :to ""}]})

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare solution test_solution main)

(declare _read_file)

(def ^:dynamic count_v nil)

(def ^:dynamic solution_i nil)

(def ^:dynamic solution_idx nil)

(def ^:dynamic solution_j nil)

(def ^:dynamic solution_product nil)

(def ^:dynamic solution_s nil)

(def ^:dynamic solution_targets nil)

(defn solution []
  (binding [count_v nil solution_i nil solution_idx nil solution_j nil solution_product nil solution_s nil solution_targets nil] (try (do (set! solution_targets [1 10 100 1000 10000 100000 1000000]) (set! solution_idx 0) (set! solution_product 1) (set! count_v 0) (set! solution_i 1) (loop [while_flag_1 true] (when (and while_flag_1 (< solution_idx (count solution_targets))) (do (set! solution_s (mochi_str solution_i)) (set! solution_j 0) (loop [while_flag_2 true] (when (and while_flag_2 (< solution_j (count solution_s))) (do (set! count_v (+ count_v 1)) (if (= count_v (nth solution_targets solution_idx)) (do (set! solution_product (* solution_product (long (nth solution_s solution_j)))) (set! solution_idx (+ solution_idx 1)) (when (= solution_idx (count solution_targets)) (recur false))) (set! solution_j (+ solution_j 1))) (cond :else (recur while_flag_2))))) (set! solution_i (+ solution_i 1)) (cond :else (recur while_flag_1))))) (throw (ex-info "return" {:v solution_product}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn test_solution []
  (when (not= (solution) 210) (throw (Exception. "solution failed"))))

(defn main []
  (do (test_solution) (println (mochi_str (solution)))))

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
