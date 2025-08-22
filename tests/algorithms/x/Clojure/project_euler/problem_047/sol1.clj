(ns main (:refer-clojure :exclude [run solution]))

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

(declare run solution)

(declare _read_file)

(def ^:dynamic run_c nil)

(def ^:dynamic run_counts nil)

(def ^:dynamic run_j nil)

(def ^:dynamic run_limit nil)

(def ^:dynamic run_m nil)

(def ^:dynamic run_num nil)

(def ^:dynamic run_p nil)

(def ^:dynamic run_result nil)

(def ^:dynamic run_start nil)

(def ^:dynamic run_streak nil)

(def ^:dynamic solution_res nil)

(defn run [run_n]
  (binding [run_c nil run_counts nil run_j nil run_limit nil run_m nil run_num nil run_p nil run_result nil run_start nil run_streak nil] (try (do (set! run_limit 200000) (set! run_counts {}) (set! run_p 2) (while (<= run_p run_limit) (do (when (not (in run_p run_counts)) (do (set! run_m run_p) (while (<= run_m run_limit) (do (if (in run_m run_counts) (set! run_counts (assoc run_counts run_m (+ (get run_counts run_m) 1))) (set! run_counts (assoc run_counts run_m 1))) (set! run_m (+ run_m run_p)))))) (set! run_p (+ run_p 1)))) (set! run_streak 0) (set! run_num 2) (while (<= run_num run_limit) (do (set! run_c (if (in run_num run_counts) (get run_counts run_num) 0)) (if (= run_c run_n) (do (set! run_streak (+ run_streak 1)) (when (= run_streak run_n) (do (set! run_result []) (set! run_start (+ (- run_num run_n) 1)) (set! run_j 0) (while (< run_j run_n) (do (set! run_result (conj run_result (+ run_start run_j))) (set! run_j (+ run_j 1)))) (throw (ex-info "return" {:v run_result}))))) (set! run_streak 0)) (set! run_num (+ run_num 1)))) (throw (ex-info "return" {:v []}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn solution [solution_n]
  (binding [solution_res nil] (try (do (set! solution_res (run solution_n)) (throw (ex-info "return" {:v (nth solution_res 0)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (mochi_str (solution 4)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
