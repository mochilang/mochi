(ns main (:refer-clojure :exclude [solution]))

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

(defn mochi_str [v]
  (cond (float? v) (let [s (str v)] (if (clojure.string/ends-with? s ".0") (subs s 0 (- (count s) 2)) s)) :else (str v)))

(defn _fetch [url]
  {:data [{:from "" :intensity {:actual 0 :forecast 0 :index ""} :to ""}]})

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare solution)

(def ^:dynamic solution_num nil)

(def ^:dynamic solution_total nil)

(defn solution [solution_n]
  (binding [solution_num nil solution_total nil] (try (do (set! solution_total 0) (set! solution_num 0) (loop [while_flag_1 true] (when (and while_flag_1 true) (do (set! solution_num (+ solution_num 3)) (cond (>= solution_num solution_n) (recur false) (>= solution_num solution_n) (recur false) (>= solution_num solution_n) (recur false) (>= solution_num solution_n) (recur false) (>= solution_num solution_n) (recur false) (>= solution_num solution_n) (recur false) (>= solution_num solution_n) (recur false) :else (do (set! solution_total (+ solution_total solution_num)) (set! solution_num (+ solution_num 2)) (set! solution_total (+ solution_total solution_num)) (set! solution_num (+ solution_num 1)) (set! solution_total (+ solution_total solution_num)) (set! solution_num (+ solution_num 3)) (set! solution_total (+ solution_total solution_num)) (set! solution_num (+ solution_num 1)) (set! solution_total (+ solution_total solution_num)) (set! solution_num (+ solution_num 2)) (set! solution_total (+ solution_total solution_num)) (set! solution_num (+ solution_num 3)) (set! solution_total (+ solution_total solution_num)) (recur while_flag_1)))))) (throw (ex-info "return" {:v solution_total}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (mochi_str (solution 1000)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
