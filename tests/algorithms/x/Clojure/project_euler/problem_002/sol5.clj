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

(defn _fetch [url]
  {:data [{:from "" :intensity {:actual 0 :forecast 0 :index ""} :to ""}]})

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare solution)

(def ^:dynamic solution_a nil)

(def ^:dynamic solution_b nil)

(def ^:dynamic solution_next_val nil)

(def ^:dynamic solution_total nil)

(defn solution [solution_n]
  (binding [solution_a nil solution_b nil solution_next_val nil solution_total nil] (try (do (set! solution_a 0) (set! solution_b 1) (set! solution_total 0) (while (<= solution_b solution_n) (do (when (= (mod solution_b 2) 0) (set! solution_total (+ solution_total solution_b))) (set! solution_next_val (+ solution_a solution_b)) (set! solution_a solution_b) (set! solution_b solution_next_val))) (throw (ex-info "return" {:v solution_total}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (solution 10)))
      (println (str (solution 15)))
      (println (str (solution 2)))
      (println (str (solution 1)))
      (println (str (solution 34)))
      (println (str (solution 4000000)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
