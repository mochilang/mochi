(ns main (:refer-clojure :exclude [solution main]))

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

(declare solution main)

(declare _read_file)

(def ^:dynamic main_ans nil)

(def ^:dynamic main_n nil)

(def ^:dynamic solution_a nil)

(def ^:dynamic solution_b nil)

(def ^:dynamic solution_index nil)

(def ^:dynamic solution_temp nil)

(defn solution [solution_n]
  (binding [solution_a nil solution_b nil solution_index nil solution_temp nil] (try (do (set! solution_a 0) (set! solution_b 1) (set! solution_index 1) (while (< (count (mochi_str solution_b)) solution_n) (do (set! solution_temp (+ solution_a solution_b)) (set! solution_a solution_b) (set! solution_b solution_temp) (set! solution_index (+ solution_index 1)))) (throw (ex-info "return" {:v solution_index}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_ans nil main_n nil] (do (set! main_n (toi (read-line))) (set! main_ans (solution main_n)) (println (mochi_str main_ans)))))

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
