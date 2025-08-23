(ns main (:refer-clojure :exclude [num_digits solution]))

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

(declare num_digits solution)

(declare _read_file)

(def ^:dynamic count_v nil)

(def ^:dynamic num_digits_n nil)

(def ^:dynamic solution_f nil)

(def ^:dynamic solution_f1 nil)

(def ^:dynamic solution_f2 nil)

(def ^:dynamic solution_index nil)

(defn num_digits [num_digits_x]
  (binding [count_v nil num_digits_n nil] (try (do (set! count_v 0) (set! num_digits_n num_digits_x) (while (> num_digits_n 0) (do (set! count_v (+ count_v 1)) (set! num_digits_n (quot num_digits_n 10)))) (throw (ex-info "return" {:v count_v}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn solution [solution_n]
  (binding [solution_f nil solution_f1 nil solution_f2 nil solution_index nil] (try (do (set! solution_f1 1) (set! solution_f2 1) (set! solution_index 2) (loop [while_flag_1 true] (when (and while_flag_1 true) (do (set! solution_f (+ solution_f1 solution_f2)) (set! solution_f1 solution_f2) (set! solution_f2 solution_f) (set! solution_index (+ solution_index 1)) (cond (= (num_digits solution_f) solution_n) (recur false) :else (recur while_flag_1))))) (throw (ex-info "return" {:v solution_index}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str "solution(1000) = " (mochi_str (solution 1000))))
      (println (str "solution(100) = " (mochi_str (solution 100))))
      (println (str "solution(50) = " (mochi_str (solution 50))))
      (println (str "solution(3) = " (mochi_str (solution 3))))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
