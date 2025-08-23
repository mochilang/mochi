(ns main (:refer-clojure :exclude [countDivisors solution]))

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

(declare countDivisors solution)

(declare _read_file)

(def ^:dynamic countDivisors_i nil)

(def ^:dynamic countDivisors_multiplicity nil)

(def ^:dynamic countDivisors_num nil)

(def ^:dynamic countDivisors_total nil)

(def ^:dynamic solution_n nil)

(def ^:dynamic solution_tri nil)

(defn countDivisors [countDivisors_n]
  (binding [countDivisors_i nil countDivisors_multiplicity nil countDivisors_num nil countDivisors_total nil] (try (do (set! countDivisors_num countDivisors_n) (set! countDivisors_total 1) (set! countDivisors_i 2) (while (<= (* countDivisors_i countDivisors_i) countDivisors_num) (do (set! countDivisors_multiplicity 0) (while (= (mod countDivisors_num countDivisors_i) 0) (do (set! countDivisors_num (quot countDivisors_num countDivisors_i)) (set! countDivisors_multiplicity (+ countDivisors_multiplicity 1)))) (set! countDivisors_total (* countDivisors_total (+ countDivisors_multiplicity 1))) (set! countDivisors_i (+ countDivisors_i 1)))) (when (> countDivisors_num 1) (set! countDivisors_total (* countDivisors_total 2))) (throw (ex-info "return" {:v countDivisors_total}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn solution []
  (binding [solution_n nil solution_tri nil] (try (do (set! solution_n 1) (set! solution_tri 1) (while (<= (countDivisors solution_tri) 500) (do (set! solution_n (+ solution_n 1)) (set! solution_tri (+ solution_tri solution_n)))) (throw (ex-info "return" {:v solution_tri}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (solution))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
