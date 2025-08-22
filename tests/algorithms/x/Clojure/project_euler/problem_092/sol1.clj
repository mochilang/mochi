(ns main (:refer-clojure :exclude [next_number chain solution]))

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

(declare next_number chain solution)

(declare _read_file)

(def ^:dynamic chain_n nil)

(def ^:dynamic count_v nil)

(def ^:dynamic next_number_d nil)

(def ^:dynamic next_number_n nil)

(def ^:dynamic next_number_total nil)

(def ^:dynamic solution_i nil)

(defn next_number [next_number_number]
  (binding [next_number_d nil next_number_n nil next_number_total nil] (try (do (set! next_number_n next_number_number) (set! next_number_total 0) (while (> next_number_n 0) (do (set! next_number_d (mod next_number_n 10)) (set! next_number_total (+ next_number_total (* next_number_d next_number_d))) (set! next_number_n (/ next_number_n 10)))) (throw (ex-info "return" {:v next_number_total}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn chain [chain_number]
  (binding [chain_n nil] (try (do (set! chain_n chain_number) (while (and (not= chain_n 1) (not= chain_n 89)) (set! chain_n (next_number chain_n))) (throw (ex-info "return" {:v (= chain_n 1)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn solution [solution_limit]
  (binding [count_v nil solution_i nil] (try (do (set! count_v 0) (set! solution_i 1) (while (< solution_i solution_limit) (do (when (not (chain solution_i)) (set! count_v (+ count_v 1))) (set! solution_i (+ solution_i 1)))) (throw (ex-info "return" {:v count_v}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (mochi_str (next_number 44)))
      (println (mochi_str (next_number 10)))
      (println (mochi_str (next_number 32)))
      (println (mochi_str (chain 10)))
      (println (mochi_str (chain 58)))
      (println (mochi_str (chain 1)))
      (println (mochi_str (solution 100)))
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
