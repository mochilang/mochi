(ns main (:refer-clojure :exclude [power_of_two solution]))

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

(declare power_of_two solution)

(declare _read_file)

(def ^:dynamic power_of_two_i nil)

(def ^:dynamic power_of_two_result nil)

(def ^:dynamic solution_i nil)

(def ^:dynamic solution_num nil)

(def ^:dynamic solution_string_num nil)

(def ^:dynamic solution_sum nil)

(defn power_of_two [power_of_two_exp]
  (binding [power_of_two_i nil power_of_two_result nil] (try (do (set! power_of_two_result 1) (set! power_of_two_i 0) (while (< power_of_two_i power_of_two_exp) (do (set! power_of_two_result (* power_of_two_result 2)) (set! power_of_two_i (+ power_of_two_i 1)))) (throw (ex-info "return" {:v power_of_two_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn solution [solution_power]
  (binding [solution_i nil solution_num nil solution_string_num nil solution_sum nil] (try (do (set! solution_num (power_of_two solution_power)) (set! solution_string_num (mochi_str solution_num)) (set! solution_sum 0) (set! solution_i 0) (while (< solution_i (count solution_string_num)) (do (set! solution_sum (+ solution_sum (long (nth solution_string_num solution_i)))) (set! solution_i (+ solution_i 1)))) (throw (ex-info "return" {:v solution_sum}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (mochi_str (solution 1000)))
      (println (mochi_str (solution 50)))
      (println (mochi_str (solution 20)))
      (println (mochi_str (solution 15)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
