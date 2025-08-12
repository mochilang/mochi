(ns main (:refer-clojure :exclude [binary_step main]))

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

(declare binary_step main)

(def ^:dynamic binary_step_i nil)

(def ^:dynamic binary_step_out nil)

(def ^:dynamic main_result nil)

(def ^:dynamic main_vector nil)

(defn binary_step [binary_step_vector]
  (binding [binary_step_i nil binary_step_out nil] (try (do (set! binary_step_out []) (set! binary_step_i 0) (while (< binary_step_i (count binary_step_vector)) (do (if (>= (nth binary_step_vector binary_step_i) 0.0) (set! binary_step_out (conj binary_step_out 1)) (set! binary_step_out (conj binary_step_out 0))) (set! binary_step_i (+ binary_step_i 1)))) (throw (ex-info "return" {:v binary_step_out}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_result nil main_vector nil] (do (set! main_vector [(- 1.2) 0.0 2.0 1.45 (- 3.7) 0.3]) (set! main_result (binary_step main_vector)) (println main_result))))

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
