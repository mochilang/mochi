(ns main (:refer-clojure :exclude [find_previous_power_of_two main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(defn indexOf [s sub]
  (let [idx (clojure.string/index-of s sub)] (if (nil? idx) -1 idx)))

(defn split [s sep]
  (clojure.string/split s (re-pattern sep)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare find_previous_power_of_two main)

(def ^:dynamic find_previous_power_of_two_power nil)

(def ^:dynamic main_i nil)

(def ^:dynamic main_results nil)

(defn find_previous_power_of_two [find_previous_power_of_two_number]
  (binding [find_previous_power_of_two_power nil] (try (do (when (< find_previous_power_of_two_number 0) (throw (Exception. "Input must be a non-negative integer"))) (when (= find_previous_power_of_two_number 0) (throw (ex-info "return" {:v 0}))) (set! find_previous_power_of_two_power 1) (while (<= find_previous_power_of_two_power find_previous_power_of_two_number) (set! find_previous_power_of_two_power (* find_previous_power_of_two_power 2))) (if (> find_previous_power_of_two_number 1) (throw (ex-info "return" {:v (quot find_previous_power_of_two_power 2)})) (throw (ex-info "return" {:v 1})))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_i nil main_results nil] (do (set! main_results []) (set! main_i 0) (while (< main_i 18) (do (set! main_results (conj main_results (find_previous_power_of_two main_i))) (set! main_i (+ main_i 1)))) (println (str main_results)))))

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
