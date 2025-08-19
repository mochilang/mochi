(ns main (:refer-clojure :exclude [fabs reynolds_number]))

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

(declare fabs reynolds_number)

(defn fabs [fabs_x]
  (try (if (< fabs_x 0.0) (throw (ex-info "return" {:v (- fabs_x)})) (throw (ex-info "return" {:v fabs_x}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn reynolds_number [reynolds_number_density reynolds_number_velocity reynolds_number_diameter reynolds_number_viscosity]
  (try (do (when (or (or (<= reynolds_number_density 0.0) (<= reynolds_number_diameter 0.0)) (<= reynolds_number_viscosity 0.0)) (throw (Exception. "please ensure that density, diameter and viscosity are positive"))) (throw (ex-info "return" {:v (/ (* (* reynolds_number_density (fabs reynolds_number_velocity)) reynolds_number_diameter) reynolds_number_viscosity)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (reynolds_number 900.0 2.5 0.05 0.4))
      (println (reynolds_number 450.0 3.86 0.078 0.23))
      (println (reynolds_number 234.0 (- 4.5) 0.3 0.44))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
