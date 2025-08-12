(ns main (:refer-clojure :exclude [kinetic_energy]))

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

(declare kinetic_energy)

(def ^:dynamic kinetic_energy_v nil)

(defn kinetic_energy [kinetic_energy_mass kinetic_energy_velocity]
  (binding [kinetic_energy_v nil] (try (do (when (< kinetic_energy_mass 0.0) (throw (Exception. "The mass of a body cannot be negative"))) (set! kinetic_energy_v kinetic_energy_velocity) (when (< kinetic_energy_v 0.0) (set! kinetic_energy_v (- kinetic_energy_v))) (throw (ex-info "return" {:v (* (* (* 0.5 kinetic_energy_mass) kinetic_energy_v) kinetic_energy_v)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (kinetic_energy 10.0 10.0))
      (println (kinetic_energy 0.0 10.0))
      (println (kinetic_energy 10.0 0.0))
      (println (kinetic_energy 20.0 (- 20.0)))
      (println (kinetic_energy 0.0 0.0))
      (println (kinetic_energy 2.0 2.0))
      (println (kinetic_energy 100.0 100.0))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
