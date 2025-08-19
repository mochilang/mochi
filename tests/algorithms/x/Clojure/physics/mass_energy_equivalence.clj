(ns main (:refer-clojure :exclude [energy_from_mass mass_from_energy]))

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

(declare energy_from_mass mass_from_energy)

(def ^:dynamic main_C nil)

(defn energy_from_mass [energy_from_mass_mass]
  (try (do (when (< energy_from_mass_mass 0.0) (throw (Exception. "Mass can't be negative."))) (throw (ex-info "return" {:v (* (* energy_from_mass_mass main_C) main_C)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn mass_from_energy [mass_from_energy_energy]
  (try (do (when (< mass_from_energy_energy 0.0) (throw (Exception. "Energy can't be negative."))) (throw (ex-info "return" {:v (/ mass_from_energy_energy (* main_C main_C))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (alter-var-root (var main_C) (constantly 299792458.0))
      (println (mochi_str (energy_from_mass 124.56)))
      (println (mochi_str (energy_from_mass 320.0)))
      (println (mochi_str (energy_from_mass 0.0)))
      (println (mochi_str (mass_from_energy 124.56)))
      (println (mochi_str (mass_from_energy 320.0)))
      (println (mochi_str (mass_from_energy 0.0)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
