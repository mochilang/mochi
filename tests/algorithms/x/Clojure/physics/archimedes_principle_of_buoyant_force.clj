(ns main (:refer-clojure :exclude [archimedes_principle archimedes_principle_default]))

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

(declare archimedes_principle archimedes_principle_default)

(def ^:dynamic archimedes_principle_default_res nil)

(def ^:dynamic main_G 9.80665)

(defn archimedes_principle [archimedes_principle_fluid_density archimedes_principle_volume archimedes_principle_gravity]
  (try (do (when (<= archimedes_principle_fluid_density 0.0) (throw (Exception. "Impossible fluid density"))) (when (<= archimedes_principle_volume 0.0) (throw (Exception. "Impossible object volume"))) (when (< archimedes_principle_gravity 0.0) (throw (Exception. "Impossible gravity"))) (throw (ex-info "return" {:v (* (* archimedes_principle_fluid_density archimedes_principle_volume) archimedes_principle_gravity)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn archimedes_principle_default [archimedes_principle_default_fluid_density archimedes_principle_default_volume]
  (binding [archimedes_principle_default_res nil] (try (do (set! archimedes_principle_default_res (archimedes_principle archimedes_principle_default_fluid_density archimedes_principle_default_volume main_G)) (throw (ex-info "return" {:v archimedes_principle_default_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]

      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
