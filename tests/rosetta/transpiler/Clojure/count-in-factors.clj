(ns main (:refer-clojure :exclude [show]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare show)

(def ^:dynamic show_f nil)

(def ^:dynamic show_m nil)

(def ^:dynamic show_out nil)

(def ^:dynamic show_x nil)

(defn show [show_n]
  (binding [show_f nil show_m nil show_out nil show_x nil] (try (do (when (= show_n 1) (do (println "1: 1") (throw (ex-info "return" {:v nil})))) (set! show_out (str (str show_n) ": ")) (set! show_x "") (set! show_m show_n) (set! show_f 2) (while (not= show_m 1) (if (= (mod show_m show_f) 0) (do (set! show_out (str (str show_out show_x) (str show_f))) (set! show_x "×") (set! show_m (long (/ show_m show_f)))) (set! show_f (+ show_f 1)))) (println show_out)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (show 1)
      (doseq [i (range 2 10)] (show i))
      (println "...")
      (doseq [i (range 2144 2155)] (show i))
      (println "...")
      (doseq [i (range 9987 10000)] (show i))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
