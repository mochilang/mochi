(ns main (:refer-clojure :exclude [peelFirstEat main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare peelFirstEat main)

(def ^:dynamic main_f0 nil)

(def ^:dynamic main_fb nil)

(defn peelFirstEat [peelFirstEat_p]
  (println (str (str "mm, that " (:value peelFirstEat_p)) " was good!")))

(defn main []
  (binding [main_f0 nil main_fb nil] (do (set! main_fb {:items [{:value "banana"} {:value "mango"}]}) (set! main_f0 (get (:items main_fb) 0)) (peelFirstEat main_f0))))

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
