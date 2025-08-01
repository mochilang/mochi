(ns main)

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(defn Foo_ValueMethod [Foo_ValueMethod_self Foo_ValueMethod_x]
  (do))

(defn Foo_PointerMethod [Foo_PointerMethod_self Foo_PointerMethod_x]
  (do))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare main_myPointer main_myValue)

(def main_myValue {})

(def main_myPointer {})

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (Foo_ValueMethod main_myValue 0)
      (Foo_PointerMethod main_myPointer 0)
      (Foo_ValueMethod main_myPointer 0)
      (Foo_PointerMethod main_myValue 0)
      (Foo_ValueMethod main_myValue 0)
      (Foo_PointerMethod main_myPointer 0)
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
