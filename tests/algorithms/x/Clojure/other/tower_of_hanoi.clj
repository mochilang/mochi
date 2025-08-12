(ns main (:refer-clojure :exclude [move_tower move_disk]))

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

(declare move_tower move_disk)

(defn move_tower [move_tower_height move_tower_from_pole move_tower_to_pole move_tower_with_pole]
  (do (when (>= move_tower_height 1) (do (move_tower (- move_tower_height 1) move_tower_from_pole move_tower_with_pole move_tower_to_pole) (move_disk move_tower_from_pole move_tower_to_pole) (move_tower (- move_tower_height 1) move_tower_with_pole move_tower_to_pole move_tower_from_pole))) move_tower_height))

(defn move_disk [move_disk_fp move_disk_tp]
  (do (println (str (str (str "moving disk from " move_disk_fp) " to ") move_disk_tp)) move_disk_fp))

(def ^:dynamic main_height 3)

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (move_tower main_height "A" "B" "C")
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
