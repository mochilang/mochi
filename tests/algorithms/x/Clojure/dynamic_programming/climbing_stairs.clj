(ns main (:refer-clojure :exclude [climb_stairs]))

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

(declare climb_stairs)

(def ^:dynamic climb_stairs_current nil)

(def ^:dynamic climb_stairs_i nil)

(def ^:dynamic climb_stairs_previous nil)

(def ^:dynamic next_v nil)

(defn climb_stairs [climb_stairs_number_of_steps]
  (binding [climb_stairs_current nil climb_stairs_i nil climb_stairs_previous nil next_v nil] (try (do (when (<= climb_stairs_number_of_steps 0) (throw (Exception. "number_of_steps needs to be positive"))) (when (= climb_stairs_number_of_steps 1) (throw (ex-info "return" {:v 1}))) (set! climb_stairs_previous 1) (set! climb_stairs_current 1) (set! climb_stairs_i 0) (while (< climb_stairs_i (- climb_stairs_number_of_steps 1)) (do (set! next_v (+ climb_stairs_current climb_stairs_previous)) (set! climb_stairs_previous climb_stairs_current) (set! climb_stairs_current next_v) (set! climb_stairs_i (+ climb_stairs_i 1)))) (throw (ex-info "return" {:v climb_stairs_current}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (climb_stairs 3))
      (println (climb_stairs 1))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
