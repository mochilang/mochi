(ns main (:refer-clojure :exclude [get_gas_stations can_complete_journey]))

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

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare get_gas_stations can_complete_journey)

(def ^:dynamic can_complete_journey_i nil)

(def ^:dynamic can_complete_journey_net nil)

(def ^:dynamic can_complete_journey_start nil)

(def ^:dynamic can_complete_journey_station nil)

(def ^:dynamic can_complete_journey_total_cost nil)

(def ^:dynamic can_complete_journey_total_gas nil)

(def ^:dynamic get_gas_stations_i nil)

(def ^:dynamic get_gas_stations_stations nil)

(defn get_gas_stations [get_gas_stations_gas_quantities get_gas_stations_costs]
  (binding [get_gas_stations_i nil get_gas_stations_stations nil] (try (do (set! get_gas_stations_stations []) (set! get_gas_stations_i 0) (while (< get_gas_stations_i (count get_gas_stations_gas_quantities)) (do (set! get_gas_stations_stations (conj get_gas_stations_stations {:cost (nth get_gas_stations_costs get_gas_stations_i) :gas_quantity (nth get_gas_stations_gas_quantities get_gas_stations_i)})) (set! get_gas_stations_i (+ get_gas_stations_i 1)))) (throw (ex-info "return" {:v get_gas_stations_stations}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn can_complete_journey [can_complete_journey_gas_stations]
  (binding [can_complete_journey_i nil can_complete_journey_net nil can_complete_journey_start nil can_complete_journey_station nil can_complete_journey_total_cost nil can_complete_journey_total_gas nil] (try (do (set! can_complete_journey_total_gas 0) (set! can_complete_journey_total_cost 0) (set! can_complete_journey_i 0) (while (< can_complete_journey_i (count can_complete_journey_gas_stations)) (do (set! can_complete_journey_total_gas (+ can_complete_journey_total_gas (get (nth can_complete_journey_gas_stations can_complete_journey_i) "gas_quantity"))) (set! can_complete_journey_total_cost (+ can_complete_journey_total_cost (get (nth can_complete_journey_gas_stations can_complete_journey_i) "cost"))) (set! can_complete_journey_i (+ can_complete_journey_i 1)))) (when (< can_complete_journey_total_gas can_complete_journey_total_cost) (throw (ex-info "return" {:v (- 1)}))) (set! can_complete_journey_start 0) (set! can_complete_journey_net 0) (set! can_complete_journey_i 0) (while (< can_complete_journey_i (count can_complete_journey_gas_stations)) (do (set! can_complete_journey_station (nth can_complete_journey_gas_stations can_complete_journey_i)) (set! can_complete_journey_net (- (+ can_complete_journey_net (get can_complete_journey_station "gas_quantity")) (get can_complete_journey_station "cost"))) (when (< can_complete_journey_net 0) (do (set! can_complete_journey_start (+ can_complete_journey_i 1)) (set! can_complete_journey_net 0))) (set! can_complete_journey_i (+ can_complete_journey_i 1)))) (throw (ex-info "return" {:v can_complete_journey_start}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_example1 (get_gas_stations [1 2 3 4 5] [3 4 5 1 2]))

(def ^:dynamic main_example2 (get_gas_stations [2 3 4] [3 4 3]))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (can_complete_journey main_example1)))
      (println (str (can_complete_journey main_example2)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
