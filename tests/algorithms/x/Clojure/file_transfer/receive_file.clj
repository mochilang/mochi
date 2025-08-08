(ns main (:refer-clojure :exclude [receive_file main]))

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

(declare receive_file main)

(def ^:dynamic main_incoming nil)

(def ^:dynamic main_received nil)

(def ^:dynamic receive_file_data nil)

(def ^:dynamic receive_file_i nil)

(def ^:dynamic receive_file_out nil)

(defn receive_file [receive_file_chunks]
  (binding [receive_file_data nil receive_file_i nil receive_file_out nil] (try (do (set! receive_file_out "") (set! receive_file_i 0) (println "File opened") (println "Receiving data...") (loop [while_flag_1 true] (when (and while_flag_1 (< receive_file_i (count receive_file_chunks))) (do (set! receive_file_data (nth receive_file_chunks receive_file_i)) (cond (= receive_file_data "") (recur false) :else (do (set! receive_file_out (str receive_file_out receive_file_data)) (set! receive_file_i (+ receive_file_i 1)) (recur while_flag_1)))))) (println "Successfully received the file") (println "Connection closed") (throw (ex-info "return" {:v receive_file_out}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_incoming nil main_received nil] (do (set! main_incoming ["Hello " "from " "server"]) (set! main_received (receive_file main_incoming)) (println main_received))))

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
