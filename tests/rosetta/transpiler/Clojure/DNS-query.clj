(ns main)

(require 'clojure.set)

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(def net {:LookupHost (fn [host] [["2001:2f0:0:8800:226:2dff:fe0b:4311" "2001:2f0:0:8800::1:1" "210.155.141.200"] nil])})

(def res ((:LookupHost net) "www.kame.net"))

(def addrs (nth res 0))

(def err (nth res 1))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (if (= err nil) (println (str addrs)) (println err))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
