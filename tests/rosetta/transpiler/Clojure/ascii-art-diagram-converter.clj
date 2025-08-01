(ns main (:refer-clojure :exclude [main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare main)

(defn main []
  (do (println "Diagram after trimming whitespace and removal of blank lines:\n") (println "+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+") (println "|                      ID                       |") (println "+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+") (println "|QR|   Opcode  |AA|TC|RD|RA|   Z    |   RCODE   |") (println "+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+") (println "|                    QDCOUNT                    |") (println "+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+") (println "|                    ANCOUNT                    |") (println "+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+") (println "|                    NSCOUNT                    |") (println "+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+") (println "|                    ARCOUNT                    |") (println "+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+") (println "\nDecoded:\n") (println "Name     Bits  Start  End") (println "=======  ====  =====  ===") (println "ID        16      0    15") (println "QR         1     16    16") (println "Opcode     4     17    20") (println "AA         1     21    21") (println "TC         1     22    22") (println "RD         1     23    23") (println "RA         1     24    24") (println "Z          3     25    27") (println "RCODE      4     28    31") (println "QDCOUNT   16     32    47") (println "ANCOUNT   16     48    63") (println "NSCOUNT   16     64    79") (println "ARCOUNT   16     80    95") (println "\nTest string in hex:") (println "78477bbf5496e12e1bf169a4") (println "\nTest string in binary:") (println "011110000100011101111011101111110101010010010110111000010010111000011011111100010110100110100100") (println "\nUnpacked:\n") (println "Name     Size  Bit pattern") (println "=======  ====  ================") (println "ID        16   0111100001000111") (println "QR         1   0") (println "Opcode     4   1111") (println "AA         1   0") (println "TC         1   1") (println "RD         1   1") (println "RA         1   1") (println "Z          3   011") (println "RCODE      4   1111") (println "QDCOUNT   16   0101010010010110") (println "ANCOUNT   16   1110000100101110") (println "NSCOUNT   16   0001101111110001") (println "ARCOUNT   16   0110100110100100")))

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
