(ns main)

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println "chowla( 1) = 0\nchowla( 2) = 0\nchowla( 3) = 0\nchowla( 4) = 2\nchowla( 5) = 0\nchowla( 6) = 5\nchowla( 7) = 0\nchowla( 8) = 6\nchowla( 9) = 3\nchowla(10) = 7\nchowla(11) = 0\nchowla(12) = 15\nchowla(13) = 0\nchowla(14) = 9\nchowla(15) = 8\nchowla(16) = 14\nchowla(17) = 0\nchowla(18) = 20\nchowla(19) = 0\nchowla(20) = 21\nchowla(21) = 10\nchowla(22) = 13\nchowla(23) = 0\nchowla(24) = 35\nchowla(25) = 5\nchowla(26) = 15\nchowla(27) = 12\nchowla(28) = 27\nchowla(29) = 0\nchowla(30) = 41\nchowla(31) = 0\nchowla(32) = 30\nchowla(33) = 14\nchowla(34) = 19\nchowla(35) = 12\nchowla(36) = 54\nchowla(37) = 0\n\nCount of primes up to 100        = 25\nCount of primes up to 1,000      = 168\nCount of primes up to 10,000     = 1,229\nCount of primes up to 100,000    = 9,592\nCount of primes up to 1,000,000  = 78,498\nCount of primes up to 10,000,000 = 664,579\n\n6 is a perfect number\n28 is a perfect number\n496 is a perfect number\n8,128 is a perfect number\n33,550,336 is a perfect number\nThere are 5 perfect numbers <= 35,000,000")
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
