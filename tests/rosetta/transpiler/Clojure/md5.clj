(ns main)

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare sum)

(def testpkg {:Add (fn [a b] (+ a b)) :Pi 3.14 :Answer 42 :FifteenPuzzleExample "Solution found in 52 moves: rrrulddluuuldrurdddrullulurrrddldluurddlulurruldrdrd"})

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (doseq [pair [["d41d8cd98f00b204e9800998ecf8427e" ""] ["0cc175b9c0f1b6a831c399e269772661" "a"] ["900150983cd24fb0d6963f7d28e17f72" "abc"] ["f96b697d7cb7938d525a2f31aaf161d0" "message digest"] ["c3fcd3d76192e4007dfb496cca67e13b" "abcdefghijklmnopqrstuvwxyz"] ["d174ab98d277d9f5a5611c2c9f419d9f" "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"] ["57edf4a22be3c955ac49da2e2107b67a" (str "12345678901234567890" "123456789012345678901234567890123456789012345678901234567890")] ["e38ca1d920c4b8b8d3946b2c72f01680" "The quick brown fox jumped over the lazy dog's back"]]] (do (def sum ((:MD5Hex testpkg) (nth pair 1))) (when (not= sum (nth pair 0)) (do (println "MD5 fail") (println "  for string," (nth pair 1)) (println "  expected:  " (nth pair 0)) (println "  got:       " sum)))))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
