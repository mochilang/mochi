(ns main (:refer-clojure :exclude [padLeft]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare padLeft)

(defn padLeft [s w]
  (try (do (def res "") (def n (- w (count s))) (while (> n 0) (do (def res (str res " ")) (def n (- n 1)))) (throw (ex-info "return" {:v (str res s)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn -main []
  (def dna (str (str (str (str (str (str (str (str (str (str "" "CGTAAAAAATTACAACGTCCTTTGGCTATCTCTTAAACTCCTGCTAAATG") "CTCGTGCTTTCCAATTATGTAAGCGTTCCGAGACGGGGTGGTCGATTCTG") "AGGACAAAGGTCAAGATGGAGCGCATCGAACGCAATAAGGATCATTTGAT") "GGGACGTTTCGTCGACAAAGTCTTGTTTCGAGAGTAACGGCTACCGTCTT") "CGATTCTGCTTATAACACTATGTTCTTATGAAATGGATGTTCTGAGTTGG") "TCAGTCCCAATGTGCGGGGTTTCTTTTAGTACGTCGGGAGTGGTATTATA") "TTTAATTTTTCTATATAGCGATCTGTATTTAAGCAATTCATTTAGGTTAT") "CGCCGCGATGCTCGGTTCGGACCGCCAAGCATCTGGCTCCACTGCTAGTG") "TCCTAAATTTGAATGGCAAACACAAATAAGATTTAGCAATTCGTGTAGAC") "GACCGGGGACTTGCATGATGGGAGCAGCTTTGTTAAACTACGAACGTAAT"))
  (println "SEQUENCE:")
  (def le (count dna))
  (def i 0)
  (while (< i le) (do (def k (+ i 50)) (when (> k le) (def k le)) (println (str (str (padLeft (str i) 5) ": ") (subs dna i k))) (def i (+ i 50))))
  (def a 0)
  (def c 0)
  (def g 0)
  (def t 0)
  (def idx 0)
  (while (< idx le) (do (def ch (subs dna idx (+ idx 1))) (if (= ch "A") (def a (+ a 1)) (if (= ch "C") (def c (+ c 1)) (if (= ch "G") (def g (+ g 1)) (when (= ch "T") (def t (+ t 1)))))) (def idx (+ idx 1))))
  (println "")
  (println "BASE COUNT:")
  (println (str "    A: " (padLeft (str a) 3)))
  (println (str "    C: " (padLeft (str c) 3)))
  (println (str "    G: " (padLeft (str g) 3)))
  (println (str "    T: " (padLeft (str t) 3)))
  (println "    ------")
  (println (str "    Σ: " (str le)))
  (println "    ======"))

(-main)
