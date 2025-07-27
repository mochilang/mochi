(ns main (:refer-clojure :exclude [padLeft indexOfFrom containsStr distinct permutations headTailOverlap deduplicate joinAll shortestCommonSuperstring printCounts main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare padLeft indexOfFrom containsStr distinct permutations headTailOverlap deduplicate joinAll shortestCommonSuperstring printCounts main)

(defn padLeft [s w]
  (try (do (def res "") (def n (- w (count s))) (while (> n 0) (do (def res (str res " ")) (def n (- n 1)))) (throw (ex-info "return" {:v (str res s)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn indexOfFrom [s ch start]
  (try (do (def i start) (while (< i (count s)) (do (when (= (subs s i (+ i 1)) ch) (throw (ex-info "return" {:v i}))) (def i (+ i 1)))) (throw (ex-info "return" {:v (- 1)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn containsStr [s sub]
  (try (do (def i 0) (def sl (count s)) (def subl (count sub)) (while (<= i (- sl subl)) (do (when (= (subs s i (+ i subl)) sub) (throw (ex-info "return" {:v true}))) (def i (+ i 1)))) (throw (ex-info "return" {:v false}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn distinct [slist]
  (try (do (def res []) (doseq [s slist] (do (def found false) (loop [r_seq res] (when (seq r_seq) (let [r (first r_seq)] (cond (= r s) (do (def found true) (recur nil)) :else (recur (rest r_seq)))))) (when (not found) (def res (conj res s))))) (throw (ex-info "return" {:v res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn permutations [xs]
  (try (do (when (<= (count xs) 1) (throw (ex-info "return" {:v [xs]}))) (def res []) (def i 0) (while (< i (count xs)) (do (def rest_v []) (def j 0) (while (< j (count xs)) (do (when (not= j i) (def rest_v (conj rest_v (nth xs j)))) (def j (+ j 1)))) (def subs (permutations rest_v)) (doseq [p subs] (do (def perm [(nth xs i)]) (def k 0) (while (< k (count p)) (do (def perm (conj perm (nth p k))) (def k (+ k 1)))) (def res (conj res perm)))) (def i (+ i 1)))) (throw (ex-info "return" {:v res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn headTailOverlap [s1 s2]
  (try (do (def start 0) (while true (do (def ix (indexOfFrom s1 (subs s2 0 1) start)) (when (= ix (- 0 1)) (throw (ex-info "return" {:v 0}))) (def start ix) (def sublen (- (count s1) start)) (when (> sublen (count s2)) (def sublen (count s2))) (when (= (subs s2 0 sublen) (subs s1 start (+ start sublen))) (throw (ex-info "return" {:v sublen}))) (def start (+ start 1))))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn deduplicate [slist]
  (try (do (def arr (distinct slist)) (def filtered []) (def i 0) (while (< i (count arr)) (do (def s1 (nth arr i)) (def within false) (def j 0) (loop [while_flag_1 true] (when (and while_flag_1 (< j (count arr))) (cond (and (not= j i) (containsStr (nth arr j) s1)) (do (def within true) (recur false)) :else (do (def j (+ j 1)) (recur while_flag_1))))) (when (not within) (def filtered (conj filtered s1))) (def i (+ i 1)))) (throw (ex-info "return" {:v filtered}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn joinAll [ss]
  (try (do (def out "") (doseq [s ss] (def out (str out s))) (throw (ex-info "return" {:v out}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn shortestCommonSuperstring [slist]
  (try (do (def ss (deduplicate slist)) (def shortest (joinAll ss)) (def perms (permutations ss)) (def idx 0) (while (< idx (count perms)) (do (def perm (nth perms idx)) (def sup (nth perm 0)) (def i 0) (while (< i (- (count ss) 1)) (do (def ov (headTailOverlap (nth perm i) (nth perm (+ i 1)))) (def sup (str sup (subs (nth perm (+ i 1)) ov (count (nth perm (+ i 1)))))) (def i (+ i 1)))) (when (< (count sup) (count shortest)) (def shortest sup)) (def idx (+ idx 1)))) (throw (ex-info "return" {:v shortest}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn printCounts [seq]
  (do (def a 0) (def c 0) (def g 0) (def t 0) (def i 0) (while (< i (count seq)) (do (def ch (subs seq i (+ i 1))) (if (= ch "A") (def a (+ a 1)) (if (= ch "C") (def c (+ c 1)) (if (= ch "G") (def g (+ g 1)) (when (= ch "T") (def t (+ t 1)))))) (def i (+ i 1)))) (def total (count seq)) (println (str (str "\nNucleotide counts for " seq) ":\n")) (println (+ (padLeft "A" 10) (padLeft (str a) 12))) (println (+ (padLeft "C" 10) (padLeft (str c) 12))) (println (+ (padLeft "G" 10) (padLeft (str g) 12))) (println (+ (padLeft "T" 10) (padLeft (str t) 12))) (println (+ (padLeft "Other" 10) (padLeft (str (- total (+ (+ (+ a c) g) t))) 12))) (println "  ____________________") (println (+ (padLeft "Total length" 14) (padLeft (str total) 8)))))

(defn main []
  (do (def tests [["TA" "AAG" "TA" "GAA" "TA"] ["CATTAGGG" "ATTAG" "GGG" "TA"] ["AAGAUGGA" "GGAGCGCAUC" "AUCGCAAUAAGGA"] ["ATGAAATGGATGTTCTGAGTTGGTCAGTCCCAATGTGCGGGGTTTCTTTTAGTACGTCGGGAGTGGTATTAT" "GGTCGATTCTGAGGACAAAGGTCAAGATGGAGCGCATCGAACGCAATAAGGATCATTTGATGGGACGTTTCGTCGACAAAGT" "CTATGTTCTTATGAAATGGATGTTCTGAGTTGGTCAGTCCCAATGTGCGGGGTTTCTTTTAGTACGTCGGGAGTGGTATTATA" "TGCTTTCCAATTATGTAAGCGTTCCGAGACGGGGTGGTCGATTCTGAGGACAAAGGTCAAGATGGAGCGCATC" "AACGCAATAAGGATCATTTGATGGGACGTTTCGTCGACAAAGTCTTGTTTCGAGAGTAACGGCTACCGTCTT" "GCGCATCGAACGCAATAAGGATCATTTGATGGGACGTTTCGTCGACAAAGTCTTGTTTCGAGAGTAACGGCTACCGTC" "CGTTTCGTCGACAAAGTCTTGTTTCGAGAGTAACGGCTACCGTCTTCGATTCTGCTTATAACACTATGTTCT" "TGCTTTCCAATTATGTAAGCGTTCCGAGACGGGGTGGTCGATTCTGAGGACAAAGGTCAAGATGGAGCGCATC" "CGTAAAAAATTACAACGTCCTTTGGCTATCTCTTAAACTCCTGCTAAATGCTCGTGC" "GATGGAGCGCATCGAACGCAATAAGGATCATTTGATGGGACGTTTCGTCGACAAAGTCTTGTTTCGAGAGTAACGGCTACCGTCTTCGATT" "TTTCCAATTATGTAAGCGTTCCGAGACGGGGTGGTCGATTCTGAGGACAAAGGTCAAGATGGAGCGCATC" "CTATGTTCTTATGAAATGGATGTTCTGAGTTGGTCAGTCCCAATGTGCGGGGTTTCTTTTAGTACGTCGGGAGTGGTATTATA" "TCTCTTAAACTCCTGCTAAATGCTCGTGCTTTCCAATTATGTAAGCGTTCCGAGACGGGGTGGTCGATTCTGAGGACAAAGGTCAAGA"]]) (doseq [seqs tests] (do (def scs (shortestCommonSuperstring seqs)) (printCounts scs)))))

(defn -main []
  (main))

(-main)
