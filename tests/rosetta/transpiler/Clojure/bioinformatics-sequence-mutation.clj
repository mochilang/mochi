(ns main (:refer-clojure :exclude [randInt padLeft makeSeq mutate prettyPrint wstring main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare randInt padLeft makeSeq mutate prettyPrint wstring main)

(defn randInt [s n]
  (try (do (def next (mod (+ (* s 1664525) 1013904223) 2147483647)) (throw (ex-info "return" {:v [next (mod next n)]}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn padLeft [s w]
  (try (do (def res "") (def n (- w (count s))) (while (> n 0) (do (def res (str res " ")) (def n (- n 1)))) (throw (ex-info "return" {:v (str res s)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn makeSeq [s le]
  (try (do (def bases "ACGT") (def out "") (def i 0) (while (< i le) (do (def r (randInt s 4)) (def s (nth r 0)) (def idx (int (nth r 1))) (def out (str out (subs bases idx (+ idx 1)))) (def i (+ i 1)))) (throw (ex-info "return" {:v [s out]}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn mutate [s dna w]
  (try (do (def bases "ACGT") (def le (count dna)) (def r (randInt s le)) (def s (nth r 0)) (def p (int (nth r 1))) (def r (randInt s 300)) (def s (nth r 0)) (def x (int (nth r 1))) (def arr []) (def i 0) (while (< i le) (do (def arr (conj arr (subs dna i (+ i 1)))) (def i (+ i 1)))) (if (< x (nth w 0)) (do (def r (randInt s 4)) (def s (nth r 0)) (def idx (int (nth r 1))) (def b (subs bases idx (+ idx 1))) (println (str (str (str (str (str (str "  Change @" (padLeft (str p) 3)) " '") (nth arr p)) "' to '") b) "'")) (def arr (assoc arr p b))) (if (< x (+ (nth w 0) (nth w 1))) (do (println (str (str (str (str "  Delete @" (padLeft (str p) 3)) " '") (nth arr p)) "'")) (def j p) (while (< j (- (count arr) 1)) (do (def arr (assoc arr j (nth arr (+ j 1)))) (def j (+ j 1)))) (def arr (subs arr 0 (- (count arr) 1)))) (do (def r (randInt s 4)) (def s (nth r 0)) (def idx2 (int (nth r 1))) (def b (subs bases idx2 (+ idx2 1))) (def arr (conj arr "")) (def j (- (count arr) 1)) (while (> j p) (do (def arr (assoc arr j (nth arr (- j 1)))) (def j (- j 1)))) (println (str (str (str (str "  Insert @" (padLeft (str p) 3)) " '") b) "'")) (def arr (assoc arr p b))))) (def out "") (def i 0) (while (< i (count arr)) (do (def out (str out (nth arr i))) (def i (+ i 1)))) (throw (ex-info "return" {:v [s out]}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn prettyPrint [dna rowLen]
  (do (println "SEQUENCE:") (def le (count dna)) (def i 0) (while (< i le) (do (def k (+ i rowLen)) (when (> k le) (def k le)) (println (str (str (padLeft (str i) 5) ": ") (subs dna i k))) (def i (+ i rowLen)))) (def a 0) (def c 0) (def g 0) (def t 0) (def idx 0) (while (< idx le) (do (def ch (subs dna idx (+ idx 1))) (if (= ch "A") (def a (+ a 1)) (if (= ch "C") (def c (+ c 1)) (if (= ch "G") (def g (+ g 1)) (when (= ch "T") (def t (+ t 1)))))) (def idx (+ idx 1)))) (println "") (println "BASE COUNT:") (println (str "    A: " (padLeft (str a) 3))) (println (str "    C: " (padLeft (str c) 3))) (println (str "    G: " (padLeft (str g) 3))) (println (str "    T: " (padLeft (str t) 3))) (println "    ------") (println (str "    Σ: " (str le))) (println "    ======")))

(defn wstring [w]
  (try (throw (ex-info "return" {:v (str (str (str (str (str (str "  Change: " (str (nth w 0))) "\n  Delete: ") (str (nth w 1))) "\n  Insert: ") (str (nth w 2))) "\n")})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (do (def seed 1) (def res (makeSeq seed 250)) (def seed (nth res 0)) (def dna (str (nth res 1))) (prettyPrint dna 50) (def muts 10) (def w [100 100 100]) (println "\nWEIGHTS (ex 300):") (println (wstring w)) (println (str (str "MUTATIONS (" (str muts)) "):")) (def i 0) (while (< i muts) (do (def res (mutate seed dna w)) (def seed (nth res 0)) (def dna (str (nth res 1))) (def i (+ i 1)))) (println "") (prettyPrint dna 50)))

(defn -main []
  (main))

(-main)
