(ns main (:refer-clojure :exclude [neighborsList plus isStable topple pileString]))

(require 'clojure.set)

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare neighborsList plus isStable topple pileString)

(defn neighborsList []
  (try (throw (ex-info "return" {:v [[1 3] [0 2 4] [1 5] [0 4 6] [1 3 5 7] [2 4 8] [3 7] [4 6 8] [5 7]]})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn plus [a b]
  (try (do (def res []) (def i 0) (while (< i (count a)) (do (def res (conj res (+ (nth a i) (nth b i)))) (def i (+ i 1)))) (throw (ex-info "return" {:v res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn isStable [p]
  (try (do (doseq [v p] (when (> v 3) (throw (ex-info "return" {:v false})))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn topple [p]
  (try (do (def neighbors (neighborsList)) (def i 0) (while (< i (count p)) (do (when (> (nth p i) 3) (do (def p (assoc p i (- (nth p i) 4))) (def nbs (nth neighbors i)) (doseq [j nbs] (def p (assoc p j (+ (nth p j) 1)))) (throw (ex-info "return" {:v 0})))) (def i (+ i 1)))) (throw (ex-info "return" {:v 0}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn pileString [p]
  (try (do (def s "") (def r 0) (while (< r 3) (do (def c 0) (while (< c 3) (do (def s (str (str s (str (nth p (+ (* 3 r) c)))) " ")) (def c (+ c 1)))) (def s (str s "\n")) (def r (+ r 1)))) (throw (ex-info "return" {:v s}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(def s4 [4 3 3 3 1 2 0 2 3])

(def s1 [1 2 0 2 1 1 0 1 3])

(def s2 [2 1 3 1 0 1 0 1 0])

(def s3_a (plus s1 s2))

(def s3_b (plus s2 s1))

(def s3 [3 3 3 3 3 3 3 3 3])

(def s3_id [2 1 2 1 0 1 2 1 2])

(def s4b (plus s3 s3_id))

(def s5 (plus s3_id s3_id))

(defn -main []
  (println "Avalanche of topplings:\n")
  (println (pileString s4))
  (while (not (isStable s4)) (do (topple s4) (println (pileString s4))))
  (println "Commutative additions:\n")
  (while (not (isStable s3_a)) (topple s3_a))
  (while (not (isStable s3_b)) (topple s3_b))
  (println (str (str (str (str (pileString s1) "\nplus\n\n") (pileString s2)) "\nequals\n\n") (pileString s3_a)))
  (println (str (str (str (str (str "and\n\n" (pileString s2)) "\nplus\n\n") (pileString s1)) "\nalso equals\n\n") (pileString s3_b)))
  (println "Addition of identity sandpile:\n")
  (while (not (isStable s4b)) (topple s4b))
  (println (str (str (str (str (pileString s3) "\nplus\n\n") (pileString s3_id)) "\nequals\n\n") (pileString s4b)))
  (println "Addition of identities:\n")
  (while (not (isStable s5)) (topple s5))
  (println (str (str (str (str (pileString s3_id) "\nplus\n\n") (pileString s3_id)) "\nequals\n\n") (pileString s5))))

(-main)
