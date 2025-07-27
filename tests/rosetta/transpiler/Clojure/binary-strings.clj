(ns main (:refer-clojure :exclude [char fromBytes]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare char fromBytes)

(defn char [n]
  (try (do (def letters "abcdefghijklmnopqrstuvwxyz") (def idx (- n 97)) (if (or (< idx 0) (>= idx (count letters))) "?" (subs letters idx (+ idx 1)))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn fromBytes [bs]
  (try (do (def s "") (def i 0) (while (< i (count bs)) (do (def s (str s (char (nth bs i)))) (def i (+ i 1)))) (throw (ex-info "return" {:v s}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn -main []
  (def b [98 105 110 97 114 121])
  (println (str b))
  (def c b)
  (println (str c))
  (println (str (= b c)))
  (def d [])
  (def i 0)
  (while (< i (count b)) (do (def d (conj d (nth b i))) (def i (+ i 1))))
  (def d (assoc d 1 97))
  (def d (assoc d 4 110))
  (println (fromBytes b))
  (println (fromBytes d))
  (println (str (= (count b) 0)))
  (def z (conj b 122))
  (println (fromBytes z))
  (def sub (subvec b 1 3))
  (println (fromBytes sub))
  (def f [])
  (def i 0)
  (while (< i (count d)) (do (def val (nth d i)) (if (= val 110) (def f (conj f 109)) (def f (conj f val))) (def i (+ i 1))))
  (println (str (str (fromBytes d) " -> ") (fromBytes f)))
  (def rem [])
  (def rem (conj rem (nth b 0)))
  (def i 3)
  (while (< i (count b)) (do (def rem (conj rem (nth b i))) (def i (+ i 1))))
  (println (fromBytes rem)))

(-main)
