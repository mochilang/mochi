(ns main (:refer-clojure :exclude [indexOf set58 doubleSHA256 computeChecksum validA58]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare indexOf set58 doubleSHA256 computeChecksum validA58)

(defn indexOf [s ch]
  (try (do (def i 0) (while (< i (count s)) (do (when (= (subs s i (+ i 1)) ch) (throw (ex-info "return" {:v i}))) (def i (+ i 1)))) (throw (ex-info "return" {:v (- 1)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn set58 [addr]
  (try (do (def tmpl "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz") (def a []) (def i 0) (while (< i 25) (do (def a (conj a 0)) (def i (+ i 1)))) (def idx 0) (while (< idx (count addr)) (do (def ch (subs addr idx (+ idx 1))) (def c (indexOf tmpl ch)) (when (< c 0) (throw (ex-info "return" {:v []}))) (def j 24) (while (>= j 0) (do (def c (+ c (* 58 (nth a j)))) (def a (assoc a j (mod c 256))) (def c (int (/ c 256))) (def j (- j 1)))) (when (> c 0) (throw (ex-info "return" {:v []}))) (def idx (+ idx 1)))) (throw (ex-info "return" {:v a}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn doubleSHA256 [bs]
  (try (do (def first (sha256 bs)) (throw (ex-info "return" {:v (sha256 first)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn computeChecksum [a]
  (try (do (def hash (doubleSHA256 (subvec a 0 21))) (throw (ex-info "return" {:v (subvec hash 0 4)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn validA58 [addr]
  (try (do (def a (set58 addr)) (when (not= (count a) 25) (throw (ex-info "return" {:v false}))) (when (not= (nth a 0) 0) (throw (ex-info "return" {:v false}))) (def sum (computeChecksum a)) (def i 0) (while (< i 4) (do (when (not= (nth a (+ 21 i)) (nth sum i)) (throw (ex-info "return" {:v false}))) (def i (+ i 1)))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn -main []
  (println (str (validA58 "1AGNa15ZQXAZUgFiqJ3i7Z2DPU2J6hW62i")))
  (println (str (validA58 "17NdbrSGoUotzeGCcMMCqnFkEvLymoou9j"))))

(-main)
