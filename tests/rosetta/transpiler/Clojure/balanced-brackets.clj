(ns main (:refer-clojure :exclude [prng gen testBalanced main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare prng gen testBalanced main)

(defn prng [max]
  (try (do (def seed (mod (+ (* seed 1103515245) 12345) 2147483648)) (throw (ex-info "return" {:v (mod seed max)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn gen [n]
  (try (do (def arr []) (def i 0) (while (< i n) (do (def arr (conj arr "[")) (def arr (conj arr "]")) (def i (+ i 1)))) (def j (- (count arr) 1)) (while (> j 0) (do (def k (prng (+ j 1))) (def tmp (nth arr j)) (def arr (assoc arr j (nth arr k))) (def arr (assoc arr k tmp)) (def j (- j 1)))) (def out "") (doseq [ch arr] (def out (str out ch))) (throw (ex-info "return" {:v out}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn testBalanced [s]
  (try (do (def open 0) (def i 0) (while (< i (count s)) (do (def c (subs s i (+ i 1))) (if (= c "[") (def open (+ open 1)) (if (= c "]") (do (when (= open 0) (do (println (str s ": not ok")) (throw (ex-info "return" {:v nil})))) (def open (- open 1))) (do (println (str s ": not ok")) (throw (ex-info "return" {:v nil}))))) (def i (+ i 1)))) (if (= open 0) (println (str s ": ok")) (println (str s ": not ok")))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (do (def i 0) (while (< i 10) (do (testBalanced (gen i)) (def i (+ i 1)))) (testBalanced "()")))

(defn -main []
  (def seed 1)
  (main))

(-main)
