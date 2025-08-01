(ns main (:refer-clojure :exclude [qsel fivenum]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare qsel fivenum)

(declare arr arr2 i last m n5 pv px q3 t tmp v x1 x2 x3)

(defn qsel [a k_p]
  (try (do (def k k_p) (def arr a) (while (> (count arr) 1) (do (def px (mod (swap! nowSeed (fn [s] (mod (+ (* s 1664525) 1013904223) 2147483647))) (count arr))) (def pv (nth arr px)) (def last (- (count arr) 1)) (def tmp (nth arr px)) (def arr (assoc arr px (nth arr last))) (def arr (assoc arr last tmp)) (def px 0) (def i 0) (while (< i last) (do (def v (nth arr i)) (when (< v pv) (do (def t (nth arr px)) (def arr (assoc arr px (nth arr i))) (def arr (assoc arr i t)) (def px (+' px 1)))) (def i (+' i 1)))) (def arr (assoc arr px pv)) (when (= px k) (throw (ex-info "return" {:v pv}))) (if (< k px) (def arr (subvec arr 0 px)) (do (def arr (subvec arr (+' px 1) (count arr))) (def k (- k (+' px 1))))))) (throw (ex-info "return" {:v (nth arr 0)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn fivenum [a]
  (try (do (def last (- (count a) 1)) (def m (/ last 2)) (def n5 []) (def n5 (conj n5 (qsel (subvec a 0 m) 0))) (def n5 (conj n5 (qsel (subvec a 0 m) (/ (count a) 4)))) (def n5 (conj n5 (qsel a m))) (def arr2 (subvec a m (count a))) (def q3 (- (- last m) (/ (count a) 4))) (def n5 (conj n5 (qsel arr2 q3))) (def arr2 (subvec arr2 q3 (count arr2))) (def n5 (conj n5 (qsel arr2 (- (count arr2) 1)))) (throw (ex-info "return" {:v n5}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(def x1 [36.0 40.0 7.0 39.0 41.0 15.0])

(def x2 [15.0 6.0 42.0 41.0 7.0 36.0 49.0 40.0 39.0 47.0 43.0])

(def x3 [0.14082834 0.0974879 1.73131507 0.87636009 (- 1.95059594) 0.73438555 (- 0.03035726) 1.4667597 (- 0.74621349) (- 0.72588772) 0.6390516 0.61501527 (- 0.9898378) (- 1.00447874) (- 0.62759469) 0.66206163 1.04312009 (- 0.10305385) 0.75775634 0.32566578])

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (fivenum x1)))
      (println (str (fivenum x2)))
      (println (str (fivenum x3)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
