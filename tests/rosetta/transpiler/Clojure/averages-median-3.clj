(ns main (:refer-clojure :exclude [qsel median]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare qsel median)

(defn qsel [a k]
  (try (do (def arr a) (while (> (count arr) 1) (do (def px (mod (swap! nowSeed (fn [s] (mod (+ (* s 1664525) 1013904223) 2147483647))) (count arr))) (def pv (nth arr px)) (def last (- (count arr) 1)) (def tmp (nth arr px)) (def arr (assoc arr px (nth arr last))) (def arr (assoc arr last tmp)) (def px 0) (def i 0) (while (< i last) (do (def v (nth arr i)) (when (< v pv) (do (def tmp2 (nth arr px)) (def arr (assoc arr px (nth arr i))) (def arr (assoc arr i tmp2)) (def px (+ px 1)))) (def i (+ i 1)))) (when (= px k) (throw (ex-info "return" {:v pv}))) (if (< k px) (def arr (subvec arr 0 px)) (do (def tmp2 (nth arr px)) (def arr (assoc arr px pv)) (def arr (assoc arr last tmp2)) (def arr (subvec arr (+ px 1) (count arr))) (def k (- k (+ px 1))))))) (throw (ex-info "return" {:v (nth arr 0)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn median [list]
  (try (do (def arr list) (def half (int (/ (count arr) 2))) (def med (qsel arr half)) (if (= (mod (count arr) 2) 0) (/ (+ med (qsel arr (- half 1))) 2.0) med)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (median [3.0 1.0 4.0 1.0])))
      (println (str (median [3.0 1.0 4.0 1.0 5.0])))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
