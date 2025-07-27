(ns main (:refer-clojure :exclude [sortFloat median]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare sortFloat median)

(defn sortFloat [xs]
  (try (do (def arr xs) (def n (count arr)) (def i 0) (while (< i n) (do (def j 0) (while (< j (- n 1)) (do (when (> (nth arr j) (nth arr (+ j 1))) (do (def tmp (nth arr j)) (def arr (assoc arr j (nth arr (+ j 1)))) (def arr (assoc arr (+ j 1) tmp)))) (def j (+ j 1)))) (def i (+ i 1)))) (throw (ex-info "return" {:v arr}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn median [a]
  (try (do (def arr (sortFloat a)) (def half (int (/ (count arr) 2))) (def m (nth arr half)) (when (= (mod (count arr) 2) 0) (def m (/ (+ m (nth arr (- half 1))) 2.0))) (throw (ex-info "return" {:v m}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

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
