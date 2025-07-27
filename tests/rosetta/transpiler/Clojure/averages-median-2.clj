(ns main (:refer-clojure :exclude [sel median]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare sel median)

(defn sel [list k]
  (try (do (def i 0) (while (<= i k) (do (def minIndex i) (def j (+ i 1)) (while (< j (count list)) (do (when (< (nth list j) (nth list minIndex)) (def minIndex j)) (def j (+ j 1)))) (def tmp (nth list i)) (def list (assoc list i (nth list minIndex))) (def list (assoc list minIndex tmp)) (def i (+ i 1)))) (throw (ex-info "return" {:v (nth list k)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn median [a]
  (try (do (def arr a) (def half (int (/ (count arr) 2))) (def med (sel arr half)) (if (= (mod (count arr) 2) 0) (/ (+ med (nth arr (- half 1))) 2.0) med)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

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
