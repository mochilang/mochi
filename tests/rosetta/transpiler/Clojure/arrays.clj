(ns main (:refer-clojure :exclude [listStr]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare listStr)

(defn listStr [xs_v0]
  (try (do (def s_v1 "[") (def i_v2 0) (while (< i_v2 (count xs_v0)) (do (def s_v1 (str s_v1 (str (nth xs_v0 i_v2)))) (when (< (+ i_v2 1) (count xs_v0)) (def s_v1 (str s_v1 " "))) (def i_v2 (+ i_v2 1)))) (def s_v1 (str s_v1 "]")) (throw (ex-info "return" {:v s_v1}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(def a [0 0 0 0 0])

(def s (subvec a 0 4))

(def cap_s 5)

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str "len(a) = " (str (count a))))
      (println (str "a = " (listStr a)))
      (def a (assoc a 0 3))
      (println (str "a = " (listStr a)))
      (println (str "a[0] = " (str (nth a 0))))
      (println (str "s = " (listStr s)))
      (println (str (str (str "len(s) = " (str (count s))) "  cap(s) = ") (str cap_s)))
      (def s (subvec a 0 5))
      (println (str "s = " (listStr s)))
      (def a (assoc a 0 22))
      (def s (assoc s 0 22))
      (println (str "a = " (listStr a)))
      (println (str "s = " (listStr s)))
      (def s (conj s 4))
      (def s (conj s 5))
      (def s (conj s 6))
      (def cap_s 10)
      (println (str "s = " (listStr s)))
      (println (str (str (str "len(s) = " (str (count s))) "  cap(s) = ") (str cap_s)))
      (def a (assoc a 4 (- 1)))
      (println (str "a = " (listStr a)))
      (println (str "s = " (listStr s)))
      (def s [])
      (dotimes [i 8] (def s (conj s 0)))
      (def cap_s 8)
      (println (str "s = " (listStr s)))
      (println (str (str (str "len(s) = " (str (count s))) "  cap(s) = ") (str cap_s)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
