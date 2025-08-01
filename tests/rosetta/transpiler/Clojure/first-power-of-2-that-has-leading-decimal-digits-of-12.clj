(ns main (:refer-clojure :exclude [commatize p]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare commatize p)

(declare L Ls count_v d digits i idx le m out params probe ps s ten18)

(defn commatize [n]
  (try (do (def s (str n)) (def i (mod (count s) 3)) (when (= i 0) (def i 3)) (def out (subs s 0 i)) (while (< i (count s)) (do (def out (str (str out ",") (subs s i (+' i 3)))) (def i (+' i 3)))) (throw (ex-info "return" {:v out}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn p [L n]
  (try (do (def Ls (str L)) (def digits 1) (def d 1) (while (<= d (- 18 (count Ls))) (do (def digits (* digits 10)) (def d (+' d 1)))) (def ten18 1000000000000000000) (def count_v 0) (def i 0) (def probe 1) (loop [while_flag_1 true] (when (and while_flag_1 true) (do (def probe (* probe 2)) (def i (+' i 1)) (when (>= probe ten18) (loop [while_flag_2 true] (when (and while_flag_2 true) (do (when (>= probe ten18) (def probe (/ probe 10))) (cond (= (/ probe digits) L) (do (def count_v (+' count_v 1)) (when (>= count_v n) (do (def count_v (- count_v 1)) (recur false)))) :else (do (def probe (* probe 2)) (def i (+' i 1)) (recur while_flag_2))))))) (def ps (str probe)) (def le (count Ls)) (when (> le (count ps)) (def le (count ps))) (cond (= (subs ps 0 le) Ls) (do (def count_v (+' count_v 1)) (when (>= count_v n) (recur false))) :else (recur while_flag_1))))) (throw (ex-info "return" {:v i}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(def params [[12 1] [12 2] [123 45]])

(def idx 0)

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (while (< idx (count params)) (do (def L (nth (nth params idx) 0)) (def m (nth (nth params idx) 1)) (println (str (str (str (str (str "p(" (str L)) ", ") (str m)) ") = ") (commatize (p L m)))) (def idx (+' idx 1))))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
