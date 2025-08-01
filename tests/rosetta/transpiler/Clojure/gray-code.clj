(ns main (:refer-clojure :exclude [xor enc dec binary pad5 main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare xor enc dec binary pad5 main)

(declare b bit d g p res s x y)

(defn xor [a b]
  (try (do (def res 0) (def bit 1) (def x a) (def y b) (while (or (> x 0) (> y 0)) (do (when (= (mod (+' (mod x 2) (mod y 2)) 2) 1) (def res (+' res bit))) (def x (/ x 2)) (def y (/ y 2)) (def bit (* bit 2)))) (throw (ex-info "return" {:v res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn enc [b]
  (try (throw (ex-info "return" {:v (xor b (/ b 2))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn dec [g]
  (try (do (def b 0) (def x g) (while (> x 0) (do (def b (xor b x)) (def x (/ x 2)))) (throw (ex-info "return" {:v b}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn binary [n]
  (try (do (when (= n 0) (throw (ex-info "return" {:v "0"}))) (def s "") (def x n) (while (> x 0) (do (if (= (mod x 2) 1) (def s (str "1" s)) (def s (str "0" s))) (def x (/ x 2)))) (throw (ex-info "return" {:v s}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn pad5 [s]
  (try (do (def p s) (while (< (count p) 5) (def p (str "0" p))) (throw (ex-info "return" {:v p}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (do (println "decimal  binary   gray    decoded") (def b 0) (while (< b 32) (do (def g (enc b)) (def d (dec g)) (println (str (str (str (str (str (str (str "  " (pad5 (binary b))) "   ") (pad5 (binary g))) "   ") (pad5 (binary d))) "  ") (str d))) (def b (+' b 1))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (main)
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
