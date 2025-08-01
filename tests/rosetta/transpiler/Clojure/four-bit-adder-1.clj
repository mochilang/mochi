(ns main (:refer-clojure :exclude [xor ha fa add4 b2i main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare xor ha fa add4 b2i main)

(declare r r0 r1 r2 r3)

(defn xor [a b]
  (try (throw (ex-info "return" {:v (or (and a (not b)) (and (not a) b))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn ha [a b]
  (try (throw (ex-info "return" {:v {:s (xor a b) :c (and a b)}})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn fa [a b c0]
  (try (do (def r1 (ha a c0)) (def r2 (ha (:s r1) b)) (throw (ex-info "return" {:v {:s (:s r2) :c (or (:c r1) (:c r2))}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn add4 [a3 a2 a1 a0 b3 b2 b1 b0]
  (try (do (def r0 (fa a0 b0 false)) (def r1 (fa a1 b1 (:c r0))) (def r2 (fa a2 b2 (:c r1))) (def r3 (fa a3 b3 (:c r2))) (throw (ex-info "return" {:v {:v (:c r3) :s3 (:s r3) :s2 (:s r2) :s1 (:s r1) :s0 (:s r0)}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn b2i [b]
  (try (if b 1 0) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (do (def r (add4 true false true false true false false true)) (println (str (str (str (str (str (str (str (str (str (b2i (:v r))) " ") (str (b2i (:s3 r)))) " ") (str (b2i (:s2 r)))) " ") (str (b2i (:s1 r)))) " ") (str (b2i (:s0 r)))))))

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
