(ns main (:refer-clojure :exclude [add sub mul dot intersectPoint main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare add sub mul dot intersectPoint main)

(declare diff ip pn pp prod1 prod2 prod3 rp rv)

(defn add [a b]
  (try (throw (ex-info "return" {:v {:x (+' (:x a) (:x b)) :y (+' (:y a) (:y b)) :z (+' (:z a) (:z b))}})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn sub [a b]
  (try (throw (ex-info "return" {:v {:x (- (:x a) (:x b)) :y (- (:y a) (:y b)) :z (- (:z a) (:z b))}})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn mul [v s]
  (try (throw (ex-info "return" {:v {:x (* (:x v) s) :y (* (:y v) s) :z (* (:z v) s)}})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn dot [a b]
  (try (throw (ex-info "return" {:v (+' (+' (* (:x a) (:x b)) (* (:y a) (:y b))) (* (:z a) (:z b)))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn intersectPoint [rv rp pn pp]
  (try (do (def diff (sub rp pp)) (def prod1 (dot diff pn)) (def prod2 (dot rv pn)) (def prod3 (/ prod1 prod2)) (throw (ex-info "return" {:v (sub rp (mul rv prod3))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (do (def rv {:x 0.0 :y (- 1.0) :z (- 1.0)}) (def rp {:x 0.0 :y 0.0 :z 10.0}) (def pn {:x 0.0 :y 0.0 :z 1.0}) (def pp {:x 0.0 :y 0.0 :z 5.0}) (def ip (intersectPoint rv rp pn pp)) (println (str (str (str (str (str (str "The ray intersects the plane at (" (str (:x ip))) ", ") (str (:y ip))) ", ") (str (:z ip))) ")"))))

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
