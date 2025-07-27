(ns main (:refer-clojure :exclude [add mul neg inv conj cstr]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare add mul neg inv conj cstr)

(defn add [a b]
  (try (throw (ex-info "return" {:v {:re (+ (:re a) (:re b)) :im (+ (:im a) (:im b))}})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn mul [a b]
  (try (throw (ex-info "return" {:v {:re (- (* (:re a) (:re b)) (* (:im a) (:im b))) :im (+ (* (:re a) (:im b)) (* (:im a) (:re b)))}})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn neg [a]
  (try (throw (ex-info "return" {:v {:re (- (:re a)) :im (- (:im a))}})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn inv [a]
  (try (do (def denom (+ (* (:re a) (:re a)) (* (:im a) (:im a)))) (throw (ex-info "return" {:v {:re (/ (:re a) denom) :im (/ (- (:im a)) denom)}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn conj [a]
  (try (throw (ex-info "return" {:v {:re (:re a) :im (- (:im a))}})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn cstr [a]
  (try (do (def s (str "(" (str (:re a)))) (if (>= (:im a) 0) (def s (str (str (str s "+") (str (:im a))) "i)")) (def s (str (str s (str (:im a))) "i)"))) (throw (ex-info "return" {:v s}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(def a {:re 1.0 :im 1.0})

(def b {:re 3.14159 :im 1.25})

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str "a:       " (cstr a)))
      (println (str "b:       " (cstr b)))
      (println (str "a + b:   " (cstr (add a b))))
      (println (str "a * b:   " (cstr (mul a b))))
      (println (str "-a:      " (cstr (neg a))))
      (println (str "1 / a:   " (cstr (inv a))))
      (println (str "a̅:       " (cstr (conj a))))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
