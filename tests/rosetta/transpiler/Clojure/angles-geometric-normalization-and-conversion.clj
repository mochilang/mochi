(ns main (:refer-clojure :exclude [d2d g2g m2m r2r d2g d2m d2r g2d g2m g2r m2d m2g m2r r2d r2g r2m main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare d2d g2g m2m r2r d2g d2m d2r g2d g2m g2r m2d m2g m2r r2d r2g r2m main)

(defn d2d [d]
  (try (throw (ex-info "return" {:v (mod d 360)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn g2g [g]
  (try (throw (ex-info "return" {:v (mod g 400)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn m2m [m]
  (try (throw (ex-info "return" {:v (mod m 6400)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn r2r [r]
  (try (throw (ex-info "return" {:v (mod r (* 2 3.141592653589793))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn d2g [d]
  (try (throw (ex-info "return" {:v (/ (* (d2d d) 400) 360)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn d2m [d]
  (try (throw (ex-info "return" {:v (/ (* (d2d d) 6400) 360)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn d2r [d]
  (try (throw (ex-info "return" {:v (/ (* (d2d d) 3.141592653589793) 180)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn g2d [g]
  (try (throw (ex-info "return" {:v (/ (* (g2g g) 360) 400)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn g2m [g]
  (try (throw (ex-info "return" {:v (/ (* (g2g g) 6400) 400)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn g2r [g]
  (try (throw (ex-info "return" {:v (/ (* (g2g g) 3.141592653589793) 200)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn m2d [m]
  (try (throw (ex-info "return" {:v (/ (* (m2m m) 360) 6400)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn m2g [m]
  (try (throw (ex-info "return" {:v (/ (* (m2m m) 400) 6400)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn m2r [m]
  (try (throw (ex-info "return" {:v (/ (* (m2m m) 3.141592653589793) 3200)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn r2d [r]
  (try (throw (ex-info "return" {:v (/ (* (r2r r) 180) 3.141592653589793)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn r2g [r]
  (try (throw (ex-info "return" {:v (/ (* (r2r r) 200) 3.141592653589793)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn r2m [r]
  (try (throw (ex-info "return" {:v (/ (* (r2r r) 3200) 3.141592653589793)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (do (def angles [(- 2) (- 1) 0 1 2 6.2831853 16 57.2957795 359 399 6399 1000000]) (println "degrees normalized_degs gradians mils radians") (doseq [a angles] (println (str (str (str (str (str (str (str (str (str a) " ") (str (d2d a))) " ") (str (d2g a))) " ") (str (d2m a))) " ") (str (d2r a))))) (println "\ngradians normalized_grds degrees mils radians") (doseq [a angles] (println (str (str (str (str (str (str (str (str (str a) " ") (str (g2g a))) " ") (str (g2d a))) " ") (str (g2m a))) " ") (str (g2r a))))) (println "\nmils normalized_mils degrees gradians radians") (doseq [a angles] (println (str (str (str (str (str (str (str (str (str a) " ") (str (m2m a))) " ") (str (m2d a))) " ") (str (m2g a))) " ") (str (m2r a))))) (println "\nradians normalized_rads degrees gradians mils") (doseq [a angles] (println (str (str (str (str (str (str (str (str (str a) " ") (str (r2r a))) " ") (str (r2d a))) " ") (str (r2g a))) " ") (str (r2m a)))))))

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
