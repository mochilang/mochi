(ns main (:refer-clojure :exclude [abs_float bisection f main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(defn indexOf [s sub]
  (let [idx (clojure.string/index-of s sub)] (if (nil? idx) -1 idx)))

(defn split [s sep]
  (clojure.string/split s (re-pattern sep)))

(defn toi [s]
  (Integer/parseInt (str s)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare abs_float bisection f main)

(def ^:dynamic bisection_end nil)

(def ^:dynamic bisection_fmid nil)

(def ^:dynamic bisection_mid nil)

(def ^:dynamic bisection_start nil)

(defn abs_float [abs_float_x]
  (try (if (< abs_float_x 0.0) (throw (ex-info "return" {:v (- abs_float_x)})) (throw (ex-info "return" {:v abs_float_x}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn bisection [f_v bisection_a bisection_b]
  (binding [bisection_end nil bisection_fmid nil bisection_mid nil bisection_start nil] (try (do (set! bisection_start bisection_a) (set! bisection_end bisection_b) (when (= (f_v bisection_a) 0.0) (throw (ex-info "return" {:v bisection_a}))) (when (= (f_v bisection_b) 0.0) (throw (ex-info "return" {:v bisection_b}))) (when (> (* (f_v bisection_a) (f_v bisection_b)) 0.0) (throw (Exception. "could not find root in given interval."))) (set! bisection_mid (+ bisection_start (/ (- bisection_end bisection_start) 2.0))) (while (> (abs_float (- bisection_start bisection_mid)) 0.0000001) (do (set! bisection_fmid (f_v bisection_mid)) (when (= bisection_fmid 0.0) (throw (ex-info "return" {:v bisection_mid}))) (if (< (* bisection_fmid (f_v bisection_start)) 0.0) (set! bisection_end bisection_mid) (set! bisection_start bisection_mid)) (set! bisection_mid (+ bisection_start (/ (- bisection_end bisection_start) 2.0))))) (throw (ex-info "return" {:v bisection_mid}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn f [f_x]
  (try (throw (ex-info "return" {:v (- (- (* (* f_x f_x) f_x) (* 2.0 f_x)) 5.0)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (println (str (bisection f 1.0 1000.0))))

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
