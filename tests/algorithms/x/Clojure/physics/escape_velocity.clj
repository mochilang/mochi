(ns main (:refer-clojure :exclude [pow10 sqrt_newton round3 escape_velocity]))

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

(defn mochi_str [v]
  (cond (float? v) (let [s (str v)] (if (clojure.string/ends-with? s ".0") (subs s 0 (- (count s) 2)) s)) :else (str v)))

(defn _fetch [url]
  {:data [{:from "" :intensity {:actual 0 :forecast 0 :index ""} :to ""}]})

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare pow10 sqrt_newton round3 escape_velocity)

(def ^:dynamic escape_velocity_G nil)

(def ^:dynamic escape_velocity_velocity nil)

(def ^:dynamic pow10_k nil)

(def ^:dynamic pow10_m nil)

(def ^:dynamic pow10_p nil)

(def ^:dynamic round3_y nil)

(def ^:dynamic round3_yi nil)

(def ^:dynamic sqrt_newton_j nil)

(def ^:dynamic sqrt_newton_x nil)

(defn pow10 [pow10_n]
  (binding [pow10_k nil pow10_m nil pow10_p nil] (try (do (set! pow10_p 1.0) (set! pow10_k 0) (if (>= pow10_n 0) (while (< pow10_k pow10_n) (do (set! pow10_p (* pow10_p 10.0)) (set! pow10_k (+ pow10_k 1)))) (do (set! pow10_m (- pow10_n)) (while (< pow10_k pow10_m) (do (set! pow10_p (/ pow10_p 10.0)) (set! pow10_k (+ pow10_k 1)))))) (throw (ex-info "return" {:v pow10_p}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn sqrt_newton [sqrt_newton_n]
  (binding [sqrt_newton_j nil sqrt_newton_x nil] (try (do (when (= sqrt_newton_n 0.0) (throw (ex-info "return" {:v 0.0}))) (set! sqrt_newton_x sqrt_newton_n) (set! sqrt_newton_j 0) (while (< sqrt_newton_j 20) (do (set! sqrt_newton_x (/ (+ sqrt_newton_x (quot sqrt_newton_n sqrt_newton_x)) 2.0)) (set! sqrt_newton_j (+ sqrt_newton_j 1)))) (throw (ex-info "return" {:v sqrt_newton_x}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn round3 [round3_x]
  (binding [round3_y nil round3_yi nil] (try (do (set! round3_y (+ (* round3_x 1000.0) 0.5)) (set! round3_yi (long round3_y)) (when (> (double round3_yi) round3_y) (set! round3_yi (- round3_yi 1))) (throw (ex-info "return" {:v (/ (double round3_yi) 1000.0)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn escape_velocity [escape_velocity_mass escape_velocity_radius]
  (binding [escape_velocity_G nil escape_velocity_velocity nil] (try (do (when (= escape_velocity_radius 0.0) (throw (Exception. "Radius cannot be zero."))) (set! escape_velocity_G (* 6.6743 (pow10 (- 11)))) (set! escape_velocity_velocity (sqrt_newton (/ (* (* 2.0 escape_velocity_G) escape_velocity_mass) escape_velocity_radius))) (throw (ex-info "return" {:v (round3 escape_velocity_velocity)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (escape_velocity (* 5.972 (pow10 24)) (* 6.371 (pow10 6))))
      (println (escape_velocity (* 7.348 (pow10 22)) (* 1.737 (pow10 6))))
      (println (escape_velocity (* 1.898 (pow10 27)) (* 6.9911 (pow10 7))))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
