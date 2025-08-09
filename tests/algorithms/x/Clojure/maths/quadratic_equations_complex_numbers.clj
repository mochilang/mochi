(ns main (:refer-clojure :exclude [add sub div_real sqrt_newton sqrt_to_complex quadratic_roots root_str main]))

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

(declare add sub div_real sqrt_newton sqrt_to_complex quadratic_roots root_str main)

(def ^:dynamic main_roots nil)

(def ^:dynamic quadratic_roots_delta nil)

(def ^:dynamic quadratic_roots_minus_b nil)

(def ^:dynamic quadratic_roots_root1 nil)

(def ^:dynamic quadratic_roots_root2 nil)

(def ^:dynamic quadratic_roots_sqrt_d nil)

(def ^:dynamic quadratic_roots_two_a nil)

(def ^:dynamic root_str_s nil)

(def ^:dynamic sqrt_newton_guess nil)

(def ^:dynamic sqrt_newton_i nil)

(defn add [add_a add_b]
  (try (throw (ex-info "return" {:v {:im (+ (:im add_a) (:im add_b)) :re (+ (:re add_a) (:re add_b))}})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn sub [sub_a sub_b]
  (try (throw (ex-info "return" {:v {:im (- (:im sub_a) (:im sub_b)) :re (- (:re sub_a) (:re sub_b))}})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn div_real [div_real_a div_real_r]
  (try (throw (ex-info "return" {:v {:im (quot (:im div_real_a) div_real_r) :re (quot (:re div_real_a) div_real_r)}})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn sqrt_newton [sqrt_newton_x]
  (binding [sqrt_newton_guess nil sqrt_newton_i nil] (try (do (when (<= sqrt_newton_x 0.0) (throw (ex-info "return" {:v 0.0}))) (set! sqrt_newton_guess (/ sqrt_newton_x 2.0)) (set! sqrt_newton_i 0) (while (< sqrt_newton_i 20) (do (set! sqrt_newton_guess (/ (+ sqrt_newton_guess (quot sqrt_newton_x sqrt_newton_guess)) 2.0)) (set! sqrt_newton_i (+ sqrt_newton_i 1)))) (throw (ex-info "return" {:v sqrt_newton_guess}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn sqrt_to_complex [sqrt_to_complex_d]
  (try (if (>= sqrt_to_complex_d 0.0) {:im 0.0 :re (sqrt_newton sqrt_to_complex_d)} {:im (sqrt_newton (- sqrt_to_complex_d)) :re 0.0}) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn quadratic_roots [quadratic_roots_a quadratic_roots_b quadratic_roots_c]
  (binding [quadratic_roots_delta nil quadratic_roots_minus_b nil quadratic_roots_root1 nil quadratic_roots_root2 nil quadratic_roots_sqrt_d nil quadratic_roots_two_a nil] (try (do (when (= quadratic_roots_a 0.0) (do (println "ValueError: coefficient 'a' must not be zero") (throw (ex-info "return" {:v []})))) (set! quadratic_roots_delta (- (* quadratic_roots_b quadratic_roots_b) (* (* 4.0 quadratic_roots_a) quadratic_roots_c))) (set! quadratic_roots_sqrt_d (sqrt_to_complex quadratic_roots_delta)) (set! quadratic_roots_minus_b {:im 0.0 :re (- quadratic_roots_b)}) (set! quadratic_roots_two_a (* 2.0 quadratic_roots_a)) (set! quadratic_roots_root1 (div_real (add quadratic_roots_minus_b quadratic_roots_sqrt_d) quadratic_roots_two_a)) (set! quadratic_roots_root2 (div_real (sub quadratic_roots_minus_b quadratic_roots_sqrt_d) quadratic_roots_two_a)) (throw (ex-info "return" {:v [quadratic_roots_root1 quadratic_roots_root2]}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn root_str [root_str_r]
  (binding [root_str_s nil] (try (do (when (= (:im root_str_r) 0.0) (throw (ex-info "return" {:v (str (:re root_str_r))}))) (set! root_str_s (str (:re root_str_r))) (if (>= (:im root_str_r) 0.0) (set! root_str_s (str (str (str root_str_s "+") (str (:im root_str_r))) "i")) (set! root_str_s (str (str root_str_s (str (:im root_str_r))) "i"))) (throw (ex-info "return" {:v root_str_s}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_roots nil] (do (set! main_roots (quadratic_roots 5.0 6.0 1.0)) (when (= (count main_roots) 2) (println (str (str (str "The solutions are: " (root_str (nth main_roots 0))) " and ") (root_str (nth main_roots 1))))))))

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
