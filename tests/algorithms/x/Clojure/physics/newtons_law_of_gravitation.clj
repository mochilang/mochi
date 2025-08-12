(ns main (:refer-clojure :exclude [sqrtApprox gravitational_law]))

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

(defn _fetch [url]
  {:data [{:from "" :intensity {:actual 0 :forecast 0 :index ""} :to ""}]})

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare sqrtApprox gravitational_law)

(def ^:dynamic gravitational_law_d nil)

(def ^:dynamic gravitational_law_f nil)

(def ^:dynamic gravitational_law_m1 nil)

(def ^:dynamic gravitational_law_m2 nil)

(def ^:dynamic gravitational_law_product_of_mass nil)

(def ^:dynamic gravitational_law_zero_count nil)

(def ^:dynamic sqrtApprox_guess nil)

(def ^:dynamic sqrtApprox_i nil)

(def ^:dynamic main_GRAVITATIONAL_CONSTANT 0.000000000066743)

(defn sqrtApprox [sqrtApprox_x]
  (binding [sqrtApprox_guess nil sqrtApprox_i nil] (try (do (set! sqrtApprox_guess (/ sqrtApprox_x 2.0)) (set! sqrtApprox_i 0) (while (< sqrtApprox_i 20) (do (set! sqrtApprox_guess (/ (+ sqrtApprox_guess (/ sqrtApprox_x sqrtApprox_guess)) 2.0)) (set! sqrtApprox_i (+ sqrtApprox_i 1)))) (throw (ex-info "return" {:v sqrtApprox_guess}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn gravitational_law [gravitational_law_force gravitational_law_mass_1 gravitational_law_mass_2 gravitational_law_distance]
  (binding [gravitational_law_d nil gravitational_law_f nil gravitational_law_m1 nil gravitational_law_m2 nil gravitational_law_product_of_mass nil gravitational_law_zero_count nil] (try (do (set! gravitational_law_zero_count 0) (when (= gravitational_law_force 0.0) (set! gravitational_law_zero_count (+ gravitational_law_zero_count 1))) (when (= gravitational_law_mass_1 0.0) (set! gravitational_law_zero_count (+ gravitational_law_zero_count 1))) (when (= gravitational_law_mass_2 0.0) (set! gravitational_law_zero_count (+ gravitational_law_zero_count 1))) (when (= gravitational_law_distance 0.0) (set! gravitational_law_zero_count (+ gravitational_law_zero_count 1))) (when (not= gravitational_law_zero_count 1) (throw (Exception. "One and only one argument must be 0"))) (when (< gravitational_law_force 0.0) (throw (Exception. "Gravitational force can not be negative"))) (when (< gravitational_law_distance 0.0) (throw (Exception. "Distance can not be negative"))) (when (< gravitational_law_mass_1 0.0) (throw (Exception. "Mass can not be negative"))) (when (< gravitational_law_mass_2 0.0) (throw (Exception. "Mass can not be negative"))) (set! gravitational_law_product_of_mass (* gravitational_law_mass_1 gravitational_law_mass_2)) (when (= gravitational_law_force 0.0) (do (set! gravitational_law_f (/ (* main_GRAVITATIONAL_CONSTANT gravitational_law_product_of_mass) (* gravitational_law_distance gravitational_law_distance))) (throw (ex-info "return" {:v {:kind "force" :value gravitational_law_f}})))) (when (= gravitational_law_mass_1 0.0) (do (set! gravitational_law_m1 (/ (* gravitational_law_force (* gravitational_law_distance gravitational_law_distance)) (* main_GRAVITATIONAL_CONSTANT gravitational_law_mass_2))) (throw (ex-info "return" {:v {:kind "mass_1" :value gravitational_law_m1}})))) (when (= gravitational_law_mass_2 0.0) (do (set! gravitational_law_m2 (/ (* gravitational_law_force (* gravitational_law_distance gravitational_law_distance)) (* main_GRAVITATIONAL_CONSTANT gravitational_law_mass_1))) (throw (ex-info "return" {:v {:kind "mass_2" :value gravitational_law_m2}})))) (set! gravitational_law_d (sqrtApprox (/ (* main_GRAVITATIONAL_CONSTANT gravitational_law_product_of_mass) gravitational_law_force))) (throw (ex-info "return" {:v {:kind "distance" :value gravitational_law_d}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_r1 (gravitational_law 0.0 5.0 10.0 20.0))

(def ^:dynamic main_r2 (gravitational_law 7367.382 0.0 74.0 3048.0))

(def ^:dynamic main_r3 (gravitational_law 100.0 5.0 0.0 3.0))

(def ^:dynamic main_r4 (gravitational_law 100.0 5.0 10.0 0.0))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (str (:kind main_r1) " ") (str (:value main_r1))))
      (println (str (str (:kind main_r2) " ") (str (:value main_r2))))
      (println (str (str (:kind main_r3) " ") (str (:value main_r3))))
      (println (str (str (:kind main_r4) " ") (str (:value main_r4))))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
