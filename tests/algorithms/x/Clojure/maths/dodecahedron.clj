(ns main (:refer-clojure :exclude [sqrtApprox abs_val approx_equal dodecahedron_surface_area dodecahedron_volume test_dodecahedron main]))

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

(declare sqrtApprox abs_val approx_equal dodecahedron_surface_area dodecahedron_volume test_dodecahedron main)

(def ^:dynamic dodecahedron_surface_area_e nil)

(def ^:dynamic dodecahedron_surface_area_term nil)

(def ^:dynamic dodecahedron_volume_e nil)

(def ^:dynamic dodecahedron_volume_term nil)

(def ^:dynamic sqrtApprox_guess nil)

(def ^:dynamic sqrtApprox_i nil)

(defn sqrtApprox [sqrtApprox_x]
  (binding [sqrtApprox_guess nil sqrtApprox_i nil] (try (do (set! sqrtApprox_guess (/ sqrtApprox_x 2.0)) (set! sqrtApprox_i 0) (while (< sqrtApprox_i 20) (do (set! sqrtApprox_guess (/ (+ sqrtApprox_guess (quot sqrtApprox_x sqrtApprox_guess)) 2.0)) (set! sqrtApprox_i (+ sqrtApprox_i 1)))) (throw (ex-info "return" {:v sqrtApprox_guess}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn abs_val [abs_val_num]
  (try (if (< abs_val_num 0.0) (- abs_val_num) abs_val_num) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn approx_equal [approx_equal_a approx_equal_b approx_equal_eps]
  (try (throw (ex-info "return" {:v (< (abs_val (- approx_equal_a approx_equal_b)) approx_equal_eps)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn dodecahedron_surface_area [dodecahedron_surface_area_edge]
  (binding [dodecahedron_surface_area_e nil dodecahedron_surface_area_term nil] (try (do (when (<= dodecahedron_surface_area_edge 0) (throw (Exception. "Length must be a positive."))) (set! dodecahedron_surface_area_term (sqrtApprox (+ 25.0 (* 10.0 (sqrtApprox 5.0))))) (set! dodecahedron_surface_area_e (double dodecahedron_surface_area_edge)) (throw (ex-info "return" {:v (* (* (* 3.0 dodecahedron_surface_area_term) dodecahedron_surface_area_e) dodecahedron_surface_area_e)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn dodecahedron_volume [dodecahedron_volume_edge]
  (binding [dodecahedron_volume_e nil dodecahedron_volume_term nil] (try (do (when (<= dodecahedron_volume_edge 0) (throw (Exception. "Length must be a positive."))) (set! dodecahedron_volume_term (/ (+ 15.0 (* 7.0 (sqrtApprox 5.0))) 4.0)) (set! dodecahedron_volume_e (double dodecahedron_volume_edge)) (throw (ex-info "return" {:v (* (* (* dodecahedron_volume_term dodecahedron_volume_e) dodecahedron_volume_e) dodecahedron_volume_e)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn test_dodecahedron []
  (do (when (not (approx_equal (dodecahedron_surface_area 5) 516.1432201766901 0.0001)) (throw (Exception. "surface area 5 failed"))) (when (not (approx_equal (dodecahedron_surface_area 10) 2064.5728807067603 0.0001)) (throw (Exception. "surface area 10 failed"))) (when (not (approx_equal (dodecahedron_volume 5) 957.8898700780791 0.0001)) (throw (Exception. "volume 5 failed"))) (when (not (approx_equal (dodecahedron_volume 10) 7663.118960624633 0.0001)) (throw (Exception. "volume 10 failed")))))

(defn main []
  (do (test_dodecahedron) (println (dodecahedron_surface_area 5)) (println (dodecahedron_volume 5))))

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
