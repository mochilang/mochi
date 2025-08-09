(ns main (:refer-clojure :exclude [create_vector get_3d_vectors_cross pow10 round_float is_zero_vector are_collinear test_are_collinear main]))

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

(declare create_vector get_3d_vectors_cross pow10 round_float is_zero_vector are_collinear test_are_collinear main)

(def ^:dynamic are_collinear_ab nil)

(def ^:dynamic are_collinear_ac nil)

(def ^:dynamic are_collinear_cross nil)

(def ^:dynamic create_vector_vx nil)

(def ^:dynamic create_vector_vy nil)

(def ^:dynamic create_vector_vz nil)

(def ^:dynamic get_3d_vectors_cross_cx nil)

(def ^:dynamic get_3d_vectors_cross_cy nil)

(def ^:dynamic get_3d_vectors_cross_cz nil)

(def ^:dynamic main_a nil)

(def ^:dynamic main_b nil)

(def ^:dynamic main_c nil)

(def ^:dynamic main_d nil)

(def ^:dynamic main_e nil)

(def ^:dynamic main_f nil)

(def ^:dynamic pow10_i nil)

(def ^:dynamic pow10_result nil)

(def ^:dynamic round_float_factor nil)

(def ^:dynamic round_float_t nil)

(def ^:dynamic round_float_v nil)

(def ^:dynamic test_are_collinear_p1 nil)

(def ^:dynamic test_are_collinear_p2 nil)

(def ^:dynamic test_are_collinear_p3 nil)

(def ^:dynamic test_are_collinear_q3 nil)

(defn create_vector [create_vector_p1 create_vector_p2]
  (binding [create_vector_vx nil create_vector_vy nil create_vector_vz nil] (try (do (set! create_vector_vx (- (:x create_vector_p2) (:x create_vector_p1))) (set! create_vector_vy (- (:y create_vector_p2) (:y create_vector_p1))) (set! create_vector_vz (- (:z create_vector_p2) (:z create_vector_p1))) (throw (ex-info "return" {:v {:x create_vector_vx :y create_vector_vy :z create_vector_vz}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn get_3d_vectors_cross [get_3d_vectors_cross_ab get_3d_vectors_cross_ac]
  (binding [get_3d_vectors_cross_cx nil get_3d_vectors_cross_cy nil get_3d_vectors_cross_cz nil] (try (do (set! get_3d_vectors_cross_cx (- (* (:y get_3d_vectors_cross_ab) (:z get_3d_vectors_cross_ac)) (* (:z get_3d_vectors_cross_ab) (:y get_3d_vectors_cross_ac)))) (set! get_3d_vectors_cross_cy (- (* (:z get_3d_vectors_cross_ab) (:x get_3d_vectors_cross_ac)) (* (:x get_3d_vectors_cross_ab) (:z get_3d_vectors_cross_ac)))) (set! get_3d_vectors_cross_cz (- (* (:x get_3d_vectors_cross_ab) (:y get_3d_vectors_cross_ac)) (* (:y get_3d_vectors_cross_ab) (:x get_3d_vectors_cross_ac)))) (throw (ex-info "return" {:v {:x get_3d_vectors_cross_cx :y get_3d_vectors_cross_cy :z get_3d_vectors_cross_cz}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn pow10 [pow10_exp]
  (binding [pow10_i nil pow10_result nil] (try (do (set! pow10_result 1.0) (set! pow10_i 0) (while (< pow10_i pow10_exp) (do (set! pow10_result (* pow10_result 10.0)) (set! pow10_i (+ pow10_i 1)))) (throw (ex-info "return" {:v pow10_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn round_float [round_float_x round_float_digits]
  (binding [round_float_factor nil round_float_t nil round_float_v nil] (try (do (set! round_float_factor (pow10 round_float_digits)) (set! round_float_v (* round_float_x round_float_factor)) (if (>= round_float_v 0.0) (set! round_float_v (+ round_float_v 0.5)) (set! round_float_v (- round_float_v 0.5))) (set! round_float_t (long round_float_v)) (throw (ex-info "return" {:v (quot (double round_float_t) round_float_factor)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn is_zero_vector [is_zero_vector_v is_zero_vector_accuracy]
  (try (throw (ex-info "return" {:v (and (and (= (round_float (:x is_zero_vector_v) is_zero_vector_accuracy) 0.0) (= (round_float (:y is_zero_vector_v) is_zero_vector_accuracy) 0.0)) (= (round_float (:z is_zero_vector_v) is_zero_vector_accuracy) 0.0))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn are_collinear [are_collinear_a are_collinear_b are_collinear_c are_collinear_accuracy]
  (binding [are_collinear_ab nil are_collinear_ac nil are_collinear_cross nil] (try (do (set! are_collinear_ab (create_vector are_collinear_a are_collinear_b)) (set! are_collinear_ac (create_vector are_collinear_a are_collinear_c)) (set! are_collinear_cross (get_3d_vectors_cross are_collinear_ab are_collinear_ac)) (throw (ex-info "return" {:v (is_zero_vector are_collinear_cross are_collinear_accuracy)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn test_are_collinear []
  (binding [test_are_collinear_p1 nil test_are_collinear_p2 nil test_are_collinear_p3 nil test_are_collinear_q3 nil] (do (set! test_are_collinear_p1 {:x 0.0 :y 0.0 :z 0.0}) (set! test_are_collinear_p2 {:x 1.0 :y 1.0 :z 1.0}) (set! test_are_collinear_p3 {:x 2.0 :y 2.0 :z 2.0}) (when (not (are_collinear test_are_collinear_p1 test_are_collinear_p2 test_are_collinear_p3 10)) (throw (Exception. "collinear test failed"))) (set! test_are_collinear_q3 {:x 1.0 :y 2.0 :z 3.0}) (when (are_collinear test_are_collinear_p1 test_are_collinear_p2 test_are_collinear_q3 10) (throw (Exception. "non-collinear test failed"))))))

(defn main []
  (binding [main_a nil main_b nil main_c nil main_d nil main_e nil main_f nil] (do (test_are_collinear) (set! main_a {:x 4.802293498137402 :y 3.536233125455244 :z 0.0}) (set! main_b {:x (- 2.186788107953106) :y (- 9.24561398001649) :z 7.141509524846482}) (set! main_c {:x 1.530169574640268 :y (- 2.447927606600034) :z 3.343487096469054}) (println (str (are_collinear main_a main_b main_c 10))) (set! main_d {:x 2.399001826862445 :y (- 2.452009976680793) :z 4.464656666157666}) (set! main_e {:x (- 3.682816335934376) :y 5.753788986533145 :z 9.490993909044244}) (set! main_f {:x 1.962903518985307 :y 3.741415730125627 :z 7.0}) (println (str (are_collinear main_d main_e main_f 10))))))

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
