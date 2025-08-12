(ns main (:refer-clojure :exclude [abs_float isclose focal_length object_distance image_distance test_focal_length test_object_distance test_image_distance main]))

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

(declare abs_float isclose focal_length object_distance image_distance test_focal_length test_object_distance test_image_distance main)

(def ^:dynamic test_focal_length_f1 nil)

(def ^:dynamic test_focal_length_f2 nil)

(def ^:dynamic test_image_distance_v1 nil)

(def ^:dynamic test_image_distance_v2 nil)

(def ^:dynamic test_object_distance_u1 nil)

(def ^:dynamic test_object_distance_u2 nil)

(defn abs_float [abs_float_x]
  (try (if (< abs_float_x 0.0) (- abs_float_x) abs_float_x) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn isclose [isclose_a isclose_b isclose_tolerance]
  (try (throw (ex-info "return" {:v (< (abs_float (- isclose_a isclose_b)) isclose_tolerance)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn focal_length [focal_length_distance_of_object focal_length_distance_of_image]
  (try (do (when (or (= focal_length_distance_of_object 0.0) (= focal_length_distance_of_image 0.0)) (throw (Exception. "Invalid inputs. Enter non zero values with respect to the sign convention."))) (throw (ex-info "return" {:v (/ 1.0 (+ (/ 1.0 focal_length_distance_of_object) (/ 1.0 focal_length_distance_of_image)))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn object_distance [focal_length_v object_distance_distance_of_image]
  (try (do (when (or (= object_distance_distance_of_image 0.0) (= focal_length_v 0.0)) (throw (Exception. "Invalid inputs. Enter non zero values with respect to the sign convention."))) (throw (ex-info "return" {:v (/ 1.0 (- (/ 1.0 focal_length_v) (/ 1.0 object_distance_distance_of_image)))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn image_distance [focal_length_v image_distance_distance_of_object]
  (try (do (when (or (= image_distance_distance_of_object 0.0) (= focal_length_v 0.0)) (throw (Exception. "Invalid inputs. Enter non zero values with respect to the sign convention."))) (throw (ex-info "return" {:v (/ 1.0 (- (/ 1.0 focal_length_v) (/ 1.0 image_distance_distance_of_object)))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn test_focal_length []
  (binding [test_focal_length_f1 nil test_focal_length_f2 nil] (do (set! test_focal_length_f1 (focal_length 10.0 20.0)) (when (not (isclose test_focal_length_f1 6.66666666666666 0.00000001)) (throw (Exception. "focal_length test1 failed"))) (set! test_focal_length_f2 (focal_length 9.5 6.7)) (when (not (isclose test_focal_length_f2 3.929012346 0.00000001)) (throw (Exception. "focal_length test2 failed"))))))

(defn test_object_distance []
  (binding [test_object_distance_u1 nil test_object_distance_u2 nil] (do (set! test_object_distance_u1 (object_distance 30.0 20.0)) (when (not (isclose test_object_distance_u1 (- 60.0) 0.00000001)) (throw (Exception. "object_distance test1 failed"))) (set! test_object_distance_u2 (object_distance 10.5 11.7)) (when (not (isclose test_object_distance_u2 102.375 0.00000001)) (throw (Exception. "object_distance test2 failed"))))))

(defn test_image_distance []
  (binding [test_image_distance_v1 nil test_image_distance_v2 nil] (do (set! test_image_distance_v1 (image_distance 10.0 40.0)) (when (not (isclose test_image_distance_v1 13.33333333 0.00000001)) (throw (Exception. "image_distance test1 failed"))) (set! test_image_distance_v2 (image_distance 1.5 6.7)) (when (not (isclose test_image_distance_v2 1.932692308 0.00000001)) (throw (Exception. "image_distance test2 failed"))))))

(defn main []
  (do (test_focal_length) (test_object_distance) (test_image_distance) (println (str (focal_length 10.0 20.0))) (println (str (object_distance 30.0 20.0))) (println (str (image_distance 10.0 40.0)))))

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
