(ns main (:refer-clojure :exclude [focal_length_of_lens object_distance image_distance]))

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

(declare focal_length_of_lens object_distance image_distance)

(defn focal_length_of_lens [focal_length_of_lens_object_distance_from_lens focal_length_of_lens_image_distance_from_lens]
  (try (do (when (or (= focal_length_of_lens_object_distance_from_lens 0.0) (= focal_length_of_lens_image_distance_from_lens 0.0)) (throw (Exception. "Invalid inputs. Enter non zero values with respect to the sign convention."))) (throw (ex-info "return" {:v (/ 1.0 (- (/ 1.0 focal_length_of_lens_image_distance_from_lens) (/ 1.0 focal_length_of_lens_object_distance_from_lens)))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn object_distance [focal_length_of_lens_v object_distance_image_distance_from_lens]
  (try (do (when (or (= object_distance_image_distance_from_lens 0.0) (= focal_length_of_lens_v 0.0)) (throw (Exception. "Invalid inputs. Enter non zero values with respect to the sign convention."))) (throw (ex-info "return" {:v (/ 1.0 (- (/ 1.0 object_distance_image_distance_from_lens) (/ 1.0 focal_length_of_lens_v)))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn image_distance [focal_length_of_lens_v image_distance_object_distance_from_lens]
  (try (do (when (or (= image_distance_object_distance_from_lens 0.0) (= focal_length_of_lens_v 0.0)) (throw (Exception. "Invalid inputs. Enter non zero values with respect to the sign convention."))) (throw (ex-info "return" {:v (/ 1.0 (+ (/ 1.0 image_distance_object_distance_from_lens) (/ 1.0 focal_length_of_lens_v)))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (mochi_str (focal_length_of_lens 10.0 4.0)))
      (println (mochi_str (focal_length_of_lens 2.7 5.8)))
      (println (mochi_str (object_distance 10.0 40.0)))
      (println (mochi_str (object_distance 6.2 1.5)))
      (println (mochi_str (image_distance 50.0 40.0)))
      (println (mochi_str (image_distance 5.3 7.9)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
