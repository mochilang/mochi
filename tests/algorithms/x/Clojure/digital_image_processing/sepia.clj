(ns main (:refer-clojure :exclude [normalize to_grayscale make_sepia]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(defn indexOf [s sub]
  (let [idx (clojure.string/index-of s sub)] (if (nil? idx) -1 idx)))

(defn split [s sep]
  (clojure.string/split s (re-pattern sep)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare normalize to_grayscale make_sepia)

(def ^:dynamic make_sepia_grey nil)

(def ^:dynamic make_sepia_i nil)

(def ^:dynamic make_sepia_img nil)

(def ^:dynamic make_sepia_j nil)

(def ^:dynamic make_sepia_pixel nil)

(def ^:dynamic make_sepia_pixel_h nil)

(def ^:dynamic make_sepia_pixel_v nil)

(def ^:dynamic to_grayscale_gs nil)

(defn normalize [normalize_value]
  (try (if (> normalize_value 255) 255 normalize_value) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn to_grayscale [to_grayscale_blue to_grayscale_green to_grayscale_red]
  (binding [to_grayscale_gs nil] (try (do (set! to_grayscale_gs (+ (+ (* 0.2126 (double to_grayscale_red)) (* 0.587 (double to_grayscale_green))) (* 0.114 (double to_grayscale_blue)))) (throw (ex-info "return" {:v (Integer/parseInt to_grayscale_gs)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn make_sepia [make_sepia_img_p make_sepia_factor]
  (binding [make_sepia_grey nil make_sepia_i nil make_sepia_img nil make_sepia_j nil make_sepia_pixel nil make_sepia_pixel_h nil make_sepia_pixel_v nil] (try (do (set! make_sepia_img make_sepia_img_p) (set! make_sepia_pixel_h (count make_sepia_img)) (set! make_sepia_pixel_v (count (nth make_sepia_img 0))) (set! make_sepia_i 0) (while (< make_sepia_i make_sepia_pixel_h) (do (set! make_sepia_j 0) (while (< make_sepia_j make_sepia_pixel_v) (do (set! make_sepia_pixel (nth (nth make_sepia_img make_sepia_i) make_sepia_j)) (set! make_sepia_grey (to_grayscale (nth make_sepia_pixel 0) (nth make_sepia_pixel 1) (nth make_sepia_pixel 2))) (set! make_sepia_img (assoc-in make_sepia_img [make_sepia_i make_sepia_j] [(normalize make_sepia_grey) (normalize (+ make_sepia_grey make_sepia_factor)) (normalize (+ make_sepia_grey (* 2 make_sepia_factor)))])) (set! make_sepia_j (+ make_sepia_j 1)))) (set! make_sepia_i (+ make_sepia_i 1)))) (throw (ex-info "return" {:v make_sepia_img}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_image [[[10 20 30] [40 50 60]] [[70 80 90] [200 150 100]]])

(def ^:dynamic main_sepia (make_sepia main_image 20))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str main_sepia))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
