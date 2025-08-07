(ns main (:refer-clojure :exclude [change_contrast print_image]))

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

(declare change_contrast print_image)

(def ^:dynamic change_contrast_c nil)

(def ^:dynamic change_contrast_clamped nil)

(def ^:dynamic change_contrast_contrasted nil)

(def ^:dynamic change_contrast_factor nil)

(def ^:dynamic change_contrast_i nil)

(def ^:dynamic change_contrast_j nil)

(def ^:dynamic change_contrast_new_row nil)

(def ^:dynamic change_contrast_result nil)

(def ^:dynamic change_contrast_row nil)

(def ^:dynamic print_image_i nil)

(def ^:dynamic print_image_j nil)

(def ^:dynamic print_image_line nil)

(def ^:dynamic print_image_row nil)

(defn change_contrast [change_contrast_img change_contrast_level]
  (binding [change_contrast_c nil change_contrast_clamped nil change_contrast_contrasted nil change_contrast_factor nil change_contrast_i nil change_contrast_j nil change_contrast_new_row nil change_contrast_result nil change_contrast_row nil] (try (do (set! change_contrast_factor (quot (* 259.0 (+ (double change_contrast_level) 255.0)) (* 255.0 (- 259.0 (double change_contrast_level))))) (set! change_contrast_result []) (set! change_contrast_i 0) (while (< change_contrast_i (count change_contrast_img)) (do (set! change_contrast_row (nth change_contrast_img change_contrast_i)) (set! change_contrast_new_row []) (set! change_contrast_j 0) (while (< change_contrast_j (count change_contrast_row)) (do (set! change_contrast_c (nth change_contrast_row change_contrast_j)) (set! change_contrast_contrasted (long (+ 128.0 (* change_contrast_factor (- (double change_contrast_c) 128.0))))) (set! change_contrast_clamped (if (< change_contrast_contrasted 0) 0 (if (> change_contrast_contrasted 255) 255 change_contrast_contrasted))) (set! change_contrast_new_row (conj change_contrast_new_row change_contrast_clamped)) (set! change_contrast_j (+ change_contrast_j 1)))) (set! change_contrast_result (conj change_contrast_result change_contrast_new_row)) (set! change_contrast_i (+ change_contrast_i 1)))) (throw (ex-info "return" {:v change_contrast_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn print_image [print_image_img]
  (binding [print_image_i nil print_image_j nil print_image_line nil print_image_row nil] (do (set! print_image_i 0) (while (< print_image_i (count print_image_img)) (do (set! print_image_row (nth print_image_img print_image_i)) (set! print_image_j 0) (set! print_image_line "") (while (< print_image_j (count print_image_row)) (do (set! print_image_line (str (str print_image_line (str (nth print_image_row print_image_j))) " ")) (set! print_image_j (+ print_image_j 1)))) (println print_image_line) (set! print_image_i (+ print_image_i 1)))))))

(def ^:dynamic main_image [[100 125 150] [175 200 225] [50 75 100]])

(def ^:dynamic main_contrasted (change_contrast main_image 170))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println "Original image:")
      (print_image main_image)
      (println "After contrast:")
      (print_image main_contrasted)
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
