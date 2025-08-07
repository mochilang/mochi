(ns main (:refer-clojure :exclude [get_greyscale zeros burkes_dither main]))

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

(declare get_greyscale zeros burkes_dither main)

(def ^:dynamic burkes_dither_current_error nil)

(def ^:dynamic burkes_dither_error_table nil)

(def ^:dynamic burkes_dither_grey nil)

(def ^:dynamic burkes_dither_height nil)

(def ^:dynamic burkes_dither_new_val nil)

(def ^:dynamic burkes_dither_output nil)

(def ^:dynamic burkes_dither_px nil)

(def ^:dynamic burkes_dither_row nil)

(def ^:dynamic burkes_dither_total nil)

(def ^:dynamic burkes_dither_width nil)

(def ^:dynamic burkes_dither_x nil)

(def ^:dynamic burkes_dither_y nil)

(def ^:dynamic get_greyscale_b nil)

(def ^:dynamic get_greyscale_g nil)

(def ^:dynamic get_greyscale_r nil)

(def ^:dynamic main_img nil)

(def ^:dynamic main_line nil)

(def ^:dynamic main_result nil)

(def ^:dynamic main_x nil)

(def ^:dynamic main_y nil)

(def ^:dynamic zeros_i nil)

(def ^:dynamic zeros_j nil)

(def ^:dynamic zeros_row nil)

(def ^:dynamic zeros_table nil)

(defn get_greyscale [get_greyscale_blue get_greyscale_green get_greyscale_red]
  (binding [get_greyscale_b nil get_greyscale_g nil get_greyscale_r nil] (try (do (set! get_greyscale_b (double get_greyscale_blue)) (set! get_greyscale_g (double get_greyscale_green)) (set! get_greyscale_r (double get_greyscale_red)) (throw (ex-info "return" {:v (long (+ (+ (* 0.114 get_greyscale_b) (* 0.587 get_greyscale_g)) (* 0.299 get_greyscale_r)))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn zeros [zeros_h zeros_w]
  (binding [zeros_i nil zeros_j nil zeros_row nil zeros_table nil] (try (do (set! zeros_table []) (set! zeros_i 0) (while (< zeros_i zeros_h) (do (set! zeros_row []) (set! zeros_j 0) (while (< zeros_j zeros_w) (do (set! zeros_row (conj zeros_row 0)) (set! zeros_j (+ zeros_j 1)))) (set! zeros_table (conj zeros_table zeros_row)) (set! zeros_i (+ zeros_i 1)))) (throw (ex-info "return" {:v zeros_table}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn burkes_dither [burkes_dither_img burkes_dither_threshold]
  (binding [burkes_dither_current_error nil burkes_dither_error_table nil burkes_dither_grey nil burkes_dither_height nil burkes_dither_new_val nil burkes_dither_output nil burkes_dither_px nil burkes_dither_row nil burkes_dither_total nil burkes_dither_width nil burkes_dither_x nil burkes_dither_y nil] (try (do (set! burkes_dither_height (count burkes_dither_img)) (set! burkes_dither_width (count (nth burkes_dither_img 0))) (set! burkes_dither_error_table (zeros (+ burkes_dither_height 1) (+ burkes_dither_width 4))) (set! burkes_dither_output []) (set! burkes_dither_y 0) (while (< burkes_dither_y burkes_dither_height) (do (set! burkes_dither_row []) (set! burkes_dither_x 0) (while (< burkes_dither_x burkes_dither_width) (do (set! burkes_dither_px (nth (nth burkes_dither_img burkes_dither_y) burkes_dither_x)) (set! burkes_dither_grey (get_greyscale (nth burkes_dither_px 0) (nth burkes_dither_px 1) (nth burkes_dither_px 2))) (set! burkes_dither_total (+ burkes_dither_grey (nth (nth burkes_dither_error_table burkes_dither_y) (+ burkes_dither_x 2)))) (set! burkes_dither_new_val 0) (set! burkes_dither_current_error 0) (if (> burkes_dither_threshold burkes_dither_total) (do (set! burkes_dither_new_val 0) (set! burkes_dither_current_error burkes_dither_total)) (do (set! burkes_dither_new_val 255) (set! burkes_dither_current_error (- burkes_dither_total 255)))) (set! burkes_dither_row (conj burkes_dither_row burkes_dither_new_val)) (set! burkes_dither_error_table (assoc-in burkes_dither_error_table [burkes_dither_y (+ burkes_dither_x 3)] (+ (nth (nth burkes_dither_error_table burkes_dither_y) (+ burkes_dither_x 3)) (quot (* 8 burkes_dither_current_error) 32)))) (set! burkes_dither_error_table (assoc-in burkes_dither_error_table [burkes_dither_y (+ burkes_dither_x 4)] (+ (nth (nth burkes_dither_error_table burkes_dither_y) (+ burkes_dither_x 4)) (quot (* 4 burkes_dither_current_error) 32)))) (set! burkes_dither_error_table (assoc-in burkes_dither_error_table [(+ burkes_dither_y 1) (+ burkes_dither_x 2)] (+ (nth (nth burkes_dither_error_table (+ burkes_dither_y 1)) (+ burkes_dither_x 2)) (quot (* 8 burkes_dither_current_error) 32)))) (set! burkes_dither_error_table (assoc-in burkes_dither_error_table [(+ burkes_dither_y 1) (+ burkes_dither_x 3)] (+ (nth (nth burkes_dither_error_table (+ burkes_dither_y 1)) (+ burkes_dither_x 3)) (quot (* 4 burkes_dither_current_error) 32)))) (set! burkes_dither_error_table (assoc-in burkes_dither_error_table [(+ burkes_dither_y 1) (+ burkes_dither_x 4)] (+ (nth (nth burkes_dither_error_table (+ burkes_dither_y 1)) (+ burkes_dither_x 4)) (quot (* 2 burkes_dither_current_error) 32)))) (set! burkes_dither_error_table (assoc-in burkes_dither_error_table [(+ burkes_dither_y 1) (+ burkes_dither_x 1)] (+ (nth (nth burkes_dither_error_table (+ burkes_dither_y 1)) (+ burkes_dither_x 1)) (quot (* 4 burkes_dither_current_error) 32)))) (set! burkes_dither_error_table (assoc-in burkes_dither_error_table [(+ burkes_dither_y 1) burkes_dither_x] (+ (nth (nth burkes_dither_error_table (+ burkes_dither_y 1)) burkes_dither_x) (quot (* 2 burkes_dither_current_error) 32)))) (set! burkes_dither_x (+ burkes_dither_x 1)))) (set! burkes_dither_output (conj burkes_dither_output burkes_dither_row)) (set! burkes_dither_y (+ burkes_dither_y 1)))) (throw (ex-info "return" {:v burkes_dither_output}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_img nil main_line nil main_result nil main_x nil main_y nil] (do (set! main_img [[[0 0 0] [64 64 64] [128 128 128] [192 192 192]] [[255 255 255] [200 200 200] [150 150 150] [100 100 100]] [[30 144 255] [255 0 0] [0 255 0] [0 0 255]] [[50 100 150] [80 160 240] [70 140 210] [60 120 180]]]) (set! main_result (burkes_dither main_img 128)) (set! main_y 0) (while (< main_y (count main_result)) (do (set! main_line "") (set! main_x 0) (while (< main_x (count (nth main_result main_y))) (do (set! main_line (str main_line (str (nth (nth main_result main_y) main_x)))) (when (< main_x (- (count (nth main_result main_y)) 1)) (set! main_line (str main_line " "))) (set! main_x (+ main_x 1)))) (println main_line) (set! main_y (+ main_y 1)))))))

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
