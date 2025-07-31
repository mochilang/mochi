(ns main (:refer-clojure :exclude [newBitmap setPixel fillRect pad writePPMP3 main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare newBitmap setPixel fillRect pad writePPMP3 main)

(defn newBitmap [w h c]
  (try (do (def rows []) (def y 0) (while (< y h) (do (def row []) (def x 0) (while (< x w) (do (def row (conj row c)) (def x (+' x 1)))) (def rows (conj rows row)) (def y (+' y 1)))) (throw (ex-info "return" {:v {:width w :height h :pixels rows}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn setPixel [b_p x y c]
  (do (def b b_p) (def rows (:pixels b)) (def row (nth rows y)) (def row (assoc row x c)) (def rows (assoc rows y row)) (def b (assoc b :pixels rows))))

(defn fillRect [b x y w h c]
  (do (def yy y) (while (< yy (+' y h)) (do (def xx x) (while (< xx (+' x w)) (do (setPixel b xx yy c) (def xx (+' xx 1)))) (def yy (+' yy 1))))))

(defn pad [n width]
  (try (do (def s (str n)) (while (< (count s) width) (def s (str " " s))) (throw (ex-info "return" {:v s}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn writePPMP3 [b]
  (try (do (def maxv 0) (def y 0) (while (< y (:height b)) (do (def x 0) (while (< x (:width b)) (do (def p (nth (get (:pixels b) y) x)) (when (> (:R p) maxv) (def maxv (:R p))) (when (> (:G p) maxv) (def maxv (:G p))) (when (> (:B p) maxv) (def maxv (:B p))) (def x (+' x 1)))) (def y (+' y 1)))) (def out (str (str (str (str (str (str "P3\n# generated from Bitmap.writeppmp3\n" (str (:width b))) " ") (str (:height b))) "\n") (str maxv)) "\n")) (def numsize (count (str maxv))) (def y (- (:height b) 1)) (while (>= y 0) (do (def line "") (def x 0) (while (< x (:width b)) (do (def p (nth (get (:pixels b) y) x)) (def line (str (str (str (str (str (str line "   ") (pad (:R p) numsize)) " ") (pad (:G p) numsize)) " ") (pad (:B p) numsize))) (def x (+' x 1)))) (def out (str out line)) (if (> y 0) (def out (str out "\n")) (def out (str out "\n"))) (def y (- y 1)))) (throw (ex-info "return" {:v out}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (do (def black {:R 0 :G 0 :B 0}) (def white {:R 255 :G 255 :B 255}) (def bm (newBitmap 4 4 black)) (fillRect bm 1 0 1 2 white) (setPixel bm 3 3 {:R 127 :G 0 :B 63}) (def ppm (writePPMP3 bm)) (println ppm)))

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
