(ns main (:refer-clojure :exclude [newBitmap setPx getPx splitLines splitWS parseIntStr tokenize readP3 toGrey pad writeP3]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare newBitmap setPx getPx splitLines splitWS parseIntStr tokenize readP3 toGrey pad writeP3)

(defn newBitmap [w h max]
  (try (do (def rows []) (def y 0) (while (< y h) (do (def row []) (def x 0) (while (< x w) (do (def row (conj row {:R 0 :G 0 :B 0})) (def x (+ x 1)))) (def rows (conj rows row)) (def y (+ y 1)))) (throw (ex-info "return" {:v {:w w :h h :max max :data rows}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn setPx [b x y p]
  (do (def rows (:data b)) (def row (nth rows y)) (def row (assoc row x p)) (def rows (assoc rows y row)) (def b (assoc b :data rows))))

(defn getPx [b x y]
  (try (throw (ex-info "return" {:v (nth (get (:data b) y) x)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn splitLines [s]
  (try (do (def out []) (def cur "") (def i 0) (while (< i (count s)) (do (def ch (substr s i (+ i 1))) (if (= ch "\n") (do (def out (conj out cur)) (def cur "")) (def cur (str cur ch))) (def i (+ i 1)))) (def out (conj out cur)) (throw (ex-info "return" {:v out}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn splitWS [s]
  (try (do (def out []) (def cur "") (def i 0) (while (< i (count s)) (do (def ch (substr s i (+ i 1))) (if (or (or (or (= ch " ") (= ch "\t")) (= ch "\r")) (= ch "\n")) (when (> (count cur) 0) (do (def out (conj out cur)) (def cur ""))) (def cur (str cur ch))) (def i (+ i 1)))) (when (> (count cur) 0) (def out (conj out cur))) (throw (ex-info "return" {:v out}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn parseIntStr [str]
  (try (do (def i 0) (def neg false) (when (and (> (count str) 0) (= (subs str 0 1) "-")) (do (def neg true) (def i 1))) (def n 0) (def digits {"0" 0 "1" 1 "2" 2 "3" 3 "4" 4 "5" 5 "6" 6 "7" 7 "8" 8 "9" 9}) (while (< i (count str)) (do (def n (+ (* n 10) (get digits (subs str i (+ i 1))))) (def i (+ i 1)))) (when neg (def n (- n))) (throw (ex-info "return" {:v n}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn tokenize [s]
  (try (do (def lines (splitLines s)) (def toks []) (def i 0) (loop [while_flag_1 true] (when (and while_flag_1 (< i (count lines))) (do (def line (nth lines i)) (cond (and (> (count line) 0) (= (substr line 0 1) "#")) (do (def i (+ i 1)) (recur true)) :else (do (def parts (splitWS line)) (def j 0) (while (< j (count parts)) (do (def toks (conj toks (nth parts j))) (def j (+ j 1)))) (def i (+ i 1)) (recur while_flag_1)))))) (throw (ex-info "return" {:v toks}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn readP3 [text]
  (try (do (def toks (tokenize text)) (when (< (count toks) 4) (throw (ex-info "return" {:v (newBitmap 0 0 0)}))) (when (not= (nth toks 0) "P3") (throw (ex-info "return" {:v (newBitmap 0 0 0)}))) (def w (parseIntStr (nth toks 1))) (def h (parseIntStr (nth toks 2))) (def maxv (parseIntStr (nth toks 3))) (def idx 4) (def bm (newBitmap w h maxv)) (def y (- h 1)) (while (>= y 0) (do (def x 0) (while (< x w) (do (def r (parseIntStr (nth toks idx))) (def g (parseIntStr (nth toks (+ idx 1)))) (def b (parseIntStr (nth toks (+ idx 2)))) (setPx bm x y {:R r :G g :B b}) (def idx (+ idx 3)) (def x (+ x 1)))) (def y (- y 1)))) (throw (ex-info "return" {:v bm}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn toGrey [b]
  (do (def h (:h b)) (def w (:w b)) (def m 0) (def y 0) (while (< y h) (do (def x 0) (while (< x w) (do (def p (getPx b x y)) (def l (/ (+ (+ (* (:R p) 2126) (* (:G p) 7152)) (* (:B p) 722)) 10000)) (when (> l (:max b)) (def l (:max b))) (setPx b x y {:R l :G l :B l}) (when (> l m) (def m l)) (def x (+ x 1)))) (def y (+ y 1)))) (def b (assoc b :max m))))

(defn pad [n w]
  (try (do (def s (str n)) (while (< (count s) w) (def s (str " " s))) (throw (ex-info "return" {:v s}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn writeP3 [b]
  (try (do (def h (:h b)) (def w (:w b)) (def max (:max b)) (def digits (count (str max))) (def out (str (str (str (str (str (str "P3\n# generated from Bitmap.writeppmp3\n" (str w)) " ") (str h)) "\n") (str max)) "\n")) (def y (- h 1)) (while (>= y 0) (do (def line "") (def x 0) (while (< x w) (do (def p (getPx b x y)) (def line (str (str (str (str (str (str line "   ") (pad (:R p) digits)) " ") (pad (:G p) digits)) " ") (pad (:B p) digits))) (def x (+ x 1)))) (def out (str (str out line) "\n")) (def y (- y 1)))) (throw (ex-info "return" {:v out}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn -main []
  (def ppmtxt (str (str (str (str (str (str (str "P3\n" "# feep.ppm\n") "4 4\n") "15\n") " 0  0  0    0  0  0    0  0  0   15  0 15\n") " 0  0  0    0 15  7    0  0  0    0  0  0\n") " 0  0  0    0  0  0    0 15  7    0  0  0\n") "15  0 15    0  0  0    0  0  0    0  0  0\n"))
  (println "Original Colour PPM file")
  (println ppmtxt)
  (def bm (readP3 ppmtxt))
  (println "Grey PPM:")
  (toGrey bm)
  (def out (writeP3 bm))
  (println out))

(-main)
