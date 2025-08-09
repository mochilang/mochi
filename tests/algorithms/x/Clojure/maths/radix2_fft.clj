(ns main (:refer-clojure :exclude [c_add c_sub c_mul c_mul_scalar c_div_scalar sin_taylor cos_taylor exp_i make_complex_list fft floor pow10 round_to list_to_string multiply_poly]))

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

(declare c_add c_sub c_mul c_mul_scalar c_div_scalar sin_taylor cos_taylor exp_i make_complex_list fft floor pow10 round_to list_to_string multiply_poly)

(def ^:dynamic cos_taylor_i nil)

(def ^:dynamic cos_taylor_k1 nil)

(def ^:dynamic cos_taylor_k2 nil)

(def ^:dynamic cos_taylor_sum nil)

(def ^:dynamic cos_taylor_term nil)

(def ^:dynamic fft_a0 nil)

(def ^:dynamic fft_a1 nil)

(def ^:dynamic fft_angle nil)

(def ^:dynamic fft_even nil)

(def ^:dynamic fft_i nil)

(def ^:dynamic fft_n nil)

(def ^:dynamic fft_odd nil)

(def ^:dynamic fft_t nil)

(def ^:dynamic fft_u nil)

(def ^:dynamic fft_w nil)

(def ^:dynamic fft_wn nil)

(def ^:dynamic fft_y nil)

(def ^:dynamic fft_y0 nil)

(def ^:dynamic fft_y1 nil)

(def ^:dynamic floor_i nil)

(def ^:dynamic list_to_string_i nil)

(def ^:dynamic list_to_string_s nil)

(def ^:dynamic make_complex_list_arr nil)

(def ^:dynamic make_complex_list_i nil)

(def ^:dynamic multiply_poly_fa nil)

(def ^:dynamic multiply_poly_fb nil)

(def ^:dynamic multiply_poly_i nil)

(def ^:dynamic multiply_poly_n nil)

(def ^:dynamic multiply_poly_res nil)

(def ^:dynamic multiply_poly_val nil)

(def ^:dynamic pow10_i nil)

(def ^:dynamic pow10_p nil)

(def ^:dynamic round_to_m nil)

(def ^:dynamic sin_taylor_i nil)

(def ^:dynamic sin_taylor_k1 nil)

(def ^:dynamic sin_taylor_k2 nil)

(def ^:dynamic sin_taylor_sum nil)

(def ^:dynamic sin_taylor_term nil)

(defn c_add [c_add_a c_add_b]
  (try (throw (ex-info "return" {:v {:im (+ (:im c_add_a) (:im c_add_b)) :re (+ (:re c_add_a) (:re c_add_b))}})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn c_sub [c_sub_a c_sub_b]
  (try (throw (ex-info "return" {:v {:im (- (:im c_sub_a) (:im c_sub_b)) :re (- (:re c_sub_a) (:re c_sub_b))}})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn c_mul [c_mul_a c_mul_b]
  (try (throw (ex-info "return" {:v {:im (+ (* (:re c_mul_a) (:im c_mul_b)) (* (:im c_mul_a) (:re c_mul_b))) :re (- (* (:re c_mul_a) (:re c_mul_b)) (* (:im c_mul_a) (:im c_mul_b)))}})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn c_mul_scalar [c_mul_scalar_a c_mul_scalar_s]
  (try (throw (ex-info "return" {:v {:im (* (:im c_mul_scalar_a) c_mul_scalar_s) :re (* (:re c_mul_scalar_a) c_mul_scalar_s)}})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn c_div_scalar [c_div_scalar_a c_div_scalar_s]
  (try (throw (ex-info "return" {:v {:im (quot (:im c_div_scalar_a) c_div_scalar_s) :re (quot (:re c_div_scalar_a) c_div_scalar_s)}})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(def ^:dynamic main_PI 3.141592653589793)

(defn sin_taylor [sin_taylor_x]
  (binding [sin_taylor_i nil sin_taylor_k1 nil sin_taylor_k2 nil sin_taylor_sum nil sin_taylor_term nil] (try (do (set! sin_taylor_term sin_taylor_x) (set! sin_taylor_sum sin_taylor_x) (set! sin_taylor_i 1) (while (< sin_taylor_i 10) (do (set! sin_taylor_k1 (* 2.0 (double sin_taylor_i))) (set! sin_taylor_k2 (+ sin_taylor_k1 1.0)) (set! sin_taylor_term (quot (* (* (- sin_taylor_term) sin_taylor_x) sin_taylor_x) (* sin_taylor_k1 sin_taylor_k2))) (set! sin_taylor_sum (+ sin_taylor_sum sin_taylor_term)) (set! sin_taylor_i (+ sin_taylor_i 1)))) (throw (ex-info "return" {:v sin_taylor_sum}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn cos_taylor [cos_taylor_x]
  (binding [cos_taylor_i nil cos_taylor_k1 nil cos_taylor_k2 nil cos_taylor_sum nil cos_taylor_term nil] (try (do (set! cos_taylor_term 1.0) (set! cos_taylor_sum 1.0) (set! cos_taylor_i 1) (while (< cos_taylor_i 10) (do (set! cos_taylor_k1 (- (* 2.0 (double cos_taylor_i)) 1.0)) (set! cos_taylor_k2 (* 2.0 (double cos_taylor_i))) (set! cos_taylor_term (quot (* (* (- cos_taylor_term) cos_taylor_x) cos_taylor_x) (* cos_taylor_k1 cos_taylor_k2))) (set! cos_taylor_sum (+ cos_taylor_sum cos_taylor_term)) (set! cos_taylor_i (+ cos_taylor_i 1)))) (throw (ex-info "return" {:v cos_taylor_sum}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn exp_i [exp_i_theta]
  (try (throw (ex-info "return" {:v {:im (sin_taylor exp_i_theta) :re (cos_taylor exp_i_theta)}})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn make_complex_list [make_complex_list_n make_complex_list_value]
  (binding [make_complex_list_arr nil make_complex_list_i nil] (try (do (set! make_complex_list_arr []) (set! make_complex_list_i 0) (while (< make_complex_list_i make_complex_list_n) (do (set! make_complex_list_arr (conj make_complex_list_arr make_complex_list_value)) (set! make_complex_list_i (+ make_complex_list_i 1)))) (throw (ex-info "return" {:v make_complex_list_arr}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn fft [fft_a fft_invert]
  (binding [fft_a0 nil fft_a1 nil fft_angle nil fft_even nil fft_i nil fft_n nil fft_odd nil fft_t nil fft_u nil fft_w nil fft_wn nil fft_y nil fft_y0 nil fft_y1 nil] (try (do (set! fft_n (count fft_a)) (when (= fft_n 1) (throw (ex-info "return" {:v [(nth fft_a 0)]}))) (set! fft_a0 []) (set! fft_a1 []) (set! fft_i 0) (while (< fft_i (quot fft_n 2)) (do (set! fft_a0 (conj fft_a0 (nth fft_a (* 2 fft_i)))) (set! fft_a1 (conj fft_a1 (nth fft_a (+ (* 2 fft_i) 1)))) (set! fft_i (+ fft_i 1)))) (set! fft_y0 (fft fft_a0 fft_invert)) (set! fft_y1 (fft fft_a1 fft_invert)) (set! fft_angle (* (/ (* 2.0 main_PI) (double fft_n)) (if fft_invert (- 1.0) 1.0))) (set! fft_w {:im 0.0 :re 1.0}) (set! fft_wn (exp_i fft_angle)) (set! fft_y (make_complex_list fft_n {:im 0.0 :re 0.0})) (set! fft_i 0) (while (< fft_i (quot fft_n 2)) (do (set! fft_t (c_mul fft_w (nth fft_y1 fft_i))) (set! fft_u (nth fft_y0 fft_i)) (set! fft_even (c_add fft_u fft_t)) (set! fft_odd (c_sub fft_u fft_t)) (when fft_invert (do (set! fft_even (c_div_scalar fft_even 2.0)) (set! fft_odd (c_div_scalar fft_odd 2.0)))) (set! fft_y (assoc fft_y fft_i fft_even)) (set! fft_y (assoc fft_y (+ fft_i (quot fft_n 2)) fft_odd)) (set! fft_w (c_mul fft_w fft_wn)) (set! fft_i (+ fft_i 1)))) (throw (ex-info "return" {:v fft_y}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn floor [floor_x]
  (binding [floor_i nil] (try (do (set! floor_i (long floor_x)) (when (> (double floor_i) floor_x) (set! floor_i (- floor_i 1))) (throw (ex-info "return" {:v (double floor_i)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn pow10 [pow10_n]
  (binding [pow10_i nil pow10_p nil] (try (do (set! pow10_p 1.0) (set! pow10_i 0) (while (< pow10_i pow10_n) (do (set! pow10_p (* pow10_p 10.0)) (set! pow10_i (+ pow10_i 1)))) (throw (ex-info "return" {:v pow10_p}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn round_to [round_to_x round_to_ndigits]
  (binding [round_to_m nil] (try (do (set! round_to_m (pow10 round_to_ndigits)) (throw (ex-info "return" {:v (/ (floor (+ (* round_to_x round_to_m) 0.5)) round_to_m)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn list_to_string [list_to_string_l]
  (binding [list_to_string_i nil list_to_string_s nil] (try (do (set! list_to_string_s "[") (set! list_to_string_i 0) (while (< list_to_string_i (count list_to_string_l)) (do (set! list_to_string_s (str list_to_string_s (str (nth list_to_string_l list_to_string_i)))) (when (< (+ list_to_string_i 1) (count list_to_string_l)) (set! list_to_string_s (str list_to_string_s ", "))) (set! list_to_string_i (+ list_to_string_i 1)))) (set! list_to_string_s (str list_to_string_s "]")) (throw (ex-info "return" {:v list_to_string_s}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn multiply_poly [multiply_poly_a multiply_poly_b]
  (binding [multiply_poly_fa nil multiply_poly_fb nil multiply_poly_i nil multiply_poly_n nil multiply_poly_res nil multiply_poly_val nil] (try (do (set! multiply_poly_n 1) (while (< multiply_poly_n (- (+ (count multiply_poly_a) (count multiply_poly_b)) 1)) (set! multiply_poly_n (* multiply_poly_n 2))) (set! multiply_poly_fa (make_complex_list multiply_poly_n {:im 0.0 :re 0.0})) (set! multiply_poly_fb (make_complex_list multiply_poly_n {:im 0.0 :re 0.0})) (set! multiply_poly_i 0) (while (< multiply_poly_i (count multiply_poly_a)) (do (set! multiply_poly_fa (assoc multiply_poly_fa multiply_poly_i {:im 0.0 :re (nth multiply_poly_a multiply_poly_i)})) (set! multiply_poly_i (+ multiply_poly_i 1)))) (set! multiply_poly_i 0) (while (< multiply_poly_i (count multiply_poly_b)) (do (set! multiply_poly_fb (assoc multiply_poly_fb multiply_poly_i {:im 0.0 :re (nth multiply_poly_b multiply_poly_i)})) (set! multiply_poly_i (+ multiply_poly_i 1)))) (set! multiply_poly_fa (fft multiply_poly_fa false)) (set! multiply_poly_fb (fft multiply_poly_fb false)) (set! multiply_poly_i 0) (while (< multiply_poly_i multiply_poly_n) (do (set! multiply_poly_fa (assoc multiply_poly_fa multiply_poly_i (c_mul (get multiply_poly_fa multiply_poly_i) (get multiply_poly_fb multiply_poly_i)))) (set! multiply_poly_i (+ multiply_poly_i 1)))) (set! multiply_poly_fa (fft multiply_poly_fa true)) (set! multiply_poly_res []) (set! multiply_poly_i 0) (while (< multiply_poly_i (- (+ (count multiply_poly_a) (count multiply_poly_b)) 1)) (do (set! multiply_poly_val (get multiply_poly_fa multiply_poly_i)) (set! multiply_poly_res (conj multiply_poly_res (round_to (:re multiply_poly_val) 8))) (set! multiply_poly_i (+ multiply_poly_i 1)))) (while (and (> (count multiply_poly_res) 0) (= (nth multiply_poly_res (- (count multiply_poly_res) 1)) 0.0)) (set! multiply_poly_res (subvec multiply_poly_res 0 (- (count multiply_poly_res) 1)))) (throw (ex-info "return" {:v multiply_poly_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_A [0.0 1.0 0.0 2.0])

(def ^:dynamic main_B [2.0 3.0 4.0 0.0])

(def ^:dynamic main_product (multiply_poly main_A main_B))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (list_to_string main_product))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
