(ns main (:refer-clojure :exclude [lcg_rand roll round2 throw_dice main]))

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

(declare lcg_rand roll round2 throw_dice main)

(def ^:dynamic main_result nil)

(def ^:dynamic roll_r nil)

(def ^:dynamic roll_rv nil)

(def ^:dynamic round2_y nil)

(def ^:dynamic round2_z nil)

(def ^:dynamic throw_dice_count_of_sum nil)

(def ^:dynamic throw_dice_d nil)

(def ^:dynamic throw_dice_i nil)

(def ^:dynamic throw_dice_max_sum nil)

(def ^:dynamic throw_dice_p nil)

(def ^:dynamic throw_dice_probability nil)

(def ^:dynamic throw_dice_s nil)

(def ^:dynamic throw_dice_t nil)

(def ^:dynamic main_lcg_seed 1)

(defn lcg_rand []
  (try (do (alter-var-root (var main_lcg_seed) (fn [_] (mod (+ (* main_lcg_seed 1103515245) 12345) 2147483648))) (throw (ex-info "return" {:v main_lcg_seed}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn roll []
  (binding [roll_r nil roll_rv nil] (try (do (set! roll_rv (double (lcg_rand))) (set! roll_r (/ (* roll_rv 6.0) 2147483648.0)) (throw (ex-info "return" {:v (+ 1 (long roll_r))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn round2 [round2_x]
  (binding [round2_y nil round2_z nil] (try (do (set! round2_y (+ (* round2_x 100.0) 0.5)) (set! round2_z (long round2_y)) (throw (ex-info "return" {:v (/ (double round2_z) 100.0)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn throw_dice [throw_dice_num_throws throw_dice_num_dice]
  (binding [throw_dice_count_of_sum nil throw_dice_d nil throw_dice_i nil throw_dice_max_sum nil throw_dice_p nil throw_dice_probability nil throw_dice_s nil throw_dice_t nil] (try (do (set! throw_dice_count_of_sum []) (set! throw_dice_max_sum (+ (* throw_dice_num_dice 6) 1)) (set! throw_dice_i 0) (while (< throw_dice_i throw_dice_max_sum) (do (set! throw_dice_count_of_sum (conj throw_dice_count_of_sum 0)) (set! throw_dice_i (+ throw_dice_i 1)))) (set! throw_dice_t 0) (while (< throw_dice_t throw_dice_num_throws) (do (set! throw_dice_s 0) (set! throw_dice_d 0) (while (< throw_dice_d throw_dice_num_dice) (do (set! throw_dice_s (+ throw_dice_s (roll))) (set! throw_dice_d (+ throw_dice_d 1)))) (set! throw_dice_count_of_sum (assoc throw_dice_count_of_sum throw_dice_s (+ (nth throw_dice_count_of_sum throw_dice_s) 1))) (set! throw_dice_t (+ throw_dice_t 1)))) (set! throw_dice_probability []) (set! throw_dice_i throw_dice_num_dice) (while (< throw_dice_i throw_dice_max_sum) (do (set! throw_dice_p (/ (* (double (nth throw_dice_count_of_sum throw_dice_i)) 100.0) (double throw_dice_num_throws))) (set! throw_dice_probability (conj throw_dice_probability (round2 throw_dice_p))) (set! throw_dice_i (+ throw_dice_i 1)))) (throw (ex-info "return" {:v throw_dice_probability}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_result nil] (do (alter-var-root (var main_lcg_seed) (fn [_] 1)) (set! main_result (throw_dice 10000 2)) (println (str main_result)))))

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
