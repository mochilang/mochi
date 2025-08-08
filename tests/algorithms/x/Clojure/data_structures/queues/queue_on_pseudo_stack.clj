(ns main (:refer-clojure :exclude [empty_queue put drop_first drop_last rotate get front size to_string main]))

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

(declare empty_queue put drop_first drop_last rotate get front size to_string main)

(def ^:dynamic drop_first_i nil)

(def ^:dynamic drop_first_res nil)

(def ^:dynamic drop_last_i nil)

(def ^:dynamic drop_last_res nil)

(def ^:dynamic front_q2 nil)

(def ^:dynamic front_r nil)

(def ^:dynamic get_q1 nil)

(def ^:dynamic get_q2 nil)

(def ^:dynamic get_s nil)

(def ^:dynamic get_v nil)

(def ^:dynamic main_f nil)

(def ^:dynamic main_g nil)

(def ^:dynamic main_q nil)

(def ^:dynamic put_s nil)

(def ^:dynamic rotate_i nil)

(def ^:dynamic rotate_s nil)

(def ^:dynamic rotate_temp nil)

(def ^:dynamic to_string_i nil)

(def ^:dynamic to_string_s nil)

(defn empty_queue []
  (try (throw (ex-info "return" {:v {:length 0 :stack []}})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn put [put_q put_item]
  (binding [put_s nil] (try (do (set! put_s (conj (:stack put_q) put_item)) (throw (ex-info "return" {:v {:length (+ (:length put_q) 1) :stack put_s}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn drop_first [drop_first_xs]
  (binding [drop_first_i nil drop_first_res nil] (try (do (set! drop_first_res []) (set! drop_first_i 1) (while (< drop_first_i (count drop_first_xs)) (do (set! drop_first_res (conj drop_first_res (nth drop_first_xs drop_first_i))) (set! drop_first_i (+ drop_first_i 1)))) (throw (ex-info "return" {:v drop_first_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn drop_last [drop_last_xs]
  (binding [drop_last_i nil drop_last_res nil] (try (do (set! drop_last_res []) (set! drop_last_i 0) (while (< drop_last_i (- (count drop_last_xs) 1)) (do (set! drop_last_res (conj drop_last_res (nth drop_last_xs drop_last_i))) (set! drop_last_i (+ drop_last_i 1)))) (throw (ex-info "return" {:v drop_last_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn rotate [rotate_q rotate_rotation]
  (binding [rotate_i nil rotate_s nil rotate_temp nil] (try (do (set! rotate_s (:stack rotate_q)) (set! rotate_i 0) (while (and (< rotate_i rotate_rotation) (> (count rotate_s) 0)) (do (set! rotate_temp (nth rotate_s 0)) (set! rotate_s (drop_first rotate_s)) (set! rotate_s (conj rotate_s rotate_temp)) (set! rotate_i (+ rotate_i 1)))) (throw (ex-info "return" {:v {:length (:length rotate_q) :stack rotate_s}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn get [get_q]
  (binding [get_q1 nil get_q2 nil get_s nil get_v nil] (try (do (when (= (:length get_q) 0) (throw (Exception. "queue empty"))) (set! get_q1 (rotate get_q 1)) (set! get_v (get (:stack get_q1) (- (:length get_q1) 1))) (set! get_s (drop_last (:stack get_q1))) (set! get_q2 {:length (:length get_q1) :stack get_s}) (set! get_q2 (rotate get_q2 (- (:length get_q2) 1))) (set! get_q2 {:length (- (:length get_q2) 1) :stack (:stack get_q2)}) (throw (ex-info "return" {:v {:queue get_q2 :value get_v}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn front [front_q]
  (binding [front_q2 nil front_r nil] (try (do (set! front_r (get front_q)) (set! front_q2 (put (:queue front_r) (:value front_r))) (set! front_q2 (rotate front_q2 (- (:length front_q2) 1))) (throw (ex-info "return" {:v {:queue front_q2 :value (:value front_r)}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn size [size_q]
  (try (throw (ex-info "return" {:v (:length size_q)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn to_string [to_string_q]
  (binding [to_string_i nil to_string_s nil] (try (do (set! to_string_s "<") (when (> (:length to_string_q) 0) (do (set! to_string_s (str to_string_s (str (get (:stack to_string_q) 0)))) (set! to_string_i 1) (while (< to_string_i (:length to_string_q)) (do (set! to_string_s (str (str to_string_s ", ") (str (get (:stack to_string_q) to_string_i)))) (set! to_string_i (+ to_string_i 1)))))) (set! to_string_s (str to_string_s ">")) (throw (ex-info "return" {:v to_string_s}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_f nil main_g nil main_q nil] (do (set! main_q (empty_queue)) (set! main_q (put main_q 1)) (set! main_q (put main_q 2)) (set! main_q (put main_q 3)) (println (to_string main_q)) (set! main_g (get main_q)) (set! main_q (:queue main_g)) (println (:value main_g)) (println (to_string main_q)) (set! main_f (front main_q)) (set! main_q (:queue main_f)) (println (:value main_f)) (println (to_string main_q)) (println (size main_q)))))

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
