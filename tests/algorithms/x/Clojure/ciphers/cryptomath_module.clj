(ns main (:refer-clojure :exclude [gcd find_mod_inverse]))

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

(declare gcd find_mod_inverse)

(def ^:dynamic find_mod_inverse_q nil)

(def ^:dynamic find_mod_inverse_res nil)

(def ^:dynamic find_mod_inverse_t1 nil)

(def ^:dynamic find_mod_inverse_t2 nil)

(def ^:dynamic find_mod_inverse_t3 nil)

(def ^:dynamic find_mod_inverse_u1 nil)

(def ^:dynamic find_mod_inverse_u2 nil)

(def ^:dynamic find_mod_inverse_u3 nil)

(def ^:dynamic find_mod_inverse_v1 nil)

(def ^:dynamic find_mod_inverse_v2 nil)

(def ^:dynamic find_mod_inverse_v3 nil)

(def ^:dynamic gcd_t nil)

(def ^:dynamic gcd_x nil)

(def ^:dynamic gcd_y nil)

(defn gcd [gcd_a gcd_b]
  (binding [gcd_t nil gcd_x nil gcd_y nil] (try (do (set! gcd_x (if (< gcd_a 0) (- gcd_a) gcd_a)) (set! gcd_y (if (< gcd_b 0) (- gcd_b) gcd_b)) (while (not= gcd_y 0) (do (set! gcd_t (mod gcd_x gcd_y)) (set! gcd_x gcd_y) (set! gcd_y gcd_t))) (throw (ex-info "return" {:v gcd_x}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn find_mod_inverse [find_mod_inverse_a find_mod_inverse_m]
  (binding [find_mod_inverse_q nil find_mod_inverse_res nil find_mod_inverse_t1 nil find_mod_inverse_t2 nil find_mod_inverse_t3 nil find_mod_inverse_u1 nil find_mod_inverse_u2 nil find_mod_inverse_u3 nil find_mod_inverse_v1 nil find_mod_inverse_v2 nil find_mod_inverse_v3 nil] (try (do (when (not= (gcd find_mod_inverse_a find_mod_inverse_m) 1) (error (str (str (str (str "mod inverse of " (str find_mod_inverse_a)) " and ") (str find_mod_inverse_m)) " does not exist"))) (set! find_mod_inverse_u1 1) (set! find_mod_inverse_u2 0) (set! find_mod_inverse_u3 find_mod_inverse_a) (set! find_mod_inverse_v1 0) (set! find_mod_inverse_v2 1) (set! find_mod_inverse_v3 find_mod_inverse_m) (while (not= find_mod_inverse_v3 0) (do (set! find_mod_inverse_q (/ find_mod_inverse_u3 find_mod_inverse_v3)) (set! find_mod_inverse_t1 (- find_mod_inverse_u1 (* find_mod_inverse_q find_mod_inverse_v1))) (set! find_mod_inverse_t2 (- find_mod_inverse_u2 (* find_mod_inverse_q find_mod_inverse_v2))) (set! find_mod_inverse_t3 (- find_mod_inverse_u3 (* find_mod_inverse_q find_mod_inverse_v3))) (set! find_mod_inverse_u1 find_mod_inverse_v1) (set! find_mod_inverse_u2 find_mod_inverse_v2) (set! find_mod_inverse_u3 find_mod_inverse_v3) (set! find_mod_inverse_v1 find_mod_inverse_t1) (set! find_mod_inverse_v2 find_mod_inverse_t2) (set! find_mod_inverse_v3 find_mod_inverse_t3))) (set! find_mod_inverse_res (mod find_mod_inverse_u1 find_mod_inverse_m)) (when (< find_mod_inverse_res 0) (set! find_mod_inverse_res (+ find_mod_inverse_res find_mod_inverse_m))) (throw (ex-info "return" {:v find_mod_inverse_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (find_mod_inverse 3 11)))
      (println (str (find_mod_inverse 7 26)))
      (println (str (find_mod_inverse 11 26)))
      (println (str (find_mod_inverse 17 43)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
