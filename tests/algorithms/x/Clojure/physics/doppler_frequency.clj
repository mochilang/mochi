(ns main (:refer-clojure :exclude [doppler_effect absf almost_equal test_doppler_effect main]))

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

(declare doppler_effect absf almost_equal test_doppler_effect main)

(def ^:dynamic doppler_effect_doppler_freq nil)

(defn doppler_effect [doppler_effect_org_freq doppler_effect_wave_vel doppler_effect_obs_vel doppler_effect_src_vel]
  (binding [doppler_effect_doppler_freq nil] (try (do (when (= doppler_effect_wave_vel doppler_effect_src_vel) (throw (Exception. "division by zero implies vs=v and observer in front of the source"))) (set! doppler_effect_doppler_freq (/ (* doppler_effect_org_freq (+ doppler_effect_wave_vel doppler_effect_obs_vel)) (- doppler_effect_wave_vel doppler_effect_src_vel))) (when (<= doppler_effect_doppler_freq 0.0) (throw (Exception. "non-positive frequency implies vs>v or v0>v (in the opposite direction)"))) (throw (ex-info "return" {:v doppler_effect_doppler_freq}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn absf [absf_x]
  (try (if (< absf_x 0.0) (- absf_x) absf_x) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn almost_equal [almost_equal_a almost_equal_b almost_equal_tol]
  (try (throw (ex-info "return" {:v (<= (absf (- almost_equal_a almost_equal_b)) almost_equal_tol)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn test_doppler_effect []
  (do (when (not (almost_equal (doppler_effect 100.0 330.0 10.0 0.0) 103.03030303030303 0.0000001)) (throw (Exception. "test 1 failed"))) (when (not (almost_equal (doppler_effect 100.0 330.0 (- 10.0) 0.0) 96.96969696969697 0.0000001)) (throw (Exception. "test 2 failed"))) (when (not (almost_equal (doppler_effect 100.0 330.0 0.0 10.0) 103.125 0.0000001)) (throw (Exception. "test 3 failed"))) (when (not (almost_equal (doppler_effect 100.0 330.0 0.0 (- 10.0)) 97.05882352941177 0.0000001)) (throw (Exception. "test 4 failed"))) (when (not (almost_equal (doppler_effect 100.0 330.0 10.0 10.0) 106.25 0.0000001)) (throw (Exception. "test 5 failed"))) (when (not (almost_equal (doppler_effect 100.0 330.0 (- 10.0) (- 10.0)) 94.11764705882354 0.0000001)) (throw (Exception. "test 6 failed")))))

(defn main []
  (do (test_doppler_effect) (println (doppler_effect 100.0 330.0 10.0 0.0))))

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
