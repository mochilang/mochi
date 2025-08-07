(ns main (:refer-clojure :exclude [absf fmod roundf maxf minf hsv_to_rgb rgb_to_hsv approximately_equal_hsv]))

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

(declare absf fmod roundf maxf minf hsv_to_rgb rgb_to_hsv approximately_equal_hsv)

(def ^:dynamic approximately_equal_hsv_check_hue nil)

(def ^:dynamic approximately_equal_hsv_check_saturation nil)

(def ^:dynamic approximately_equal_hsv_check_value nil)

(def ^:dynamic hsv_to_rgb_blue nil)

(def ^:dynamic hsv_to_rgb_chroma nil)

(def ^:dynamic hsv_to_rgb_green nil)

(def ^:dynamic hsv_to_rgb_hue_section nil)

(def ^:dynamic hsv_to_rgb_match_value nil)

(def ^:dynamic hsv_to_rgb_red nil)

(def ^:dynamic hsv_to_rgb_second_largest_component nil)

(def ^:dynamic maxf_m nil)

(def ^:dynamic minf_m nil)

(def ^:dynamic rgb_to_hsv_chroma nil)

(def ^:dynamic rgb_to_hsv_float_blue nil)

(def ^:dynamic rgb_to_hsv_float_green nil)

(def ^:dynamic rgb_to_hsv_float_red nil)

(def ^:dynamic rgb_to_hsv_hue nil)

(def ^:dynamic rgb_to_hsv_min_val nil)

(def ^:dynamic rgb_to_hsv_saturation nil)

(def ^:dynamic rgb_to_hsv_value nil)

(defn absf [absf_x]
  (try (if (< absf_x 0.0) (- absf_x) absf_x) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn fmod [fmod_a fmod_b]
  (try (throw (ex-info "return" {:v (- fmod_a (* fmod_b (Integer/parseInt (/ fmod_a fmod_b))))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn roundf [roundf_x]
  (try (if (>= roundf_x 0.0) (Integer/parseInt (+ roundf_x 0.5)) (Integer/parseInt (- roundf_x 0.5))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn maxf [maxf_a maxf_b maxf_c]
  (binding [maxf_m nil] (try (do (set! maxf_m maxf_a) (when (> maxf_b maxf_m) (set! maxf_m maxf_b)) (when (> maxf_c maxf_m) (set! maxf_m maxf_c)) (throw (ex-info "return" {:v maxf_m}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn minf [minf_a minf_b minf_c]
  (binding [minf_m nil] (try (do (set! minf_m minf_a) (when (< minf_b minf_m) (set! minf_m minf_b)) (when (< minf_c minf_m) (set! minf_m minf_c)) (throw (ex-info "return" {:v minf_m}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn hsv_to_rgb [hsv_to_rgb_hue hsv_to_rgb_saturation hsv_to_rgb_value]
  (binding [hsv_to_rgb_blue nil hsv_to_rgb_chroma nil hsv_to_rgb_green nil hsv_to_rgb_hue_section nil hsv_to_rgb_match_value nil hsv_to_rgb_red nil hsv_to_rgb_second_largest_component nil] (try (do (when (or (< hsv_to_rgb_hue 0.0) (> hsv_to_rgb_hue 360.0)) (do (println "hue should be between 0 and 360") (throw (ex-info "return" {:v []})))) (when (or (< hsv_to_rgb_saturation 0.0) (> hsv_to_rgb_saturation 1.0)) (do (println "saturation should be between 0 and 1") (throw (ex-info "return" {:v []})))) (when (or (< hsv_to_rgb_value 0.0) (> hsv_to_rgb_value 1.0)) (do (println "value should be between 0 and 1") (throw (ex-info "return" {:v []})))) (set! hsv_to_rgb_chroma (* hsv_to_rgb_value hsv_to_rgb_saturation)) (set! hsv_to_rgb_hue_section (/ hsv_to_rgb_hue 60.0)) (set! hsv_to_rgb_second_largest_component (* hsv_to_rgb_chroma (- 1.0 (absf (- (fmod hsv_to_rgb_hue_section 2.0) 1.0))))) (set! hsv_to_rgb_match_value (- hsv_to_rgb_value hsv_to_rgb_chroma)) (set! hsv_to_rgb_red 0) (set! hsv_to_rgb_green 0) (set! hsv_to_rgb_blue 0) (if (and (>= hsv_to_rgb_hue_section 0.0) (<= hsv_to_rgb_hue_section 1.0)) (do (set! hsv_to_rgb_red (roundf (* 255.0 (+ hsv_to_rgb_chroma hsv_to_rgb_match_value)))) (set! hsv_to_rgb_green (roundf (* 255.0 (+ hsv_to_rgb_second_largest_component hsv_to_rgb_match_value)))) (set! hsv_to_rgb_blue (roundf (* 255.0 hsv_to_rgb_match_value)))) (if (and (> hsv_to_rgb_hue_section 1.0) (<= hsv_to_rgb_hue_section 2.0)) (do (set! hsv_to_rgb_red (roundf (* 255.0 (+ hsv_to_rgb_second_largest_component hsv_to_rgb_match_value)))) (set! hsv_to_rgb_green (roundf (* 255.0 (+ hsv_to_rgb_chroma hsv_to_rgb_match_value)))) (set! hsv_to_rgb_blue (roundf (* 255.0 hsv_to_rgb_match_value)))) (if (and (> hsv_to_rgb_hue_section 2.0) (<= hsv_to_rgb_hue_section 3.0)) (do (set! hsv_to_rgb_red (roundf (* 255.0 hsv_to_rgb_match_value))) (set! hsv_to_rgb_green (roundf (* 255.0 (+ hsv_to_rgb_chroma hsv_to_rgb_match_value)))) (set! hsv_to_rgb_blue (roundf (* 255.0 (+ hsv_to_rgb_second_largest_component hsv_to_rgb_match_value))))) (if (and (> hsv_to_rgb_hue_section 3.0) (<= hsv_to_rgb_hue_section 4.0)) (do (set! hsv_to_rgb_red (roundf (* 255.0 hsv_to_rgb_match_value))) (set! hsv_to_rgb_green (roundf (* 255.0 (+ hsv_to_rgb_second_largest_component hsv_to_rgb_match_value)))) (set! hsv_to_rgb_blue (roundf (* 255.0 (+ hsv_to_rgb_chroma hsv_to_rgb_match_value))))) (if (and (> hsv_to_rgb_hue_section 4.0) (<= hsv_to_rgb_hue_section 5.0)) (do (set! hsv_to_rgb_red (roundf (* 255.0 (+ hsv_to_rgb_second_largest_component hsv_to_rgb_match_value)))) (set! hsv_to_rgb_green (roundf (* 255.0 hsv_to_rgb_match_value))) (set! hsv_to_rgb_blue (roundf (* 255.0 (+ hsv_to_rgb_chroma hsv_to_rgb_match_value))))) (do (set! hsv_to_rgb_red (roundf (* 255.0 (+ hsv_to_rgb_chroma hsv_to_rgb_match_value)))) (set! hsv_to_rgb_green (roundf (* 255.0 hsv_to_rgb_match_value))) (set! hsv_to_rgb_blue (roundf (* 255.0 (+ hsv_to_rgb_second_largest_component hsv_to_rgb_match_value)))))))))) (throw (ex-info "return" {:v [hsv_to_rgb_red hsv_to_rgb_green hsv_to_rgb_blue]}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn rgb_to_hsv [rgb_to_hsv_red rgb_to_hsv_green rgb_to_hsv_blue]
  (binding [rgb_to_hsv_chroma nil rgb_to_hsv_float_blue nil rgb_to_hsv_float_green nil rgb_to_hsv_float_red nil rgb_to_hsv_hue nil rgb_to_hsv_min_val nil rgb_to_hsv_saturation nil rgb_to_hsv_value nil] (try (do (when (or (< rgb_to_hsv_red 0) (> rgb_to_hsv_red 255)) (do (println "red should be between 0 and 255") (throw (ex-info "return" {:v []})))) (when (or (< rgb_to_hsv_green 0) (> rgb_to_hsv_green 255)) (do (println "green should be between 0 and 255") (throw (ex-info "return" {:v []})))) (when (or (< rgb_to_hsv_blue 0) (> rgb_to_hsv_blue 255)) (do (println "blue should be between 0 and 255") (throw (ex-info "return" {:v []})))) (set! rgb_to_hsv_float_red (/ rgb_to_hsv_red 255.0)) (set! rgb_to_hsv_float_green (/ rgb_to_hsv_green 255.0)) (set! rgb_to_hsv_float_blue (/ rgb_to_hsv_blue 255.0)) (set! rgb_to_hsv_value (maxf rgb_to_hsv_float_red rgb_to_hsv_float_green rgb_to_hsv_float_blue)) (set! rgb_to_hsv_min_val (minf rgb_to_hsv_float_red rgb_to_hsv_float_green rgb_to_hsv_float_blue)) (set! rgb_to_hsv_chroma (- rgb_to_hsv_value rgb_to_hsv_min_val)) (set! rgb_to_hsv_saturation (if (= rgb_to_hsv_value 0.0) 0.0 (/ rgb_to_hsv_chroma rgb_to_hsv_value))) (set! rgb_to_hsv_hue 0.0) (if (= rgb_to_hsv_chroma 0.0) (set! rgb_to_hsv_hue 0.0) (if (= rgb_to_hsv_value rgb_to_hsv_float_red) (set! rgb_to_hsv_hue (* 60.0 (+ 0.0 (/ (- rgb_to_hsv_float_green rgb_to_hsv_float_blue) rgb_to_hsv_chroma)))) (if (= rgb_to_hsv_value rgb_to_hsv_float_green) (set! rgb_to_hsv_hue (* 60.0 (+ 2.0 (/ (- rgb_to_hsv_float_blue rgb_to_hsv_float_red) rgb_to_hsv_chroma)))) (set! rgb_to_hsv_hue (* 60.0 (+ 4.0 (/ (- rgb_to_hsv_float_red rgb_to_hsv_float_green) rgb_to_hsv_chroma))))))) (set! rgb_to_hsv_hue (fmod (+ rgb_to_hsv_hue 360.0) 360.0)) (throw (ex-info "return" {:v [rgb_to_hsv_hue rgb_to_hsv_saturation rgb_to_hsv_value]}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn approximately_equal_hsv [approximately_equal_hsv_hsv1 approximately_equal_hsv_hsv2]
  (binding [approximately_equal_hsv_check_hue nil approximately_equal_hsv_check_saturation nil approximately_equal_hsv_check_value nil] (try (do (set! approximately_equal_hsv_check_hue (< (absf (- (nth approximately_equal_hsv_hsv1 0) (nth approximately_equal_hsv_hsv2 0))) 0.2)) (set! approximately_equal_hsv_check_saturation (< (absf (- (nth approximately_equal_hsv_hsv1 1) (nth approximately_equal_hsv_hsv2 1))) 0.002)) (set! approximately_equal_hsv_check_value (< (absf (- (nth approximately_equal_hsv_hsv1 2) (nth approximately_equal_hsv_hsv2 2))) 0.002)) (throw (ex-info "return" {:v (and (and approximately_equal_hsv_check_hue approximately_equal_hsv_check_saturation) approximately_equal_hsv_check_value)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_rgb (hsv_to_rgb 180.0 0.5 0.5))

(def ^:dynamic main_hsv (rgb_to_hsv 64 128 128))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str main_rgb))
      (println (str main_hsv))
      (println (str (approximately_equal_hsv main_hsv [180.0 0.5 0.5])))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
