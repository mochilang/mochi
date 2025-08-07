(ns main (:refer-clojure :exclude [rstrip_s normalize_alias has_unit from_factor to_factor length_conversion]))

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

(declare rstrip_s normalize_alias has_unit from_factor to_factor length_conversion)

(def ^:dynamic length_conversion_new_from nil)

(def ^:dynamic length_conversion_new_to nil)

(defn rstrip_s [rstrip_s_s]
  (try (if (and (> (count rstrip_s_s) 0) (= (nth rstrip_s_s (- (count rstrip_s_s) 1)) "s")) (subs rstrip_s_s 0 (min (- (count rstrip_s_s) 1) (count rstrip_s_s))) rstrip_s_s) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn normalize_alias [normalize_alias_u]
  (try (do (when (= normalize_alias_u "millimeter") (throw (ex-info "return" {:v "mm"}))) (when (= normalize_alias_u "centimeter") (throw (ex-info "return" {:v "cm"}))) (when (= normalize_alias_u "meter") (throw (ex-info "return" {:v "m"}))) (when (= normalize_alias_u "kilometer") (throw (ex-info "return" {:v "km"}))) (when (= normalize_alias_u "inch") (throw (ex-info "return" {:v "in"}))) (when (= normalize_alias_u "inche") (throw (ex-info "return" {:v "in"}))) (when (= normalize_alias_u "feet") (throw (ex-info "return" {:v "ft"}))) (when (= normalize_alias_u "foot") (throw (ex-info "return" {:v "ft"}))) (when (= normalize_alias_u "yard") (throw (ex-info "return" {:v "yd"}))) (if (= normalize_alias_u "mile") "mi" normalize_alias_u)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn has_unit [has_unit_u]
  (try (throw (ex-info "return" {:v (or (or (or (or (or (or (or (= has_unit_u "mm") (= has_unit_u "cm")) (= has_unit_u "m")) (= has_unit_u "km")) (= has_unit_u "in")) (= has_unit_u "ft")) (= has_unit_u "yd")) (= has_unit_u "mi"))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn from_factor [from_factor_u]
  (try (do (when (= from_factor_u "mm") (throw (ex-info "return" {:v 0.001}))) (when (= from_factor_u "cm") (throw (ex-info "return" {:v 0.01}))) (when (= from_factor_u "m") (throw (ex-info "return" {:v 1.0}))) (when (= from_factor_u "km") (throw (ex-info "return" {:v 1000.0}))) (when (= from_factor_u "in") (throw (ex-info "return" {:v 0.0254}))) (when (= from_factor_u "ft") (throw (ex-info "return" {:v 0.3048}))) (when (= from_factor_u "yd") (throw (ex-info "return" {:v 0.9144}))) (if (= from_factor_u "mi") 1609.34 0.0)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn to_factor [to_factor_u]
  (try (do (when (= to_factor_u "mm") (throw (ex-info "return" {:v 1000.0}))) (when (= to_factor_u "cm") (throw (ex-info "return" {:v 100.0}))) (when (= to_factor_u "m") (throw (ex-info "return" {:v 1.0}))) (when (= to_factor_u "km") (throw (ex-info "return" {:v 0.001}))) (when (= to_factor_u "in") (throw (ex-info "return" {:v 39.3701}))) (when (= to_factor_u "ft") (throw (ex-info "return" {:v 3.28084}))) (when (= to_factor_u "yd") (throw (ex-info "return" {:v 1.09361}))) (if (= to_factor_u "mi") 0.000621371 0.0)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn length_conversion [length_conversion_value length_conversion_from_type length_conversion_to_type]
  (binding [length_conversion_new_from nil length_conversion_new_to nil] (try (do (set! length_conversion_new_from (normalize_alias (rstrip_s (clojure.string/lower-case length_conversion_from_type)))) (set! length_conversion_new_to (normalize_alias (rstrip_s (clojure.string/lower-case length_conversion_to_type)))) (when (not (has_unit length_conversion_new_from)) (throw (Exception. (str (str "Invalid 'from_type' value: '" length_conversion_from_type) "'.\nConversion abbreviations are: mm, cm, m, km, in, ft, yd, mi")))) (when (not (has_unit length_conversion_new_to)) (throw (Exception. (str (str "Invalid 'to_type' value: '" length_conversion_to_type) "'.\nConversion abbreviations are: mm, cm, m, km, in, ft, yd, mi")))) (throw (ex-info "return" {:v (* (* length_conversion_value (from_factor length_conversion_new_from)) (to_factor length_conversion_new_to))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (length_conversion 4.0 "METER" "FEET"))
      (println (length_conversion 1.0 "kilometer" "inch"))
      (println (length_conversion 2.0 "feet" "meter"))
      (println (length_conversion 2.0 "centimeter" "millimeter"))
      (println (length_conversion 4.0 "yard" "kilometer"))
      (println (length_conversion 3.0 "foot" "inch"))
      (println (length_conversion 3.0 "mm" "in"))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
