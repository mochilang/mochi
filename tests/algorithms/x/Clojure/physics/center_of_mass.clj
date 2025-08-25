(ns main (:refer-clojure :exclude [round2 center_of_mass coord_to_string]))

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
  (int (Double/valueOf (str s))))

(defn _ord [s]
  (int (first s)))

(defn mochi_str [v]
  (cond (float? v) (let [s (str v)] (if (clojure.string/ends-with? s ".0") (subs s 0 (- (count s) 2)) s)) :else (str v)))

(defn _fetch [url]
  {:data [{:from "" :intensity {:actual 0 :forecast 0 :index ""} :to ""}]})

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare round2 center_of_mass coord_to_string)

(declare _read_file)

(def ^:dynamic center_of_mass_cm_x nil)

(def ^:dynamic center_of_mass_cm_y nil)

(def ^:dynamic center_of_mass_cm_z nil)

(def ^:dynamic center_of_mass_i nil)

(def ^:dynamic center_of_mass_p nil)

(def ^:dynamic center_of_mass_sum_x nil)

(def ^:dynamic center_of_mass_sum_y nil)

(def ^:dynamic center_of_mass_sum_z nil)

(def ^:dynamic center_of_mass_total_mass nil)

(def ^:dynamic round2_rounded nil)

(def ^:dynamic round2_scaled nil)

(defn round2 [round2_x]
  (binding [round2_rounded nil round2_scaled nil] (try (do (set! round2_scaled (*' round2_x 100.0)) (set! round2_rounded (double (long (+' round2_scaled 0.5)))) (throw (ex-info "return" {:v (/ round2_rounded 100.0)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn center_of_mass [center_of_mass_ps]
  (binding [center_of_mass_cm_x nil center_of_mass_cm_y nil center_of_mass_cm_z nil center_of_mass_i nil center_of_mass_p nil center_of_mass_sum_x nil center_of_mass_sum_y nil center_of_mass_sum_z nil center_of_mass_total_mass nil] (try (do (when (= (count center_of_mass_ps) 0) (throw (Exception. "No particles provided"))) (set! center_of_mass_i 0) (set! center_of_mass_total_mass 0.0) (while (< center_of_mass_i (count center_of_mass_ps)) (do (set! center_of_mass_p (nth center_of_mass_ps center_of_mass_i)) (when (<= (:mass center_of_mass_p) 0.0) (throw (Exception. "Mass of all particles must be greater than 0"))) (set! center_of_mass_total_mass (+' center_of_mass_total_mass (:mass center_of_mass_p))) (set! center_of_mass_i (+' center_of_mass_i 1)))) (set! center_of_mass_sum_x 0.0) (set! center_of_mass_sum_y 0.0) (set! center_of_mass_sum_z 0.0) (set! center_of_mass_i 0) (while (< center_of_mass_i (count center_of_mass_ps)) (do (set! center_of_mass_p (nth center_of_mass_ps center_of_mass_i)) (set! center_of_mass_sum_x (+' center_of_mass_sum_x (*' (:x center_of_mass_p) (:mass center_of_mass_p)))) (set! center_of_mass_sum_y (+' center_of_mass_sum_y (*' (:y center_of_mass_p) (:mass center_of_mass_p)))) (set! center_of_mass_sum_z (+' center_of_mass_sum_z (*' (:z center_of_mass_p) (:mass center_of_mass_p)))) (set! center_of_mass_i (+' center_of_mass_i 1)))) (set! center_of_mass_cm_x (round2 (/ center_of_mass_sum_x center_of_mass_total_mass))) (set! center_of_mass_cm_y (round2 (/ center_of_mass_sum_y center_of_mass_total_mass))) (set! center_of_mass_cm_z (round2 (/ center_of_mass_sum_z center_of_mass_total_mass))) (throw (ex-info "return" {:v {:x center_of_mass_cm_x :y center_of_mass_cm_y :z center_of_mass_cm_z}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn coord_to_string [coord_to_string_c]
  (try (throw (ex-info "return" {:v (str (str (str (str (str (str "Coord3D(x=" (mochi_str (:x coord_to_string_c))) ", y=") (mochi_str (:y coord_to_string_c))) ", z=") (mochi_str (:z coord_to_string_c))) ")")})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(def ^:dynamic main_r1 nil)

(def ^:dynamic main_r2 nil)

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (alter-var-root (var main_r1) (constantly (center_of_mass [{:mass 4.0 :x 1.5 :y 4.0 :z 3.4} {:mass 8.1 :x 5.0 :y 6.8 :z 7.0} {:mass 12.0 :x 9.4 :y 10.1 :z 11.6}])))
      (println (coord_to_string main_r1))
      (alter-var-root (var main_r2) (constantly (center_of_mass [{:mass 4.0 :x 1.0 :y 2.0 :z 3.0} {:mass 8.0 :x 5.0 :y 6.0 :z 7.0} {:mass 12.0 :x 9.0 :y 10.0 :z 11.0}])))
      (println (coord_to_string main_r2))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
