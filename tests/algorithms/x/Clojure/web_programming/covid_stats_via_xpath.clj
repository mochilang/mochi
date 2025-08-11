(ns main (:refer-clojure :exclude [parse_int find extract_numbers covid_stats main]))

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

(defn _fetch [url]
  {:data [{:from "" :intensity {:actual 0 :forecast 0 :index ""} :to ""}]})

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare parse_int find extract_numbers covid_stats main)

(def ^:dynamic covid_stats_nums nil)

(def ^:dynamic extract_numbers_content_start nil)

(def ^:dynamic extract_numbers_e nil)

(def ^:dynamic extract_numbers_end_tag nil)

(def ^:dynamic extract_numbers_num_str nil)

(def ^:dynamic extract_numbers_nums nil)

(def ^:dynamic extract_numbers_pos nil)

(def ^:dynamic extract_numbers_s nil)

(def ^:dynamic extract_numbers_start_tag nil)

(def ^:dynamic find_i nil)

(def ^:dynamic find_j nil)

(def ^:dynamic find_matched nil)

(def ^:dynamic find_nlen nil)

(def ^:dynamic main_sample_html nil)

(def ^:dynamic main_stats nil)

(def ^:dynamic parse_int_ch nil)

(def ^:dynamic parse_int_i nil)

(def ^:dynamic parse_int_value nil)

(defn parse_int [parse_int_s]
  (binding [parse_int_ch nil parse_int_i nil parse_int_value nil] (try (do (set! parse_int_value 0) (set! parse_int_i 0) (loop [while_flag_1 true] (when (and while_flag_1 (< parse_int_i (count parse_int_s))) (do (set! parse_int_ch (subs parse_int_s parse_int_i (min (+ parse_int_i 1) (count parse_int_s)))) (cond (= parse_int_ch ",") (do (set! parse_int_i (+ parse_int_i 1)) (recur true)) :else (do (set! parse_int_value (+ (* parse_int_value 10) (Long/parseLong parse_int_ch))) (set! parse_int_i (+ parse_int_i 1)) (recur while_flag_1)))))) (throw (ex-info "return" {:v parse_int_value}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn find [find_haystack find_needle find_start]
  (binding [find_i nil find_j nil find_matched nil find_nlen nil] (try (do (set! find_nlen (count find_needle)) (set! find_i find_start) (while (<= find_i (- (count find_haystack) find_nlen)) (do (set! find_j 0) (set! find_matched true) (loop [while_flag_2 true] (when (and while_flag_2 (< find_j find_nlen)) (cond (not= (subs find_haystack (+ find_i find_j) (min (+ (+ find_i find_j) 1) (count find_haystack))) (subs find_needle find_j (min (+ find_j 1) (count find_needle)))) (do (set! find_matched false) (recur false)) :else (do (set! find_j (+ find_j 1)) (recur while_flag_2))))) (when find_matched (throw (ex-info "return" {:v find_i}))) (set! find_i (+ find_i 1)))) (throw (ex-info "return" {:v (- 0 1)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn extract_numbers [extract_numbers_html]
  (binding [extract_numbers_content_start nil extract_numbers_e nil extract_numbers_end_tag nil extract_numbers_num_str nil extract_numbers_nums nil extract_numbers_pos nil extract_numbers_s nil extract_numbers_start_tag nil] (try (do (set! extract_numbers_nums []) (set! extract_numbers_pos 0) (set! extract_numbers_start_tag "<span>") (set! extract_numbers_end_tag "</span>") (loop [while_flag_3 true] (when (and while_flag_3 true) (do (set! extract_numbers_s (find extract_numbers_html extract_numbers_start_tag extract_numbers_pos)) (cond (= extract_numbers_s (- 0 1)) (recur false) (= extract_numbers_e (- 0 1)) (recur false) :else (do (set! extract_numbers_content_start (+ extract_numbers_s (count extract_numbers_start_tag))) (set! extract_numbers_e (find extract_numbers_html extract_numbers_end_tag extract_numbers_content_start)) (set! extract_numbers_num_str (subs extract_numbers_html extract_numbers_content_start (min extract_numbers_e (count extract_numbers_html)))) (set! extract_numbers_nums (conj extract_numbers_nums (parse_int extract_numbers_num_str))) (set! extract_numbers_pos (+ extract_numbers_e (count extract_numbers_end_tag))) (recur while_flag_3)))))) (throw (ex-info "return" {:v extract_numbers_nums}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn covid_stats [covid_stats_html]
  (binding [covid_stats_nums nil] (try (do (set! covid_stats_nums (extract_numbers covid_stats_html)) (throw (ex-info "return" {:v {:cases (nth covid_stats_nums 0) :deaths (nth covid_stats_nums 1) :recovered (nth covid_stats_nums 2)}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_sample_html nil main_stats nil] (do (set! main_sample_html (str (str "<div class=\"maincounter-number\"><span>123456</span></div>" "<div class=\"maincounter-number\"><span>7890</span></div>") "<div class=\"maincounter-number\"><span>101112</span></div>")) (set! main_stats (covid_stats main_sample_html)) (println (str "Total COVID-19 cases in the world: " (str (:cases main_stats)))) (println (str "Total deaths due to COVID-19 in the world: " (str (:deaths main_stats)))) (println (str "Total COVID-19 patients recovered in the world: " (str (:recovered main_stats)))))))

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
