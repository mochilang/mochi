(ns main (:refer-clojure :exclude [make_decode_map split_spaces encode decode]))

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

(declare make_decode_map split_spaces encode decode)

(def ^:dynamic decode_ch nil)

(def ^:dynamic decode_decoded nil)

(def ^:dynamic decode_i nil)

(def ^:dynamic decode_j nil)

(def ^:dynamic decode_segment nil)

(def ^:dynamic decode_w nil)

(def ^:dynamic decode_word nil)

(def ^:dynamic decode_words nil)

(def ^:dynamic encode_ch nil)

(def ^:dynamic encode_encoded nil)

(def ^:dynamic encode_i nil)

(def ^:dynamic encode_w nil)

(def ^:dynamic make_decode_map_m nil)

(def ^:dynamic split_spaces_ch nil)

(def ^:dynamic split_spaces_current nil)

(def ^:dynamic split_spaces_i nil)

(def ^:dynamic split_spaces_parts nil)

(def ^:dynamic main_encode_map {" " " " "a" "AAAAA" "b" "AAAAB" "c" "AAABA" "d" "AAABB" "e" "AABAA" "f" "AABAB" "g" "AABBA" "h" "AABBB" "i" "ABAAA" "j" "BBBAA" "k" "ABAAB" "l" "ABABA" "m" "ABABB" "n" "ABBAA" "o" "ABBAB" "p" "ABBBA" "q" "ABBBB" "r" "BAAAA" "s" "BAAAB" "t" "BAABA" "u" "BAABB" "v" "BBBAB" "w" "BABAA" "x" "BABAB" "y" "BABBA" "z" "BABBB"})

(defn make_decode_map []
  (binding [make_decode_map_m nil] (try (do (set! make_decode_map_m {}) (doseq [k (keys main_encode_map)] (set! make_decode_map_m (assoc make_decode_map_m (get main_encode_map k) k))) (throw (ex-info "return" {:v make_decode_map_m}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_decode_map (make_decode_map))

(defn split_spaces [split_spaces_s]
  (binding [split_spaces_ch nil split_spaces_current nil split_spaces_i nil split_spaces_parts nil] (try (do (set! split_spaces_parts []) (set! split_spaces_current "") (set! split_spaces_i 0) (while (< split_spaces_i (count split_spaces_s)) (do (set! split_spaces_ch (subs split_spaces_s split_spaces_i (min (+ split_spaces_i 1) (count split_spaces_s)))) (if (= split_spaces_ch " ") (do (set! split_spaces_parts (conj split_spaces_parts split_spaces_current)) (set! split_spaces_current "")) (set! split_spaces_current (str split_spaces_current split_spaces_ch))) (set! split_spaces_i (+ split_spaces_i 1)))) (set! split_spaces_parts (conj split_spaces_parts split_spaces_current)) (throw (ex-info "return" {:v split_spaces_parts}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn encode [encode_word]
  (binding [encode_ch nil encode_encoded nil encode_i nil encode_w nil] (try (do (set! encode_w (clojure.string/lower-case encode_word)) (set! encode_encoded "") (set! encode_i 0) (while (< encode_i (count encode_w)) (do (set! encode_ch (subs encode_w encode_i (min (+ encode_i 1) (count encode_w)))) (if (in encode_ch main_encode_map) (set! encode_encoded (str encode_encoded (get main_encode_map encode_ch))) (throw (Exception. "encode() accepts only letters of the alphabet and spaces"))) (set! encode_i (+ encode_i 1)))) (throw (ex-info "return" {:v encode_encoded}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn decode [decode_coded]
  (binding [decode_ch nil decode_decoded nil decode_i nil decode_j nil decode_segment nil decode_w nil decode_word nil decode_words nil] (try (do (set! decode_i 0) (while (< decode_i (count decode_coded)) (do (set! decode_ch (subs decode_coded decode_i (min (+ decode_i 1) (count decode_coded)))) (when (and (and (not= decode_ch "A") (not= decode_ch "B")) (not= decode_ch " ")) (throw (Exception. "decode() accepts only 'A', 'B' and spaces"))) (set! decode_i (+ decode_i 1)))) (set! decode_words (split_spaces decode_coded)) (set! decode_decoded "") (set! decode_w 0) (while (< decode_w (count decode_words)) (do (set! decode_word (nth decode_words decode_w)) (set! decode_j 0) (while (< decode_j (count decode_word)) (do (set! decode_segment (subs decode_word decode_j (min (+ decode_j 5) (count decode_word)))) (set! decode_decoded (str decode_decoded (get main_decode_map decode_segment))) (set! decode_j (+ decode_j 5)))) (when (< decode_w (- (count decode_words) 1)) (set! decode_decoded (str decode_decoded " "))) (set! decode_w (+ decode_w 1)))) (throw (ex-info "return" {:v decode_decoded}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (encode "hello"))
      (println (encode "hello world"))
      (println (decode "AABBBAABAAABABAABABAABBAB BABAAABBABBAAAAABABAAAABB"))
      (println (decode "AABBBAABAAABABAABABAABBAB"))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
