(ns main (:refer-clojure :exclude [list_contains index_in_string contains_char to_uppercase plugboard_map reflector_map count_unique build_plugboard validator enigma main]))

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

(declare list_contains index_in_string contains_char to_uppercase plugboard_map reflector_map count_unique build_plugboard validator enigma main)

(def ^:dynamic build_plugboard_a nil)

(def ^:dynamic build_plugboard_b nil)

(def ^:dynamic build_plugboard_ch nil)

(def ^:dynamic build_plugboard_i nil)

(def ^:dynamic build_plugboard_pb nil)

(def ^:dynamic build_plugboard_pbstring_nospace nil)

(def ^:dynamic build_plugboard_seen nil)

(def ^:dynamic count_unique_i nil)

(def ^:dynamic count_unique_unique nil)

(def ^:dynamic enigma_i nil)

(def ^:dynamic enigma_index nil)

(def ^:dynamic enigma_plugboard nil)

(def ^:dynamic enigma_result nil)

(def ^:dynamic enigma_rotor_a nil)

(def ^:dynamic enigma_rotor_b nil)

(def ^:dynamic enigma_rotor_c nil)

(def ^:dynamic enigma_rotorpos1 nil)

(def ^:dynamic enigma_rotorpos2 nil)

(def ^:dynamic enigma_rotorpos3 nil)

(def ^:dynamic enigma_symbol nil)

(def ^:dynamic enigma_up_pb nil)

(def ^:dynamic enigma_up_text nil)

(def ^:dynamic index_in_string_i nil)

(def ^:dynamic list_contains_i nil)

(def ^:dynamic main_en nil)

(def ^:dynamic main_message nil)

(def ^:dynamic main_pb nil)

(def ^:dynamic main_rotor_pos nil)

(def ^:dynamic main_rotor_sel nil)

(def ^:dynamic plugboard_map_a nil)

(def ^:dynamic plugboard_map_b nil)

(def ^:dynamic plugboard_map_i nil)

(def ^:dynamic plugboard_map_pair nil)

(def ^:dynamic reflector_map_a nil)

(def ^:dynamic reflector_map_b nil)

(def ^:dynamic reflector_map_i nil)

(def ^:dynamic reflector_map_pair nil)

(def ^:dynamic to_uppercase_ch nil)

(def ^:dynamic to_uppercase_i nil)

(def ^:dynamic to_uppercase_idx nil)

(def ^:dynamic to_uppercase_res nil)

(def ^:dynamic validator_r1 nil)

(def ^:dynamic validator_r2 nil)

(def ^:dynamic validator_r3 nil)

(def ^:dynamic main_abc "ABCDEFGHIJKLMNOPQRSTUVWXYZ")

(def ^:dynamic main_low_abc "abcdefghijklmnopqrstuvwxyz")

(def ^:dynamic main_rotor1 "EGZWVONAHDCLFQMSIPJBYUKXTR")

(def ^:dynamic main_rotor2 "FOBHMDKEXQNRAULPGSJVTYICZW")

(def ^:dynamic main_rotor3 "ZJXESIUQLHAVRMDOYGTNFWPBKC")

(def ^:dynamic main_rotor4 "RMDJXFUWGISLHVTCQNKYPBEZOA")

(def ^:dynamic main_rotor5 "SGLCPQWZHKXAREONTFBVIYJUDM")

(def ^:dynamic main_rotor6 "HVSICLTYKQUBXDWAJZOMFGPREN")

(def ^:dynamic main_rotor7 "RZWQHFMVDBKICJLNTUXAGYPSOE")

(def ^:dynamic main_rotor8 "LFKIJODBEGAMQPXVUHYSTCZRWN")

(def ^:dynamic main_rotor9 "KOAEGVDHXPQZMLFTYWJNBRCIUS")

(def ^:dynamic main_reflector_pairs ["AN" "BO" "CP" "DQ" "ER" "FS" "GT" "HU" "IV" "JW" "KX" "LY" "MZ"])

(defn list_contains [list_contains_xs list_contains_x]
  (binding [list_contains_i nil] (try (do (set! list_contains_i 0) (while (< list_contains_i (count list_contains_xs)) (do (when (= (nth list_contains_xs list_contains_i) list_contains_x) (throw (ex-info "return" {:v true}))) (set! list_contains_i (+ list_contains_i 1)))) (throw (ex-info "return" {:v false}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn index_in_string [index_in_string_s index_in_string_ch]
  (binding [index_in_string_i nil] (try (do (set! index_in_string_i 0) (while (< index_in_string_i (count index_in_string_s)) (do (when (= (subs index_in_string_s index_in_string_i (min (+ index_in_string_i 1) (count index_in_string_s))) index_in_string_ch) (throw (ex-info "return" {:v index_in_string_i}))) (set! index_in_string_i (+ index_in_string_i 1)))) (throw (ex-info "return" {:v (- 1)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn contains_char [contains_char_s contains_char_ch]
  (try (throw (ex-info "return" {:v (>= (index_in_string contains_char_s contains_char_ch) 0)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn to_uppercase [to_uppercase_s]
  (binding [to_uppercase_ch nil to_uppercase_i nil to_uppercase_idx nil to_uppercase_res nil] (try (do (set! to_uppercase_res "") (set! to_uppercase_i 0) (while (< to_uppercase_i (count to_uppercase_s)) (do (set! to_uppercase_ch (subs to_uppercase_s to_uppercase_i (min (+ to_uppercase_i 1) (count to_uppercase_s)))) (set! to_uppercase_idx (index_in_string main_low_abc to_uppercase_ch)) (if (>= to_uppercase_idx 0) (set! to_uppercase_res (str to_uppercase_res (subs main_abc to_uppercase_idx (min (+ to_uppercase_idx 1) (count main_abc))))) (set! to_uppercase_res (str to_uppercase_res to_uppercase_ch))) (set! to_uppercase_i (+ to_uppercase_i 1)))) (throw (ex-info "return" {:v to_uppercase_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn plugboard_map [plugboard_map_pb plugboard_map_ch]
  (binding [plugboard_map_a nil plugboard_map_b nil plugboard_map_i nil plugboard_map_pair nil] (try (do (set! plugboard_map_i 0) (while (< plugboard_map_i (count plugboard_map_pb)) (do (set! plugboard_map_pair (nth plugboard_map_pb plugboard_map_i)) (set! plugboard_map_a (subs plugboard_map_pair 0 (min 1 (count plugboard_map_pair)))) (set! plugboard_map_b (subs plugboard_map_pair 1 (min 2 (count plugboard_map_pair)))) (when (= plugboard_map_ch plugboard_map_a) (throw (ex-info "return" {:v plugboard_map_b}))) (when (= plugboard_map_ch plugboard_map_b) (throw (ex-info "return" {:v plugboard_map_a}))) (set! plugboard_map_i (+ plugboard_map_i 1)))) (throw (ex-info "return" {:v plugboard_map_ch}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn reflector_map [reflector_map_ch]
  (binding [reflector_map_a nil reflector_map_b nil reflector_map_i nil reflector_map_pair nil] (try (do (set! reflector_map_i 0) (while (< reflector_map_i (count main_reflector_pairs)) (do (set! reflector_map_pair (nth main_reflector_pairs reflector_map_i)) (set! reflector_map_a (subs reflector_map_pair 0 (min 1 (count reflector_map_pair)))) (set! reflector_map_b (subs reflector_map_pair 1 (min 2 (count reflector_map_pair)))) (when (= reflector_map_ch reflector_map_a) (throw (ex-info "return" {:v reflector_map_b}))) (when (= reflector_map_ch reflector_map_b) (throw (ex-info "return" {:v reflector_map_a}))) (set! reflector_map_i (+ reflector_map_i 1)))) (throw (ex-info "return" {:v reflector_map_ch}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn count_unique [count_unique_xs]
  (binding [count_unique_i nil count_unique_unique nil] (try (do (set! count_unique_unique []) (set! count_unique_i 0) (while (< count_unique_i (count count_unique_xs)) (do (when (not (list_contains count_unique_unique (nth count_unique_xs count_unique_i))) (set! count_unique_unique (conj count_unique_unique (nth count_unique_xs count_unique_i)))) (set! count_unique_i (+ count_unique_i 1)))) (throw (ex-info "return" {:v (count count_unique_unique)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn build_plugboard [build_plugboard_pbstring]
  (binding [build_plugboard_a nil build_plugboard_b nil build_plugboard_ch nil build_plugboard_i nil build_plugboard_pb nil build_plugboard_pbstring_nospace nil build_plugboard_seen nil] (try (do (when (= (count build_plugboard_pbstring) 0) (throw (ex-info "return" {:v []}))) (when (not= (mod (count build_plugboard_pbstring) 2) 0) (throw (Exception. (str (str "Odd number of symbols(" (str (count build_plugboard_pbstring))) ")")))) (set! build_plugboard_pbstring_nospace "") (set! build_plugboard_i 0) (while (< build_plugboard_i (count build_plugboard_pbstring)) (do (set! build_plugboard_ch (subs build_plugboard_pbstring build_plugboard_i (min (+ build_plugboard_i 1) (count build_plugboard_pbstring)))) (when (not= build_plugboard_ch " ") (set! build_plugboard_pbstring_nospace (str build_plugboard_pbstring_nospace build_plugboard_ch))) (set! build_plugboard_i (+ build_plugboard_i 1)))) (set! build_plugboard_seen []) (set! build_plugboard_i 0) (while (< build_plugboard_i (count build_plugboard_pbstring_nospace)) (do (set! build_plugboard_ch (subs build_plugboard_pbstring_nospace build_plugboard_i (min (+ build_plugboard_i 1) (count build_plugboard_pbstring_nospace)))) (when (not (contains_char main_abc build_plugboard_ch)) (throw (Exception. (str (str "'" build_plugboard_ch) "' not in list of symbols")))) (when (list_contains build_plugboard_seen build_plugboard_ch) (throw (Exception. (str (str "Duplicate symbol(" build_plugboard_ch) ")")))) (set! build_plugboard_seen (conj build_plugboard_seen build_plugboard_ch)) (set! build_plugboard_i (+ build_plugboard_i 1)))) (set! build_plugboard_pb []) (set! build_plugboard_i 0) (while (< build_plugboard_i (- (count build_plugboard_pbstring_nospace) 1)) (do (set! build_plugboard_a (subs build_plugboard_pbstring_nospace build_plugboard_i (min (+ build_plugboard_i 1) (count build_plugboard_pbstring_nospace)))) (set! build_plugboard_b (subs build_plugboard_pbstring_nospace (+ build_plugboard_i 1) (min (+ build_plugboard_i 2) (count build_plugboard_pbstring_nospace)))) (set! build_plugboard_pb (conj build_plugboard_pb (str build_plugboard_a build_plugboard_b))) (set! build_plugboard_i (+ build_plugboard_i 2)))) (throw (ex-info "return" {:v build_plugboard_pb}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn validator [validator_rotpos validator_rotsel validator_pb]
  (binding [validator_r1 nil validator_r2 nil validator_r3 nil] (do (when (< (count_unique validator_rotsel) 3) (throw (Exception. (str (str "Please use 3 unique rotors (not " (str (count_unique validator_rotsel))) ")")))) (when (not= (count validator_rotpos) 3) (throw (Exception. "Rotor position must have 3 values"))) (set! validator_r1 (nth validator_rotpos 0)) (set! validator_r2 (nth validator_rotpos 1)) (set! validator_r3 (nth validator_rotpos 2)) (when (not (and (< 0 validator_r1) (<= validator_r1 (count main_abc)))) (throw (Exception. (str (str "First rotor position is not within range of 1..26 (" (str validator_r1)) ")")))) (when (not (and (< 0 validator_r2) (<= validator_r2 (count main_abc)))) (throw (Exception. (str (str "Second rotor position is not within range of 1..26 (" (str validator_r2)) ")")))) (when (not (and (< 0 validator_r3) (<= validator_r3 (count main_abc)))) (throw (Exception. (str (str "Third rotor position is not within range of 1..26 (" (str validator_r3)) ")")))))))

(defn enigma [enigma_text enigma_rotor_position enigma_rotor_selection enigma_plugb]
  (binding [enigma_i nil enigma_index nil enigma_plugboard nil enigma_result nil enigma_rotor_a nil enigma_rotor_b nil enigma_rotor_c nil enigma_rotorpos1 nil enigma_rotorpos2 nil enigma_rotorpos3 nil enigma_symbol nil enigma_up_pb nil enigma_up_text nil] (try (do (set! enigma_up_text (to_uppercase enigma_text)) (set! enigma_up_pb (to_uppercase enigma_plugb)) (validator enigma_rotor_position enigma_rotor_selection enigma_up_pb) (set! enigma_plugboard (build_plugboard enigma_up_pb)) (set! enigma_rotorpos1 (- (nth enigma_rotor_position 0) 1)) (set! enigma_rotorpos2 (- (nth enigma_rotor_position 1) 1)) (set! enigma_rotorpos3 (- (nth enigma_rotor_position 2) 1)) (set! enigma_rotor_a (nth enigma_rotor_selection 0)) (set! enigma_rotor_b (nth enigma_rotor_selection 1)) (set! enigma_rotor_c (nth enigma_rotor_selection 2)) (set! enigma_result "") (set! enigma_i 0) (while (< enigma_i (count enigma_up_text)) (do (set! enigma_symbol (subs enigma_up_text enigma_i (min (+ enigma_i 1) (count enigma_up_text)))) (when (contains_char main_abc enigma_symbol) (do (set! enigma_symbol (plugboard_map enigma_plugboard enigma_symbol)) (set! enigma_index (+ (index_in_string main_abc enigma_symbol) enigma_rotorpos1)) (set! enigma_symbol (subs enigma_rotor_a (mod enigma_index (count main_abc)) (min (+ (mod enigma_index (count main_abc)) 1) (count enigma_rotor_a)))) (set! enigma_index (+ (index_in_string main_abc enigma_symbol) enigma_rotorpos2)) (set! enigma_symbol (subs enigma_rotor_b (mod enigma_index (count main_abc)) (min (+ (mod enigma_index (count main_abc)) 1) (count enigma_rotor_b)))) (set! enigma_index (+ (index_in_string main_abc enigma_symbol) enigma_rotorpos3)) (set! enigma_symbol (subs enigma_rotor_c (mod enigma_index (count main_abc)) (min (+ (mod enigma_index (count main_abc)) 1) (count enigma_rotor_c)))) (set! enigma_symbol (reflector_map enigma_symbol)) (set! enigma_index (- (index_in_string enigma_rotor_c enigma_symbol) enigma_rotorpos3)) (when (< enigma_index 0) (set! enigma_index (+ enigma_index (count main_abc)))) (set! enigma_symbol (subs main_abc enigma_index (min (+ enigma_index 1) (count main_abc)))) (set! enigma_index (- (index_in_string enigma_rotor_b enigma_symbol) enigma_rotorpos2)) (when (< enigma_index 0) (set! enigma_index (+ enigma_index (count main_abc)))) (set! enigma_symbol (subs main_abc enigma_index (min (+ enigma_index 1) (count main_abc)))) (set! enigma_index (- (index_in_string enigma_rotor_a enigma_symbol) enigma_rotorpos1)) (when (< enigma_index 0) (set! enigma_index (+ enigma_index (count main_abc)))) (set! enigma_symbol (subs main_abc enigma_index (min (+ enigma_index 1) (count main_abc)))) (set! enigma_symbol (plugboard_map enigma_plugboard enigma_symbol)) (set! enigma_rotorpos1 (+ enigma_rotorpos1 1)) (when (>= enigma_rotorpos1 (count main_abc)) (do (set! enigma_rotorpos1 0) (set! enigma_rotorpos2 (+ enigma_rotorpos2 1)))) (when (>= enigma_rotorpos2 (count main_abc)) (do (set! enigma_rotorpos2 0) (set! enigma_rotorpos3 (+ enigma_rotorpos3 1)))) (when (>= enigma_rotorpos3 (count main_abc)) (set! enigma_rotorpos3 0)))) (set! enigma_result (str enigma_result enigma_symbol)) (set! enigma_i (+ enigma_i 1)))) (throw (ex-info "return" {:v enigma_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_en nil main_message nil main_pb nil main_rotor_pos nil main_rotor_sel nil] (do (set! main_message "This is my Python script that emulates the Enigma machine from WWII.") (set! main_rotor_pos [1 1 1]) (set! main_pb "pictures") (set! main_rotor_sel [main_rotor2 main_rotor4 main_rotor8]) (set! main_en (enigma main_message main_rotor_pos main_rotor_sel main_pb)) (println (str "Encrypted message: " main_en)) (println (str "Decrypted message: " (enigma main_en main_rotor_pos main_rotor_sel main_pb))))))

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
