(ns main (:refer-clojure :exclude [count_char char_in starts_with_char ends_with_char contains_double_dot is_valid_email_address]))

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

(declare count_char char_in starts_with_char ends_with_char contains_double_dot is_valid_email_address)

(def ^:dynamic char_in_i nil)

(def ^:dynamic contains_double_dot_i nil)

(def ^:dynamic count_char_cnt nil)

(def ^:dynamic count_char_i nil)

(def ^:dynamic is_valid_email_address_at_idx nil)

(def ^:dynamic is_valid_email_address_ch nil)

(def ^:dynamic is_valid_email_address_domain nil)

(def ^:dynamic is_valid_email_address_i nil)

(def ^:dynamic is_valid_email_address_local_part nil)

(def ^:dynamic main_idx nil)

(def ^:dynamic main_MAX_LOCAL_PART_OCTETS 64)

(def ^:dynamic main_MAX_DOMAIN_OCTETS 255)

(def ^:dynamic main_ASCII_LETTERS "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")

(def ^:dynamic main_DIGITS "0123456789")

(def ^:dynamic main_LOCAL_EXTRA ".(!#$%&'*+-/=?^_`{|}~)")

(def ^:dynamic main_DOMAIN_EXTRA ".-")

(defn count_char [count_char_s count_char_target]
  (binding [count_char_cnt nil count_char_i nil] (try (do (set! count_char_cnt 0) (set! count_char_i 0) (while (< count_char_i (count count_char_s)) (do (when (= (subs count_char_s count_char_i (min (+ count_char_i 1) (count count_char_s))) count_char_target) (set! count_char_cnt (+ count_char_cnt 1))) (set! count_char_i (+ count_char_i 1)))) (throw (ex-info "return" {:v count_char_cnt}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn char_in [char_in_c char_in_allowed]
  (binding [char_in_i nil] (try (do (set! char_in_i 0) (while (< char_in_i (count char_in_allowed)) (do (when (= (subs char_in_allowed char_in_i (min (+ char_in_i 1) (count char_in_allowed))) char_in_c) (throw (ex-info "return" {:v true}))) (set! char_in_i (+ char_in_i 1)))) (throw (ex-info "return" {:v false}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn starts_with_char [starts_with_char_s starts_with_char_c]
  (try (throw (ex-info "return" {:v (and (> (count starts_with_char_s) 0) (= (subs starts_with_char_s 0 (min 1 (count starts_with_char_s))) starts_with_char_c))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn ends_with_char [ends_with_char_s ends_with_char_c]
  (try (throw (ex-info "return" {:v (and (> (count ends_with_char_s) 0) (= (subs ends_with_char_s (- (count ends_with_char_s) 1) (min (count ends_with_char_s) (count ends_with_char_s))) ends_with_char_c))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn contains_double_dot [contains_double_dot_s]
  (binding [contains_double_dot_i nil] (try (do (when (< (count contains_double_dot_s) 2) (throw (ex-info "return" {:v false}))) (set! contains_double_dot_i 0) (while (< contains_double_dot_i (- (count contains_double_dot_s) 1)) (do (when (= (subs contains_double_dot_s contains_double_dot_i (min (+ contains_double_dot_i 2) (count contains_double_dot_s))) "..") (throw (ex-info "return" {:v true}))) (set! contains_double_dot_i (+ contains_double_dot_i 1)))) (throw (ex-info "return" {:v false}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn is_valid_email_address [is_valid_email_address_email]
  (binding [is_valid_email_address_at_idx nil is_valid_email_address_ch nil is_valid_email_address_domain nil is_valid_email_address_i nil is_valid_email_address_local_part nil] (try (do (when (not= (count_char is_valid_email_address_email "@") 1) (throw (ex-info "return" {:v false}))) (set! is_valid_email_address_at_idx 0) (set! is_valid_email_address_i 0) (loop [while_flag_1 true] (when (and while_flag_1 (< is_valid_email_address_i (count is_valid_email_address_email))) (cond (= (subs is_valid_email_address_email is_valid_email_address_i (min (+ is_valid_email_address_i 1) (count is_valid_email_address_email))) "@") (do (set! is_valid_email_address_at_idx is_valid_email_address_i) (recur false)) :else (do (set! is_valid_email_address_i (+ is_valid_email_address_i 1)) (recur while_flag_1))))) (set! is_valid_email_address_local_part (subs is_valid_email_address_email 0 (min is_valid_email_address_at_idx (count is_valid_email_address_email)))) (set! is_valid_email_address_domain (subs is_valid_email_address_email (+ is_valid_email_address_at_idx 1) (min (count is_valid_email_address_email) (count is_valid_email_address_email)))) (when (or (> (count is_valid_email_address_local_part) main_MAX_LOCAL_PART_OCTETS) (> (count is_valid_email_address_domain) main_MAX_DOMAIN_OCTETS)) (throw (ex-info "return" {:v false}))) (set! is_valid_email_address_i 0) (while (< is_valid_email_address_i (count is_valid_email_address_local_part)) (do (set! is_valid_email_address_ch (subs is_valid_email_address_local_part is_valid_email_address_i (min (+ is_valid_email_address_i 1) (count is_valid_email_address_local_part)))) (when (not (char_in is_valid_email_address_ch (str (str main_ASCII_LETTERS main_DIGITS) main_LOCAL_EXTRA))) (throw (ex-info "return" {:v false}))) (set! is_valid_email_address_i (+ is_valid_email_address_i 1)))) (when (or (or (starts_with_char is_valid_email_address_local_part ".") (ends_with_char is_valid_email_address_local_part ".")) (contains_double_dot is_valid_email_address_local_part)) (throw (ex-info "return" {:v false}))) (set! is_valid_email_address_i 0) (while (< is_valid_email_address_i (count is_valid_email_address_domain)) (do (set! is_valid_email_address_ch (subs is_valid_email_address_domain is_valid_email_address_i (min (+ is_valid_email_address_i 1) (count is_valid_email_address_domain)))) (when (not (char_in is_valid_email_address_ch (str (str main_ASCII_LETTERS main_DIGITS) main_DOMAIN_EXTRA))) (throw (ex-info "return" {:v false}))) (set! is_valid_email_address_i (+ is_valid_email_address_i 1)))) (when (or (starts_with_char is_valid_email_address_domain "-") (ends_with_char is_valid_email_address_domain ".")) (throw (ex-info "return" {:v false}))) (if (or (or (starts_with_char is_valid_email_address_domain ".") (ends_with_char is_valid_email_address_domain ".")) (contains_double_dot is_valid_email_address_domain)) false true)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_email_tests ["simple@example.com" "very.common@example.com" "disposable.style.email.with+symbol@example.com" "other-email-with-hyphen@and.subdomains.example.com" "fully-qualified-domain@example.com" "user.name+tag+sorting@example.com" "x@example.com" "example-indeed@strange-example.com" "test/test@test.com" "123456789012345678901234567890123456789012345678901234567890123@example.com" "admin@mailserver1" "example@s.example" "Abc.example.com" "A@b@c@example.com" "abc@example..com" "a(c)d,e:f;g<h>i[j\\k]l@example.com" "12345678901234567890123456789012345678901234567890123456789012345@example.com" "i.like.underscores@but_its_not_allowed_in_this_part" ""])

(def ^:dynamic main_idx 0)

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (while (< main_idx (count main_email_tests)) (do (def ^:dynamic main_email (nth main_email_tests main_idx)) (println (str (is_valid_email_address main_email))) (def main_idx (+ main_idx 1))))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
