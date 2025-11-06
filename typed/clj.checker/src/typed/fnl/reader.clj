;; MIT License
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;; 
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;; 
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;; transliterated from https://github.com/bakpakin/Fennel/blob/main/src/fennel/parser.fnl
;; additions labeled with "Typed Fennel"
(ns ^:typed.clojure typed.fnl.reader
  "Reader for Fennel source code. Converts Fennel syntax to Clojure data structures.
  
  This is a port of the Fennel parser (src/fennel/parser.fnl) to Clojure.
  It converts Fennel forms into Clojure-friendly data structures for use with 
  the type checker. The mapping is:
  
  - Fennel lists (fn ...) -> Clojure lists (fn ...)
  - Fennel vectors [a b c] -> Clojure vectors [a b c]
  - Fennel tables {:a 1 :b 2} -> Clojure maps {:a 1 :b 2}
  - Fennel symbols -> Clojure symbols
  - Fennel strings -> Clojure strings
  - Fennel numbers -> Clojure numbers/longs/doubles
  - Fennel booleans (true/false) -> Clojure booleans
  - Fennel nil -> Clojure nil
  - Fennel ... (varargs) -> symbol ...
  - Fennel :keyword -> Clojure strings (Fennel keywords are syntactic sugar for strings)
    :a -> \"a\", :a/b -> \"a/b\", ::a -> \":a\"
  
  Example:
    (read-string \"(fn demo-func [x] x)\")
    => (fn demo-func [x] x)"
  (:refer-clojure :exclude [read-string])
  (:require [clojure.string :as str]))

;; Table of delimiter bytes - (, ), [, ], {, }
;; Opener keys have closer as the value; closers keys have true as their value.
(def delims {40 41, 41 true, 91 93, 93 true, 123 125, 125 true})

;; Prefix chars substituted while reading
(def prefixes {35 :hashfn    ; #
               39 :quote      ; '
               44 :unquote    ; ,
               96 :quote})    ; `

;; Escape sequences  
(def escapes {\a "\u0007"
              \b "\b"
              \f "\f"
              \n "\n"
              \r "\r"
              \t "\t"
              \v "\u000B"
              \\ "\\"
              \" "\""
              \' "'"
              \newline "\n"})

(defn sym-char? [b]
  "Check if byte is a valid symbol character."
  (let [b (int b)]
    (and (< 32 b)
         (not (contains? delims b))
         (not= b 127)  ; backspace
         (not= b 34)   ; double quote
         (not= b 39)   ; single quote
         (not= b 126)  ; tilde
         (not= b 59)   ; semicolon
         (not= b 44)   ; comma
         (not= b 64)   ; at
         (not= b 96)))) ; backtick

(defn char-starter? [b]
  "Check if byte is a UTF-8 character starter."
  (let [b (int b)]
    (or (< b 128) (< 192 b 248))))

(defn string-stream [s]
  "Convert a string into a stream of bytes."
  (let [s (str/replace s #"^#!" ";;")  ; replace shebang with comment
        len (count s)
        index (atom 0)]
    (fn [_parser-state]
      (when (< @index len)
        (let [b (int (.charAt s @index))]
          (swap! index inc)
          b)))))

(defn parser-fn [getbyte filename options]
  "Create a parser function that returns AST nodes from a byte stream.
  
  This is the main Fennel parser ported to Clojure. It reads bytes from
  getbyte and returns Clojure data structures representing Fennel forms."
  (let [stack (atom [])
        line (atom 1)
        byteindex (atom 0)
        col (atom 0)
        prev-col (atom 0)
        lastb (atom nil)
        whitespace-since-dispatch (atom true)
        done? (atom false)
        retval (atom nil)]
    
    (letfn [(ungetb [ub]
              (when (char-starter? ub)
                (swap! col dec))
              (when (= ub 10)
                (swap! line dec)
                (reset! col @prev-col))
              (swap! byteindex dec)
              (reset! lastb ub))
            
            (getb []
              (let [r (if @lastb
                       (let [result @lastb]
                         (reset! lastb nil)
                         result)
                       (getbyte {:stack-size (count @stack)}))]
                (when r
                  (swap! byteindex inc))
                (when (and r (char-starter? r))
                  (swap! col inc))
                (when (= r 10)
                  (reset! prev-col @col)
                  (swap! line inc)
                  (reset! col 0))
                r))
            
            (whitespace? [b]
              (or (= b 32) (<= 9 b 13) (get-in options [:whitespace b])))
            
            (parse-error [msg col-adjust]
              (let [error-col (+ @col (or col-adjust -1))]
                (throw (ex-info (format "%s:%s:%s: Parse error: %s"
                                       filename (or @line "?") error-col msg)
                               {:type :parse-error
                                :filename filename
                                :line @line
                                :column error-col}))))
            
            (dispatch [v source raw]
              (reset! whitespace-since-dispatch false)
              (let [top (peek @stack)]
                (cond
                  (nil? top)
                  (do
                    (reset! retval v)
                    (reset! done? true))
                  
                  (:prefix top)
                  (let [prefix-data (peek @stack)
                        _ (swap! stack pop)
                        wrapped (list (symbol (:prefix prefix-data)) v)]
                    (dispatch wrapped source raw))
                  
                  :else
                  ;; Add value to the items of the current collection
                  (swap! stack (fn [s]
                                (update s (dec (count s)) update :items conj v))))))
            
            (badend []
              (let [closers (mapv :closer @stack)]
                (parse-error (format "expected closing delimiter%s %s"
                                    (if (= (count @stack) 1) "" "s")
                                    (apply str (map char closers)))
                            0)))
            
            (skip-whitespace [b]
              (cond
                (and b (whitespace? b))
                (do
                  (reset! whitespace-since-dispatch true)
                  (recur (getb)))
                
                (and (not b) (seq @stack))
                (do
                  (badend)
                  ;; Return first closer to continue
                  (:closer (first @stack)))
                
                :else b))
            
            (parse-comment [b contents]
              (if (and b (not= 10 b))
                (recur (getb) (conj contents (char b)))
                (do
                  (when b (ungetb 10))
                  ;; Skip comments for now
                  nil)))
            
            (open-table [b]
              (when (not @whitespace-since-dispatch)
                (parse-error (str "expected whitespace before opening delimiter "
                                 (char b))
                            nil))
              (swap! stack conj {:bytestart @byteindex
                                :closer (get delims b)
                                :filename filename
                                :line @line
                                :col (dec @col)
                                :items []}))
            
            (close-list [lst-data]
              (dispatch (apply list (:items lst-data)) nil nil))
            
            (close-sequence [tbl-data]
              (dispatch (vec (:items tbl-data)) nil nil))
            
            (close-curly-table [tbl-data]
              (let [items (:items tbl-data)]
                (when (odd? (count items))
                  (swap! byteindex dec)
                  (parse-error "expected even number of values in table literal" nil))
                ;; Handle :key value shorthand - convert :key symbols to keywords
                (let [pairs (partition 2 items)
                      processed-pairs (map (fn [[k v]]
                                            ;; If k is a symbol starting with :, convert to keyword
                                            (let [k (if (and (symbol? k)
                                                            (str/starts-with? (str k) ":"))
                                                     (keyword (subs (str k) 1))
                                                     k)]
                                              [k v]))
                                          pairs)]
                  (dispatch (into {} processed-pairs) nil nil))))
            
            (close-table [b]
              (when (empty? @stack)
                (parse-error (str "unexpected closing delimiter " (char b)) nil))
              (let [top (peek @stack)
                    _ (swap! stack pop)]
                (when (and (:closer top) (not= (:closer top) b))
                  (parse-error (str "mismatched closing delimiter " (char b)
                                   ", expected " (char (:closer top)))
                              nil))
                (cond
                  (= b 41) (close-list top)      ; )
                  (= b 93) (close-sequence top)   ; ]
                  :else (close-curly-table top)))) ; }
            
            (bitrange [codepoint low high]
              (mod (quot codepoint (bit-shift-left 1 low))
                   (bit-shift-left 1 (- high low))))
            
            (encode-utf8 [codepoint-str]
              ;; codepoint-str format is "u{hexadecimal digits}"
              (let [hex-str (subs codepoint-str 2 (dec (count codepoint-str)))]
                (try
                  (let [codepoint (Integer/parseInt hex-str 16)]
                    (cond
                      (<= 0 codepoint 0x7F)
                      (str (char codepoint))
                      
                      (<= 0x80 codepoint 0x7FF)
                      (str (char (+ 0xC0 (bitrange codepoint 6 11)))
                           (char (+ 0x80 (bitrange codepoint 0 6))))
                      
                      (<= 0x800 codepoint 0xFFFF)
                      (str (char (+ 0xE0 (bitrange codepoint 12 16)))
                           (char (+ 0x80 (bitrange codepoint 6 12)))
                           (char (+ 0x80 (bitrange codepoint 0 6))))
                      
                      (<= 0x10000 codepoint 0x10FFFF)
                      (str (char (+ 0xF0 (bitrange codepoint 18 21)))
                           (char (+ 0x80 (bitrange codepoint 12 18)))
                           (char (+ 0x80 (bitrange codepoint 6 12)))
                           (char (+ 0x80 (bitrange codepoint 0 6))))
                      
                      :else
                      (parse-error (str "utf8 value too large: " codepoint-str) nil)))
                  (catch Exception _
                    (parse-error (str "Illegal string: " codepoint-str) nil)))))
            
            (parse-string-loop [chars b state]
              (when b
                (swap! chars conj (char b)))
              (let [new-state (cond
                               (and (= state :base) (= b 92)) :backslash
                               (and (= state :base) (= b 34)) :done
                               :else :base)]
                (if (and b (not= new-state :done))
                  (recur chars (getb) new-state)
                  b)))
            
            (expand-str [s]
              "Expand escape sequences in a string."
              (loop [i 0
                     result (StringBuilder.)]
                (if (< i (count s))
                  (let [ch (.charAt s i)]
                    (if (= ch \\)
                      (if (< (inc i) (count s))
                        (let [next-ch (.charAt s (inc i))]
                          (if-let [escaped (get escapes next-ch)]
                            (do (.append result escaped)
                                (recur (+ i 2) result))
                            ;; Handle \xHH hex escapes
                            (if (= next-ch \x)
                              (if (<= (+ i 4) (count s))
                                (let [hex-str (subs s (+ i 2) (+ i 4))
                                      parsed (try
                                              (Integer/parseInt hex-str 16)
                                              (catch Exception _
                                                (parse-error "invalid hex escape" nil)))]
                                  (.append result (char parsed))
                                  (recur (+ i 4) result))
                                (parse-error "incomplete hex escape" nil))
                              ;; Handle \u{...} unicode escapes
                              (if (= next-ch \u)
                                (if-let [end-idx (str/index-of s "}" (+ i 2))]
                                  (let [unicode-str (str "u{" (subs s (+ i 3) end-idx) "}")]
                                    (.append result (encode-utf8 unicode-str))
                                    (recur (inc end-idx) result))
                                  (parse-error "incomplete unicode escape" nil))
                                ;; Handle \DDD decimal escapes
                                (if (Character/isDigit next-ch)
                                  (let [end (min (+ i 4) (count s))
                                        digit-chars (take-while #(Character/isDigit %)
                                                               (seq (subs s (inc i) end)))
                                        digit-str (apply str digit-chars)
                                        byte-val (Integer/parseInt digit-str 10)]
                                    (when (> byte-val 255)
                                      (parse-error "invalid decimal escape" nil))
                                    (.append result (char byte-val))
                                    (recur (+ i 1 (count digit-str)) result))
                                  ;; Unknown escape - just include both chars
                                  (do (.append result next-ch)
                                      (recur (+ i 2) result)))))))
                        (do (.append result ch)
                            (recur (inc i) result)))
                      (do (.append result ch)
                          (recur (inc i) result))))
                  (.toString result))))
            
            (parse-string [source]
              (when (not @whitespace-since-dispatch)
                ;; Just warn, don't error
                nil)
              (swap! stack conj {:closer 34})
              (let [chars (atom [])
                    _ (swap! chars conj \")]
                (when (not (parse-string-loop chars (getb) :base))
                  (badend))
                (swap! stack pop)
                (let [raw (apply str @chars)
                      content (subs raw 1 (dec (count raw)))]
                  (dispatch (expand-str content) source raw))))
            
            (parse-prefix [b]
              "Expand prefix byte into wrapping form."
              (swap! stack conj {:prefix (get prefixes b)
                                :filename filename
                                :line @line
                                :bytestart @byteindex
                                :col (dec @col)})
              (let [nextb (getb)
                    trailing-ws? (or (whitespace? nextb)
                                    (true? (get delims nextb)))]
                (when (and trailing-ws? (not= b 35))
                  (parse-error "invalid whitespace after quoting prefix" nil))
                (ungetb nextb)
                (when (and trailing-ws? (= b 35))
                  ;; Standalone # symbol
                  (swap! stack pop)
                  (dispatch (symbol "#") nil nil))))
            
            (parse-sym-loop [chars b]
              (if (and b (sym-char? b))
                (recur (conj chars (char b)) (getb))
                (do
                  (when b (ungetb b))
                  chars)))
            
            (parse-number [rawstr source]
              "Parse a number from a string."
              (let [trimmed (when-not (str/starts-with? rawstr "_")
                             (str/replace rawstr "_" ""))]
                (cond
                  (or (= trimmed "nan") (= trimmed "-nan"))
                  false
                  
                  (re-matches #"^\d.*" rawstr)
                  (do
                    (if-let [n (try
                                (Long/parseLong trimmed)
                                (catch Exception _
                                  (try
                                    (Double/parseDouble trimmed)
                                    (catch Exception _
                                      (parse-error (str "could not read number \"" rawstr "\"")
                                                  (- (count rawstr)))))))]
                      (dispatch n source rawstr)
                      nil)
                    true)
                  
                  :else
                  (if-let [n (try
                              (Long/parseLong trimmed)
                              (catch Exception _
                                (try
                                  (Double/parseDouble trimmed)
                                  (catch Exception _ nil))))]
                    (do (dispatch n source rawstr)
                        true)
                    false))))
            
            (check-malformed-sym [rawstr]
              "Check for malformed multi-symbols."
              (cond
                (and (str/starts-with? rawstr "~") (not= rawstr "~="))
                (parse-error "invalid character: ~" nil)
                
                (and (re-find #"[\.:][\.\:]" rawstr)
                     (not= rawstr "..")
                     (not= rawstr "$..."))
                (parse-error (str "malformed multisym: " rawstr) nil)
                
                (and (not= rawstr ":") (str/ends-with? rawstr ":"))
                (parse-error (str "malformed multisym: " rawstr) nil)
                
                (re-find #":.+[\.\:]" rawstr)
                (parse-error (str "method must be last component of multisym: " rawstr) nil)
                
                :else rawstr))
            
            (parse-sym [b]
              "Parse a symbol or literal value."
              (let [source {:bytestart @byteindex
                            :filename filename
                            :line @line
                            :col (dec @col)}
                    rawstr (apply str (parse-sym-loop [(char b)] (getb)))]
                (when (not @whitespace-since-dispatch)
                  ;; Just warn, don't error
                  nil)
                (cond
                  (= rawstr "true") (dispatch true source nil)
                  (= rawstr "false") (dispatch false source nil)
                  ;;added for Typed Fennel
                  (= rawstr "nil") (dispatch nil source nil)
                  (= rawstr "...") (dispatch (symbol "...") source nil)
                  (or (= rawstr ".inf") (= rawstr "+.inf")) (dispatch ##Inf source rawstr)
                  (= rawstr "-.inf") (dispatch ##-Inf source rawstr)
                  (or (= rawstr ".nan") (= rawstr "+.nan")) (dispatch ##NaN source rawstr)
                  (= rawstr "-.nan") (dispatch ##NaN source rawstr)
                  (str/starts-with? rawstr ":")
                  ;; In Fennel, :foo is syntactic sugar for "foo" (strings)
                  ;; :a => "a", :a/b => "a/b", ::a => ":a"
                  (dispatch (subs rawstr 1) source rawstr)
                  :else
                  (when (not (parse-number rawstr source))
                    (dispatch (symbol (check-malformed-sym rawstr)) source nil)))))
            
            (parse-loop [b]
              (cond
                (not b) nil  ; EOF
                
                (= b 59)  ; semicolon - comment
                (do (parse-comment (getb) [])
                    (recur (skip-whitespace (getb))))
                
                (number? (get delims b))  ; opener
                (do (open-table b)
                    (recur (skip-whitespace (getb))))
                
                (get delims b)  ; closer
                (do (close-table b)
                    (if @done?
                      @retval
                      (recur (skip-whitespace (getb)))))
                
                (= b 34)  ; double quote - string
                (do (parse-string {:bytestart @byteindex
                                  :filename filename
                                  :line @line
                                  :col @col})
                    (if @done?
                      @retval
                      (recur (skip-whitespace (getb)))))
                
                (get prefixes b)  ; prefix character
                (do (parse-prefix b)
                    (if @done?
                      @retval
                      (recur (skip-whitespace (getb)))))
                
                (or (sym-char? b) (= b (int \~)))  ; symbol
                (do (parse-sym b)
                    (if @done?
                      @retval
                      (recur (skip-whitespace (getb)))))
                
                :else
                (parse-error (str "invalid character: " (char b)) nil)))
            
            (parse-stream []
              (let [result (parse-loop (skip-whitespace (getb)))]
                ;; Reset state for next form
                (reset! stack [])
                (reset! whitespace-since-dispatch true)
                (reset! done? false)
                (reset! retval nil)
                result))]
      
      parse-stream)))

;; Public API

(defn read-string
  "Read a single Fennel form from a string.
  
  Returns a Clojure data structure representing the Fennel form.
  
  Example:
    (read-string \"(fn demo-func [x] x)\")
    => (fn demo-func [x] x)"
  ([s] (read-string s nil))
  ([s opts]
   (let [filename (or (:filename opts) "<string>")
         options (or opts {})
         getbyte (string-stream s)
         parse-fn (parser-fn getbyte filename options)]
     (parse-fn))))

(defn read-all
  "Read all Fennel forms from a string.
  
  Returns a sequence of Clojure data structures representing the Fennel forms.
  Stops parsing on EOF or parse error."
  ([s] (read-all s nil))
  ([s opts]
   (let [filename (or (:filename opts) "<string>")
         options (or opts {})
         getbyte (string-stream s)
         parse-fn (parser-fn getbyte filename options)]
     (loop [forms []]
       (let [result (try
                      (when-let [form (parse-fn)]
                        {:form form})
                      (catch clojure.lang.ExceptionInfo e
                        (if (= (:type (ex-data e)) :parse-error)
                          nil  ; Stop on parse error
                          (throw e))))]
         (if result
           (recur (conj forms (:form result)))
           forms))))))

(defn read-file
  "Read all Fennel forms from a file.
  
  Returns a sequence of Clojure data structures representing the Fennel forms."
  [filename]
  (read-all (slurp filename) {:filename filename}))
