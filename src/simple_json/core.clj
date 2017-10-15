(ns simple-json.core
  (:require [clojure.string :as cls])
  (require clojure.pprint))

(def regex-strings
  {
   :inte #"^(-?[1-9][0-9]*).*?$"         ;fails for "1,\nas\"" any no of w/s b/w  '- and digits'
   :doublee #"^([-]?\s*(?:0|[1-9]\d*)\.?\d+(?:[eE][+-]?\d+)?).*?$" ; false +ve for 5.5e5       ;(?: non capturing parens)
   }
  )
(defn parse-string
  [input-str]
  (if (= \" (first input-str))
    (let [x (subs input-str 1 (cls/index-of input-str "\"" 2))]
      (if x
        [x (subs input-str (+ 2 (count x)))]
        [nil input-str]))
    [nil input-str]))

(defn parse-null
  "Parses null values to nil"
  [input-str]
  (if (nil? input-str)
    [nil nil]
    (if (cls/starts-with? input-str "null")
      ["null" (subs input-str 4)]
      [nil input-str])))

(def esc-list '(\space \backspace \newline \formfeed \tab \return))
(defn parse-space
  "Removes whitespaces from left of strings & some ignore symbols"
  [input-str]
  (if (empty?
        input-str)
    input-str
    (if (some
          #(= (char (first input-str))  %)
          esc-list)
      (parse-space (subs input-str 1))
      input-str)))

(defn parse-sp-symbol
  "parses , ] } : symbols"
  [input-str]
  (if (empty? input-str)
    [nil nil]
    (if (some #(= (first input-str) %) '(\, \] \} \:))
      [(first input-str) (subs input-str 1)]
      [nil input-str])))
(defn parse-boolean
  "Parses the string value to bool value"
  [input-str]
  (if (nil? input-str)
    nil
    (cond
      (cls/starts-with? input-str "true") [true (subs input-str 4)]
      (cls/starts-with? input-str "false") [false (subs input-str 5)]
      :else [nil input-str])))

(defn parse-int
  "Parses the string to int"
  [input-str]
  (let [[ss result] (re-find (:inte regex-strings) input-str)]
    (if result
      [(Integer/parseInt (cls/replace result " " "")) (subs input-str (count result))]
      [nil input-str])))

(defn parse-double
  "Parses double value in string"
  [input-str]
  (let [[ss result] (re-find (:doublee regex-strings) input-str)]
    (if result
      [(Double/parseDouble (cls/replace result " " "")) (subs input-str (count result))]
      [nil input-str])))

(defn parse-number
  "Parses given string for numeric types"
  [input-str]
  (let [[x y] (parse-double input-str)]
    (if x
      [x y]
      (let [[a b] (parse-int input-str)]
        (if a [a b]
              [a b])))))

(declare parse-values)
(declare parse-array)
(declare parse-object)
(declare factory-parsers)
(declare parse)

(defn parse-array
  "Parses array structure of Json"
  ([input-str]
   (if (or (nil? input-str) (empty? input-str))
     [nil input-str]
     (if (= (first input-str) \[)
       (parse-array (subs input-str 1) [])
       [nil input-str])))
  ([input-str arr]
   (let [[x y] (parse input-str)]
     (if (= x \])
       [arr y]
       (if (= x \,)
         (parse-array y arr)
         (parse-array y (conj arr (if (= x "null") nil x))))))))

(defn parse-object
  "parse & returns a json object"
  ([input-str]
   (if (or (nil? input-str) (empty? input-str))
     [nil input-str]
     (if (= (first input-str) \{)
       (parse-object (subs input-str 1) {})
       [nil input-str])))
  ([input-str hmap]
   (let [[x y] (parse input-str)]
     (if (= x \})
       [hmap y]
       (if (= x \,)
         (parse-object y hmap)
         (if (= x \:)
           (parse y)
           (let [[value rem] (parse-object y hmap)]
             (parse-object rem (conj hmap {x (if (= value "null") nil value)})))))))))

(def factory-parsers (list parse-boolean parse-number parse-string  parse-array  parse-object))

(defn parse-values
  "Tries all parsers & return when a parser can parse the value"
  [[p & parsers] input-str]
  (if (or (empty? input-str) (nil? input-str))
    [nil nil]
    (let [[result rem] (p input-str)]
      (if (not (nil? result))
        [result rem]
        (parse-values parsers rem)))))

(defn parse
  "Parses space sp-chars, null"
  [input-str]
  (let [x (parse-space input-str)]
    (let [[res rem] (parse-sp-symbol x)]
      (if (not (nil? res))
        [res rem]
        (let [[ress remm] (parse-null rem)]
          (if (not (nil? ress))
            [ress remm]
            (parse-values factory-parsers remm)))))))
(defn json-parse
  "Takes json string as argument & returns the parsed value"
  [input]
  (get (parse (cls/replace input "\n" "")) 0)
  )
(defn json-parse-file
  "Enter file path name to parse json file"
  [filename]
  (json-parse (slurp filename)))
(defn pprint
  "Pretty prints the parsed json"
  [content]
  (clojure.pprint/pprint content))