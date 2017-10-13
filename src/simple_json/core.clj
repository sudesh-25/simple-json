(ns simple-json.core
  (:require [clojure.string :as cls]))

(def regex-strings
  {
   :inte #"^([-]?[1-9]\d*\.?[0]*)\D*$"                 ;#"^([-]?\s*[^0\D]\d*[\.eE]{0}\d*)\D*.*$"                           ;any no of w/s b/w  '- and digits'
   :doublee #"^([-]?\s*(?:0|[1-9]\d*)[\.eE]{1}[+-]?\d+)\D*$" ; false +ve for 5.5e5       ;(?: non capturing parens)
   }
  )
;(defn get-object
;  "Gets the string content inside {} structure"
;  [json-string]
;  (if (empty?
;        json-string)
;    nil
;    (let [output-object (subs json-string (inc (cls/index-of json-string "{"))  (cls/last-index-of json-string "}"))]
;      (if (empty? output-object)
;        nil
;        output-object)
;      )
;    )
;  )
;(defn split-object
;  "Splits the object on ',' "
;  [object-string]
;  (map clojure.string/trim (cls/split object-string #","))
;  )
(defn parse-string
  [input-str]
  (if (= \" (first input-str))
    (let [x (subs input-str 1 (cls/index-of input-str "\"" 2))]
      (if x
        [x (subs input-str (+ 2 (count x)))]
        [nil input-str])
      )
    [nil input-str]
    )
  )
(declare parse-values)
(declare parse-array)
(declare parse-object)
(declare factory-parsers)
(declare parse)

(defn parse-null
  "Parses null values to nil"
  [input-str]
  (if (nil? input-str)
    nil
    (if (cls/starts-with? input-str "null")
      (subs input-str 4)
      nil)
    )
  )
(def esc-list '(\space \backspace \newline \formfeed \tab \] \}))
(defn parse-space
  "Removes whitespaces from left of strings"
  [input-str]
  (if (empty?
        input-str)
    input-str
    (if (some
          #(= (char (first input-str))  %)
          esc-list)
      (parse-space (subs input-str 1))
      input-str)
    )

  )
(defn parse-comma
  "Ignores ',' & returns remaining string"
  [input-str]
  (if (empty?
        input-str)
    nil
    (if (= \, (first input-str))
      (subs input-str 1)
      nil))
  )

(defn parse-boolean
  "Parses the string value to bool value"
  [input-str]
  (if (nil? input-str)
    nil
    (cond
      (cls/starts-with? input-str "true") [true (subs input-str 4)]
      (cls/starts-with? input-str "false") [false (subs input-str 5)]
      :else [nil input-str]
      )
    )
  )

(defn parse-literal
  "Parses null true false"
  [input-str]
  (let [[x xs] (parse-boolean input-str)]
    (if x
      [x xs]
          (let [[y ys] (parse-null input-str)]
            [y ys])
          )
    )
  )
(defn parse-int
  "Parses the string to int"
  [input-str]
  (let [[ss result] (re-find (:inte regex-strings) input-str)]
    (if result
      [(Integer/parseInt result) (subs input-str (count result))]
      [nil input-str]
      )
    )
  )
(defn parse-double
  "Parses double value in string"
  [input-str]
  (let [[ss result] (re-find (:doublee regex-strings) input-str)]
    (if result
      [(Double/parseDouble result) (subs input-str (count result))]
      [nil input-str]
      )
    )
  )

(defn parse-number
  "Parses a string to it's semantic data type
  Todo implement for exponential
  "
  [input-str]
  (let [[x y] (parse-double input-str)]
    (if x
      [x y]
          (let [[a b] (parse-int input-str)]
            (if a [a b]
                  [a b])
            )
          )
    )
  )

(defn parse-colon
  "A value or object or array can be encountered after a colon"
  [input-str]
  (if (empty?
        input-str)
    nil
    (if (= \: (first input-str))
      (subs input-str 1)
      nil
      )
    )
  )

(defn parse-array
  [input-str]
  (if (= (first input-str) \[)
    (let [[x y] (parse (subs input-str 1))]
      (if (nil? x)
        [nil input-str]
        [[x] y])
      )
    )
  )

(defn parse-object
  ""
  [input-str]
  (if (= (first input-str) \{)
    (let [[key rem] (parse (subs input-str 1))]
      (let [[value remain] (parse rem)]
        [(hash-map key value) remain]
        )
      )
    [nil input-str])
  )

(def factory-parsers (list parse-boolean parse-number parse-string  parse-array  parse-object))

(defn parse-values
  "Tries all parsers & return when a parser can parse the value"
  [[p & parsers] input-str]
  (if (or (empty? input-str) (nil? input-str))
    [nil nil]
    (let [[result rem] (p input-str)]
      (if (not (nil? result))
        [result rem]
        (parse-values parsers rem))
      )
    )
  )
(defn parse
  ""
  [input-str]
  (let [x (parse-space input-str)]
    (let [y (parse-comma x)]
      (if y
        (parse y)
        (let [z (parse-null x)]
          (if z
            (parse z)
            (let [col (parse-colon x)]
              (if col
                (parse col)
                (parse-values factory-parsers x)
                )
              )
            )
          )
        )
      )
    )
  )

;; to be removed later
(defn -main
  "I don't do a whole lot."
  []
  (def my-str "{\"name\":\"John\", \"age\":null, \"city\":\"New York\" \"salary\": 500.55 \"exponential\": 2E20}")
  (def test-str "{\"name\": 5 , \"kk\":\"js\"}")
  (def test-colon ": 5 ")
  (println "Hello, World!")
  ;(parse "false")
  ;(println (parse-array "[\"hello\"]"))
  ;(println (parse "}"))
  (println (parse-object test-str))
  )
