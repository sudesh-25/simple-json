(ns simple-json.core
  (:require [clojure.string :as cls]))

(def regex-strings
  {
   :inte #"^([-]?\s*[^0\D]\d*)\D*.*$"                           ;any no of w/s b/w  '- and digits'
   :doublee #"^([-]?\s*(?:0|[1-9]\d*)(?:\.[0-9]+)?(?:(?:e|e+|e-|E|E+|E-){1}[+-]?\d+)?)\D*$"          ;(?: non capturing parens)
   }
  )
(defn get-object
  "Gets the string content inside {} structure"
  [json-string]
  (if (empty? json-string)
    nil
    (let [output-object (subs json-string (inc (cls/index-of json-string "{"))  (cls/last-index-of json-string "}"))]
      (if (empty? output-object)
        nil
        output-object)
      )
    )
  )
(defn split-object
  "Splits the object on ',' "
  [object-string]
  (map clojure.string/trim (cls/split object-string #","))
  )
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
(defn parse-null
  "Parses null values to nil"
  [input-str]
  (if (= "null" (subs input-str 0 4))
    [nil (subs input-str 4)]
    input-str)
  )
(defn parse-space
  "Removes whitespaces from left of strings"
  [input-str]
  (if (= \space (first input-str))
    (cls/triml input-str)
    input-str)
  )
(defn parse-comma
  ""
  [input-str]
  (if (= \, (first input-str))
    (subs input-str 1)
    input-str)
  )

(defn parse-boolean
  "Parses the string value to bool value"
  [input-str]
    (cond
      (cls/starts-with? input-str "true") [true (subs input-str 4)]
      (cls/starts-with? input-str "false") [false (subs input-str 5)]
      :else [nil input-str]
      )
  )
(defn parse-colon
  "A value or object or array can be encountered after a colon"
  [input-str]
  (if (= \: (first input-str))
    ;parse a value complete this method
    [nil (subs input-str 1)]
    )
  )
(defn parse-literal
  "Parses null true false"
  [input-str]
  (let [[x xs] (parse-boolean input-str)]
    (if x [x xs]
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
    (if x [x y]
          (let [[a b] (parse-int input-str)]
            (if a [a b]
                  [a b])
            )
          )
    )
  )
;; to be removed later
(defn -main
  "I don't do a whole lot."
  []
  (println "Hello, World!"))
(def my-str "{\"name\":\"John\", \"age\":null, \"city\":\"New York\" \"salary\": 500.55 \"exponential\": 2E20}")