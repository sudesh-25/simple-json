(ns simple-json.core-test
  (:require [clojure.test :refer :all]
            [simple-json.core :refer :all]))

(deftest a-test
  (testing "FIXME, I fail."
    (is (= 0 1)))
  (testing "testing parse-array []"
    (is (= (parse-array "[]") [[],""])))
  (testing "number test"
    (is (= (parse-number "44.55") [44.55 ""])))
  )
