(ns clojure-sudoku.core-test
  (:require [clojure.test :refer :all]
            [clojure-sudoku.core :refer :all]))

; (def field (make-array Integer 9 9))


(def field ["8..6..9.5"
            "........."
            "....2.31."
            "..7318.6."
            "24.....73"
            "........."
            "..279.1.."
            "5...8..36"
            "..3......"])

(deftest parse-row-test
  (testing "Parse a row with 3 entries"
    (is (= [1 2 3] (parse-row "123")))))

(deftest parse-field-test
  (testing "A complete field can be parsed"
    (is (= [[1 2] [3 4]] (parse-field ["12" "34"])))))

(deftest parse-field-with-empty-entries-test
  (testing "A complete field that contains empty fields can be parsed"
    (is (= [[1 nil] [nil 4]] (parse-field ["1." ".4"])))))

(deftest subfield-equals?-test
  (testing "Subfield is correctly calcualted"
    (is (= true (subfield-equals? [[0 0] [2 2]] 1 1)))))

(deftest map-row-test
  (testing "A map-function that only applies to the first row"
    (is (= [[2 3] 
            [3 4]] 
           (map-row #(inc %) 
                    0
                    [[1 2]
                     [3 4]])))))

(deftest map-col-test
  (testing "A map-function that only applies to the second column"
    (is (= [[1 3] 
            [3 5]] 
           (map-col #(inc %) 
                    1
                    [[1 2]
                     [3 4]])))))

(deftest map-subfield-test
  (testing "A map-function that only applies to a sub-field of the complete field"
    (is (= [[2 3 3] 
            [5 6 6]
            [7 8 9]] 
           (map-subfield #(inc %)
                         [[0 0] [2 2]]
                        [[1 2 3]
                         [4 5 6]
                         [7 8 9]])))))