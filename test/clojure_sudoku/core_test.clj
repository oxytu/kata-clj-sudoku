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
    (is (= [[1 [0 1 2 3 4 5 6 7 8 9]] [[0 1 2 3 4 5 6 7 8 9] 4]] (parse-field ["1." ".4"])))))

(deftest subfield-equals?-test
  (testing "Subfield is correctly calcualted"
    (is (= true (subfield-equals? [[0 0] [2 2]] nil 1 1)))))

(deftest map-row-test
  (testing "A map-function that only applies to the first row"
    (is (= [[2 3]
            [3 4]]
           (map-row (fn [num _ _] (inc num))
                    0
                    [[1 2]
                     [3 4]])))))

(deftest filter-row-test
  (testing "A filter-function that only applies to the first row and returns even cells"
    (is (= [2]
           (filter-row even?
                    0
                    [[1 2]
                     [3 4]])))))

(deftest map-col-test
  (testing "A map-function that only applies to the second column"
    (is (= [[1 3]
            [3 5]]
           (map-col (fn [num _ _] (inc num))
                    1
                    [[1 2]
                     [3 4]])))))

(deftest map-subfield-test
  (testing "A map-function that only applies to a sub-field of the complete field"
    (is (= [[2 3 3]
            [5 6 6]
            [7 8 9]]
           (map-subfield (fn [num _ _] (inc num))
                         [[0 0] [2 2]]
                         [[1 2 3]
                          [4 5 6]
                          [7 8 9]])))))

(deftest calculate-sudoku-subfield-of-test-1
  (testing "lower bound is met"
    (is (= [[0 0] [3 3]] (calculate-sudoku-subfield-of 1 2)))))

(deftest calculate-sudoku-subfield-of-test-2
  (testing "lower bound is met"
    (is (= [[3 3] [6 6]] (calculate-sudoku-subfield-of 3 3)))))

(deftest contains-unresolved-fields?-test-1
  (testing "Returns false if no unresolved cell is in the field"
    (is (= false (contains-unresolved-fields? [[1 2] [3 4]])))))

(deftest contains-unresolved-fields?-test-2
  (testing "Returns false if no unresolved cell is in the field"
    (is (= true (contains-unresolved-fields? [[1 [1 2]] [3 4]])))))


