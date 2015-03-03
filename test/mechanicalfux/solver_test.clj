(ns mechanicalfux.solver-test
  (:require [clojure.test :refer :all]
            [mechanicalfux.solver :refer :all]))

(deftest get-interval-test
  (is (= 3 (get-interval 1 4)))
  (is (= 3 (get-interval 4 1))))

(deftest as-intervals-test
  (is (= [1 2 3] (as-intervals [1 2 4 7])))
  (is (= [1 2 -3] (as-intervals [1 2 4 1])))
  )

(deftest at-most-two-consecutive-leaps-test
  (is (= true (at-most-two-consecutive-leaps [1 3 5 1])))
  (is (= false (at-most-two-consecutive-leaps [2 2 2])))
  (is (= false (at-most-two-consecutive-leaps [2 2 -2])))
  (is (= true (at-most-two-consecutive-leaps [2 2 -1]))))

(deftest no-overly-long-runs-test
  (is (= true (no-overly-long-runs [1 1 1 0 1 1])))
  (is (= true (no-overly-long-runs [1 1 1 1 1])))
  (is (= true (no-overly-long-runs [1 1 1 1 2])))
  (is (= false (no-overly-long-runs [1 1 1 1 1 1])))
  (is (= false (no-overly-long-runs [-1 -1 -1 -1 -1 -1]))))
