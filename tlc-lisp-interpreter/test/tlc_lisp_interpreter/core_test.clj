(ns tlc-lisp-interpreter.core-test
  (:require [clojure.test :refer :all]
            [tlc-lisp-interpreter.core :refer :all]))

(deftest a-test
  (testing "FIXME, I fail."
    (is (= 0 1))))

(deftest controlar-aridad-test
  (testing "Control de buena aridad"
    (is (= 3 (controlar-aridad '(a b c) 3)))
  )

  (testing "Manejo de errores"
    (is (= '(*error* too-many-args) (controlar-aridad '(a b c) 2)))
    (is (= '(*error* too-few-args) (controlar-aridad '(a b c) 4)))
  )
)