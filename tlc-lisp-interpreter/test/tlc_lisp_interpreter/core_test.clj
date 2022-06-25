(ns tlc-lisp-interpreter.core-test
  (:require [clojure.test :refer :all]
            [tlc-lisp-interpreter.core :refer :all]))

(deftest controlar-aridad-test
  (testing "Control de buena aridad"
    (is (= 3 (controlar-aridad '(a b c) 3)))
  )

  (testing "Manejo de errores"
    (is (= '(*error* too-many-args) (controlar-aridad '(a b c) 2)))
    (is (= '(*error* too-few-args) (controlar-aridad '(a b c) 4)))
  )
)

(deftest igual?-test
  (testing "Numbers"
    (is (true? (igual? 1 1)))
    (is (false? (igual? 1 2)))
  )

  (testing "Symbols"
    (is (true? (igual? 'a 'a)))
    (is (true? (igual? 'A 'A)))
    (is (true? (igual? 'A 'a)))
    (is (true? (igual? 'a 'A)))
    (is (false? (igual? 'a 'b)))
  )

  (testing "Lists"
    (is (true? (igual? '(a b c) '(A B C))))
    (is (false? (igual? '(a b c) '(A B D))))
  )

  (testing "Nil"
    (is (true? (igual? nil nil)))
    (is (true? (igual? nil 'NIL)))
    (is (true? (igual? 'NIL nil)))
    (is (true? (igual? 'NIL 'NIL)))
    (is (true? (igual? nil ())))
    (is (true? (igual? 'NIL ())))
    (is (true? (igual? () ())))
    (is (false? (igual? () '(nil))))
  )

  (testing "String"
    (is (true? (igual? "a" "a")))
    (is (false? (igual? "a" "A")))
    (is (false? (igual? 'a "a")))
    (is (false? (igual? 'a "A")))
  )
)