(terpri)
(prin3 "EXISTENCIA DE UN ELEMENTO ESCALAR EN UNA LISTA:") (terpri)

(prin3 "> (DE EXISTE (A L)") (terpri)
(prin3 "    (COND") (terpri)
(prin3 "      ((NULL L) NIL)") (terpri)
(prin3 "      ((NOT (LISTP (FIRST L))) (OR (EQUAL A (FIRST L)) (EXISTE A (REST L))))") (terpri)
(prin3 "      (T (OR (EXISTE A (FIRST L)) (EXISTE A (REST L))))))") (terpri)

(PRIN3
(de EXISTE (A L)
  (COND
    ((NULL L) NIL)
    ((NOT (listp (FIRST L))) (OR (EQUAL A (FIRST L)) (EXISTE A (REST L))))
    (T (OR (EXISTE A (FIRST L)) (EXISTE A (REST L))))))
) (TERPRI)

(prin3 "> (existe 'c '(a ((b) ((d c) a) e f)))") (terpri)
(prin3 (existe 'c '(a ((b) ((d c) a) e f)))) (terpri)

(prin3 "> (existe 'g '(a ((b) ((d c) a) e f)))") (terpri)
(prin3 (existe 'g '(a ((b) ((d c) a) e f)))) (terpri)

(terpri)
(prin3 "REDUCIR [REDUCE UNA LISTA APLICANDO DE A PARES UNA FUNCION DADA]:") (terpri)

(prin3 "> (de REDUCIR (F L)") (terpri)
(prin3 "    (IF (null (rest L))") (terpri)
(prin3 "        (first L)") (terpri)
(prin3 "        (F (first L) (REDUCIR F (rest L)))))") (terpri)

(prin3
(de REDUCIR (F L)
  (IF (null (rest L))
      (first L)
      (F (first L) (REDUCIR F (rest L)))))
) (terpri)

(prin3 "> (reducir (lambda (x y) (if (gt x 0) (cons x y) y)) '(5 0 2 -1 4 6 0 8 ()))") (terpri)
(prin3 (reducir (lambda (x y) (if (gt x 0) (cons x y) y)) '(5 0 2 -1 4 6 0 8 ()))) (terpri)

(terpri)
(prin3 "ELIMINACION DE LOS ELEMENTOS REPETIDOS EN UNA LISTA SIMPLE:") (terpri)

(prin3 "> (de eliminar-repetidos (li)") (terpri)
(prin3 "    (reverse (reducir (lambda (x y) (if (existe x y) y (cons x y))) (reverse (cons () li)))))") (terpri)

(prin3
(de eliminar-repetidos (li)
  (reverse (reducir (lambda (x y) (if (existe x y) y (cons x y))) (reverse (cons () li)))))
) (terpri)

(prin3 "> (eliminar-repetidos '(a b c d e f g d c h b i j))") (terpri)
(prin3 (eliminar-repetidos '(a b c d e f g d c h b i j))) (terpri)

(terpri) (prin3 "Type a letter or a digit and press Enter... ") (read)
