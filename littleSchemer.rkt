#lang racket
(define (atom? x)
  (and (not (pair? x)) (not (null? x))))

(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))

(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? (car lat) a)
                (member? a (cdr lat)))))))

#;(define rember
    (lambda (a lat)
      (condr
       ((null? lat) '())
       (else (cond
               ((eq? (car lat) a) (cdr lat))
               (else (cons (car lat)
                           (rember a
                                   (cdr lat)))))))))

(define rember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) a) (cdr lat))
      (else (cons (car lat)
                  (rember a (cdr lat)))))))

(define firsts
  (lambda (l)
    (cond
      ((null? l) '())
      (else (cond
              ((atom? (car l)) (cons '() (firsts (cdr l))))
              (else (cons (car (car l)) (firsts (cdr l)))))))))

(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old) (cons old (cons new
                                           (cdr lat))))
      (else (cons (car lat)
                  (insertR new old (cdr lat)))))))

(define insertL
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old) (cons new lat))
      (else (cons (car lat)
                  (insertL new old (cdr lat)))))))

(define subst
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      (else (cond
              ((eq? (car lat) old) (cons new (cdr lat)))
              (else (cons (car lat)
                          (subst new old (cdr lat)))))))))

(define subst2
  (lambda (new o1 o2 lat)
    (cond
      ((null? lat) '())
      (else (cond
              ((or (eq? (car lat) o1) (eq? (car lat) o2))
               (cons new (cdr lat)))
              (else (cons (car lat)
                          (subst2 new o1 o2 (cdr lat)))))))))

(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      (else (cond
              ((eq? (car lat) a) (multirember a (cdr lat)))
              (else (cons (car lat)
                          (multirember a (cdr lat)))))))))

(define multiinsertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      (else (cond
              ((eq? (car lat) old) (cons old
                                         (cons new (multiinsertR new old (cdr lat)))))
              (else (cons (car lat)
                          (multiinsertR new old (cdr lat)))))))))

(define multiinsertL
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      (else (cond
              ((eq? (car lat) old) (cons new
                                         (cons old (multiinsertL new old (cdr lat)))))
              (else (cons (car lat)
                          (multiinsertL new old (cdr lat)))))))))

(define multisubst
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      (else (cond
              ((eq? (car lat) old) (cons new
                                         (multisubst new old (cdr lat))))
              (else (cons (car lat)
                          (multisubst new old (cdr lat)))))))))

#;(define o+
    (lambda (num1 num2)
      (cond
        ((zero? num1) num2)
        (else (o+ (sub1 num1) (add1 num2))))))

(define o+
  (lambda (num1 num2)
    (cond
      ((zero? num1) num2)
      (else (add1 (o+ (sub1 num1) num2))))))

(define o-
  (lambda (num1 num2)
    (cond
      ((zero? num2) num1)
      (else (sub1 (o- num1 (sub1 num2)))))))

(define x
  (lambda (n m)
    (cond
      ((zero? m) 0)
      (else (o+ n (x n (sub1 m)))))))

(define tup+
  (lambda (tup1 tup2)
    (cond
      ((null? tup1) tup2)
      ((null? tup2) tup1)
      (else (cons (o+ (car tup1) (car tup2))
                  (tup+ (cdr tup1) (cdr tup2)))))))

(define o>
  (lambda (n m)
    (cond
      ((zero? n) #f)
      ((zero? m) #t)
      (else (o> (sub1 n) (sub1 m))))))

(define o<
  (lambda (n m)
    (cond
      ((zero? m) #f)
      ((zero? n) #t)
      (else (o< (sub1 n) (sub1 m))))))

(define o=
  (lambda (n m)
    (cond
      ((or (o> n m) (o< n m)) #f)
      (else #t))))

(define oexpt
  (lambda (n m)
    (cond
      ((zero? m) 1)
      (else (* n (oexpt n (sub1 m)))))))
