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

(define olength
  (lambda (l)
    (cond
      ((null? l) 0)
      (else (+ 1 (olength (cdr l)))))))

(define pick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (car lat))
      (else (pick (sub1 n) (cdr lat))))))

(define rempick
  (lambda (n lat)
    (cond
      ((null? lat) '())
      (else (cond
              ((zero? (sub1 n)) (cdr lat))
              (else (cons (car lat)
                          (rempick (sub1 n) (cdr lat)))))))))

(define allnums
  (lambda (lat)
    (cond
      ((null? lat) '())
      (else (cond
              ((number? (car lat)) (cons (car lat)
                                         (nonums (cdr lat))))
              (else (nonums (cdr lat))))))))

(define nonums
  (lambda (lat)
    (cond
      ((null? lat) '())
      (else (cond
              ((number? (car lat)) (nonums (cdr lat)))
              (else (cons (car lat)
                          (nonums (cdr lat)))))))))

(define equan?
  (lambda (a1 a2)
    (cond
      ((and (number? a1) (number? a2)) (= a1 a2))
      ((or (number? a1) (number? a2)) #f)
      (else (eq? a1 a2)))))

(define occur
  (lambda (a lat)
    (cond
      ((null? lat) 0)
      (else (cond
              ((equan? a (car lat)) (add1 (occur a (cdr lat))))
              (else (occur a (cdr lat))))))))

(define one?
    (lambda (n)
      (equan? n 1)))

(define rember*
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((atom? (car lat)) (cond
                           ((equan? a (car lat)) (rember* a (cdr lat)))
                           (else (cons (car lat)
                                       (rember* a (cdr lat))))))
      (else (cons (rember* a (car lat))
                  (rember* a (cdr lat)))))))