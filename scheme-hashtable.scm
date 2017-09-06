;;; -*- geiser-scheme-implementation: guile -*-

;;; Copyright (C) 2016 Jeandre Kruger

;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.

;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Hash a string.
(define (hash-string str size)
  (let loop ((lst (string->list str))
             (accum 0))
    (if (null? lst)
        (modulo accum size)
        (loop (cdr lst)
              (+ (char->integer (car lst))
                 (* 31 accum))))))

;;; Hash an object.
(define (hash obj size)
  (cond ((number? obj) (modulo obj size))
        ((string? obj) (hash-string obj size))
        ((symbol? obj)
         (hash-string (symbol->string obj) size))
        ((char? obj)
         (hash-string (make-string 1 obj) size))
        ((boolean? obj)
         (modulo (if obj 2 1) size))
        ((pair? obj)
         (modulo
          (+ (hash (car obj) size)
             (hash (cdr obj) size))
          size))
        (else 0)))

;;; Hash table type tag.
(define hash-table-tag (list 'hash-table))

(define (tagged-vector? obj tag)
  (and (vector? obj)
       (> (vector-length obj) 0)
       (eq? (vector-ref obj 0) tag)))

;;; Return #t if obj is a hash table.
(define (hash-table? obj)
  (tagged-vector? obj hash-table-tag))

(define (make-hash-table-aux aproc)
  (lambda args
    (let ((size (if (null? args) 101 (car args))))
      (vector hash-table-tag aproc
              (make-vector size '())))))

;;; Make a hash table using assoc.
(define make-hash-table (make-hash-table-aux assoc))
;;; Make a hash table using assq.
(define make-hash-tableq (make-hash-table-aux assq))
;;; Make a hash table using assv.
(define make-hash-tablev (make-hash-table-aux assv))

;;; Get the associative list procedure
;;; from a hash table.
(define (hash-table-aproc ht) (vector-ref ht 1))
;;; Get the vector from a hash table.
(define (hash-table-vector ht) (vector-ref ht 2))

(define (hash-table-using aproc)
  (lambda (obj)
    (and (hash-table? obj)
         (eq? (hash-table-aproc obj) aproc))))

;;; Return #t if obj is a hash table
;;; using assoc.
(define hash-table-equal? (hash-table-using assoc))
;;; Return #t if obj is a hash table
;;; using assq.
(define hash-table-eq? (hash-table-using assq))
;;; Return #T if obj is a hash table
;;; using assv.
(define hash-table-eqv? (hash-table-using assv))

;;; Set a value in a hash table.
(define (hash-table-set! ht key val)
  (let* ((vec (hash-table-vector ht))
         (hashval (hash key (vector-length vec)))
         (alist (vector-ref vec hashval))
         (pair ((hash-table-aproc ht) key alist)))
    (if pair
        (set-cdr! pair val)
        (vector-set! vec hashval
                     (cons (cons key val)
                           alist)))))

;;; Get a value from a hash table.
(define (hash-table-ref ht key)
  (let ((vec (hash-table-vector ht)))
    ((hash-table-aproc ht)
     key
     (vector-ref vec (hash key (vector-length vec))))))

(define (hash-table-pred ht)
  (let ((aproc (hash-table-aproc ht)))
    (cond ((eq? aproc assq) eq?)
          ((eq? aproc assv) eqv?)
          ((eq? aproc assoc) equal?))))

;;; Delete a key from a hash table.
(define (hash-table-delete! ht key)
  (let* ((pred? (hash-table-pred ht))
         (vec (hash-table-vector ht))
         (hashval (hash key (vector-length vec))))
    (let loop ((alist (vector-ref vec hashval))
               (accum '()))
      (cond ((null? alist)
             (vector-set! vec hashval accum))
            ((pred? (caar alist) key)
             (loop (cdr alist) accum))
            (else
             (loop (cdr alist)
                   (cons (car alist) accum)))))))

;;; Convert an associative list to
;;; a hash table.
(define (alist->hash-table-aux aproc)
  (lambda (alist)
    (let ((ht ((make-hash-table-aux aproc))))
      (for-each
       (lambda (pair)
         (hash-table-set! ht (car pair) (cdr pair)))
       alist)
      ht)))

(define alist->hash-table (alist->hash-table-aux assoc))
(define alist->hash-tableq (alist->hash-table-aux assq))
(define alist->hash-tablev (alist->hash-table-aux assv))

(define (every n lst)
  (cond ((null? lst) '())
        ((zero? n) (every (+ n 1) (cdr lst)))
        (else
         (cons (car lst)
               (every (- n 1) (cdr lst))))))

(define (zip a b)
  (map cons a b))

(define (hash-table . args)
  (alist->hash-table
   (zip (every 1 args) (every 0 args))))

(define (hash-tableq . args)
  (alist->hash-tableq
   (zip (every 1 args) (every 0 args))))

(define (hash-tablev . args)
  (alist->hash-tablev
   (zip (every 1 args) (every 0 args))))

;;; Convert a hash table to an
;;; associative list.
(define (hash-table->alist ht)
  (apply append
         (vector->list (hash-table-vector ht))))

;;; Make a copy of an associative list.
;;; The new one is backwards, but that should
;;; not matter.
(define (alist-copy alist)
  (let loop ((alist alist)
             (accum '()))
    (if (null? alist)
        accum
        (loop
         (cdr alist)
         (cons (cons (caar alist) (cdar alist)) accum)))))

;;; Make a copy of a hash table.
(define (hash-table-copy ht)
  (let* ((old-vec (hash-table-vector ht))
         (size (vector-length old-vec))
         (new-vec (make-vector size)))
    (let loop ((i 0))
      (if (= i size)
          (vector hash-table-tag (hash-table-aproc ht) new-vec)
          (begin
            (vector-set! new-vec i
                         (alist-copy (vector-ref old-vec i)))
            (loop (+ i 1)))))))

;;; Create a copy of the hash table _ht_ and set the
;;; keys to the values.
(define (hash-table-set ht . args)
  (let ((new (hash-table-copy ht)))
    (let loop ((keys (every 1 args))
               (vals (every 0 args)))
      (if (not (null? keys))
          (begin
            (hash-table-set! new (car keys) (car vals))
            (loop (cdr keys) (cdr vals)))))
    new))

;;; Create a copy of the hash table _ht_ and delete the keys.
(define (hash-table-delete ht . keys)
  (let ((new (hash-table-copy ht)))
    (let loop ((keys keys))
      (if (not (null? keys))
          (begin
            (hash-table-delete! new (car keys))
            (loop (cdr keys)))))
    new))
