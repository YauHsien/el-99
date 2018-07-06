;; Find the last element of a list.
;; (eq (my_last nil) nil)
;; (eq (my_last '(a)) 'a)
;; (eq (my_last '(a b)) 'b)
(defun my_last (list)
  (cond ((eq list nil) nil)
	((eq (cdr list) nil) (car list))
	(t (my_last (cdr list)))))

;; Find the last but one element of a list.
;; (eq (last_but_one nil) nil)
;; (eq (last_but_one '(a)) nil)
;; (eq (last_but_one '(a b)) 'a)
;; (eq (last_but_one '(a b c)) 'b)
(defun last_but_one (list)
  (cond ((eq (cdr list) nil) nil)
	((and (not (eq (cadr list) nil)) (eq (cddr list) nil)) (car list))
	(t (last_but_one (cdr list)))))

;; Find the K'th element of a list; the first element in the list is number 1.
;; (eq (element_at '(a b c) -1) nil)
;; (eq (element_at '(a b c) 0) nil)
;; (eq (element_at '(a b c) 1) 'a)
;; (eq (element_at '(a b c) 2) 'b)
;; (eq (element_at '(a b c) 3) 'c)
;; (eq (element_at '(a b c) 4) nil)
(defun element_at (list nth)
  (cond ((eq 1 nth) (car list))
	((< 1 nth) (element_at (cdr list) (- nth 1)))))

;; Find the number of elements of a list.
;; (eq (number_of_elements nil) 0)
;; (eq (number_of_elements 'a) 1)
;; (eq (number_of_elements '(a)) 1)
;; (eq (number_of_elements '(a b)) 2)
(defun number_of_elements (list &optional acc)
  (cond ((not (listp list)) 1)
	((and (eq list nil) (numberp acc)) acc)
	((and (eq list nil) (not (numberp acc))) 0)
	((not (numberp acc)) (number_of_elements (cdr list) 1))
	(t (number_of_elements (cdr list) (+ acc 1)))))

;; Reverse a list.
;; (eq (my_reverse nil) nil)
;; (eq (my_reverse 123) nil)
;; (equal (my_reverse '(a)) '(a))
;; (equal (my_reverse '(a b)) '(b a))
;; (equal (my_reverse '(a b c)) '(c b a))
(defun my_reverse (list &optional acc)
  (cond ((not (listp list)) nil)
	((eq list nil) acc)
	(t (my_reverse (cdr list) (cons (car list) acc)))))

;; Find out whether a list is a palindrome.
;; (eq (is_palindrome 'nil) t)
;; (eq (is_palindrome 'a) nil)
;; (eq (is_palindrome '(a)) t)
;; (eq (is_palindrome '(a b)) nil)
;; (eq (is_palindrome '(a b a)) t)
(defun is_palindrome (list)
  (equal (my_reverse list) list))

;; Flatten a nested list structure.
;; (eq (my_flatten nil) nil)
;; (equal (my_flatten 'a) 'a)
;; (equal (my_flatten '(a)) '(a))
;; (equal (my_flatten '((a))) '(a))
;; (equal (my_flatten '((a b))) '(a b))
;; (equal (my_flatten '(((a b) c) d)) '(a b c d))
;; (equal (my_flatten '(a (b (c d)))) '(a b c d))
(defun my_flatten (list &optional acc rest)
  (cond ((and (not (listp list)) (eq acc nil) (eq rest nil)) list)
	((and (eq list nil) (eq rest nil)) (my_reverse acc))
	((eq list nil) (my_flatten (car rest) acc (cdr rest)))
	((listp (car list)) (my_flatten (car list) acc (cons (cdr list) rest)))
	(t (my_flatten (cdr list) (cons (car list) acc) rest))))

;; Eliminate consecutive duplicates of list elements.
;; (eq (compress nil) nil)
;; (eq (compress 'a) nil) ;error (wrong-type-argument listp a)
;; (equal (compress '(a a)) '(a))
;; (equal (compress '(a a a a b c c a a d e e e e)) '(a b c a d e))
(defun compress (list &optional acc)
  (cond ((eq list nil) (my_reverse acc))
	((eq (car list) (car acc)) (compress (cdr list) acc))
	(t (compress (cdr list) (cons (car list) acc)))))

;; Pack consecutive dulplicates of list elements into sublists.
;; (eq (pack nil) nil)
;; (equal (pack 'a) nil) ;error (wrong-type-argument listp a)
;; (equal (pack '(a)) '((a)))
;; (equal (pack '(a a a a b c c a a d e e e e)) '((a a a a) (b) (c c) (a a) (d) (e e e e)))
(defun pack (list &optional acc)
  (cond ((eq list nil) (my_reverse acc))
	((listp (car acc))
	 (cond ((eq (car list) (caar acc))
		(pack (cdr list) (cons (cons (car list) (car acc)) (cdr acc))))
	       (t
		(pack (cdr list) (cons (list (car list)) acc)))))
	(t (pack (cdr list) (cons (list (car list)) acc)))))

;; Run-length encoding of a list.
;; (eq (encode nil) nil)
;; (equal (encode 'a) nil) ;error (wrong-type-argument listp a)
;; (equal (encode '(a)) '((1 . a)))
;; (equal (encode '(a a a a b c c a a d e e e e)) '((4 . a) (1 . b) (2 . c) (2 . a) (1 . d) (4 . e)))
(defun encode (list &optional acc1 acc2)
  (cond ((and (eq list nil) (eq acc1 nil)) (my_reverse acc2))
	((not (eq list nil)) (encode nil (pack list) acc2))
	(t (encode list (cdr acc1) (cons (cons (number_of_elements (car acc1)) (caar acc1)) acc2)))))
  
;; Modified run-length encoding.
;; (eq (encode_modified nil) nil)
;; (equal (encode_modified 'a) nil) ;error (wrong-type-argument listp a)
;; (equal (encode_modified '(a)) '((1 . a)))
;; (equal (encode_modified '(a a a a b c c a a d e e e e)) '((4 . a) b (2 . c) (2 . a) d (4 . e)))
(defun encode_modified (list &optional acc1 acc2)
  (cond ((and (eq list nil) (eq acc1 nil)) (my_reverse acc2))
	((not (eq list nil)) (encode_modified nil (pack list) acc2))
	((eq (cdar acc1) nil) (encode_modified list (cdr acc1) (cons (caar acc1) acc2)))
	(t (encode_modified list (cdr acc1) (cons (cons (number_of_elements (car acc1)) (caar acc1)) acc2)))))

;; Decode a run-length encoded list.
;; (eq (decode nil) nil)
;; (equal (decode 'a) nil) ;error (wrong-type-argument listp a)
;; (equal (decode '(a)) '(a))
;; (equal (decode '((2 . a))) '(a a))
;; (equal (decode '((4 . a) b (2 . c) (2 . a) d (4 . e))) '(a a a a b c c a a d e e e e))
(defun decode (list &optional acc)
  (cond ((eq list nil) (my_reverse acc))
	((listp (car list))
	 (cond ((= (caar list) 0)
		(decode (cdr list) acc))
	       (t
		(decode (cons (cons (- (caar list) 1) (cdar list)) (cdr list)) (cons (cdar list) acc)))))
	(t (decode (cdr list) (cons (car list) acc)))))

;; Run-length encoding of a list (direct solution).
;; (eq (encode_direct nil) nil)
;; (equal (encode_direct 'a) nil) ;error (wrong-type-argument listp a)
;; (equal (encode_direct '(a)) '(a))
;; (equal (encode_direct '(a a)) '((2 . a)))
;; (equal (encode_direct '(a a a a b c c a a d e e e e)) '((4 . a) b (2 . c) (2 . a) d (4 . e)))
(defun encode_direct (list &optional acc1 acc2)
  (cond ((eq list nil)
	 (cond ((eq acc1 nil)
		(my_reverse acc2))
	       (t
		(my_reverse (cons acc1 acc2)))))
	((eq acc1 nil)
	 (encode_direct (cdr list) (car list) acc2))
	((listp acc1)
	 (cond ((eq (car list) (cdr acc1))
		(encode_direct (cdr list) (cons (+ (car acc1) 1) (cdr acc1)) acc2))
	       (t
		(encode_direct (cdr list) (car list) (cons acc1 acc2)))))
	(t
	 (cond ((eq (car list) acc1)
		(encode_direct (cdr list) (cons 2 acc1) acc2))
	       (t
		(encode_direct (cdr list) (car list) (cons acc1 acc2)))))))

;; Duplicate the elements of a list.
;; (equal (dupli '(a b c)) '(a a b b c c))
;; Duplicate the elements of a list a given number of times.
;; (equal (dupli '(a b c) 3) '(a a a b b b c c c))
(defun dupli (list &optional times acc1 acc2)
  (cond ((eq times nil)
	 (cond ((eq list nil) (my_reverse acc1))
	       (t (dupli (cdr list) times (cons (car list) (cons (car list) acc1))))))
	(t
	 (cond ((and (eq list nil) (eq acc1 nil))
		(my_reverse (append acc1 acc2)))
	       ((eq acc1 nil)
		(dupli (cdr list) times (list (car list)) acc2))
	       (t
		(cond ((>= (number_of_elements acc1) times)
		       (dupli list times nil (append acc1 acc2)))
		      (t
		       (dupli list times (cons (car acc1) acc1) acc2))))))))

