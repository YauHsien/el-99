% Find the last element of a list.
% (eq (my_last nil) nil)
% (eq (my_last '(a)) 'a)
% (eq (my_last '(a b)) 'b)
(defun my_last (list)
  (cond ((eq list nil) nil)
	((eq (cdr list) nil) (car list))
	(t (my_last (cdr list)))))

% Find the last but one element of a list.
% (eq (last_but_one nil) nil)
% (eq (last_but_one '(a)) nil)
% (eq (last_but_one '(a b)) 'a)
% (eq (last_but_one '(a b c)) 'b)
(defun last_but_one (list)
  (cond ((eq (cdr list) nil) nil)
	((and (not (eq (cadr list) nil)) (eq (cddr list) nil)) (car list))
	(t (last_but_one (cdr list)))))

% Find the K'th element of a list; the first element in the list is number 1.
% (eq (element_at '(a b c) -1) nil)
% (eq (element_at '(a b c) 0) nil)
% (eq (element_at '(a b c) 1) 'a)
% (eq (element_at '(a b c) 2) 'b)
% (eq (element_at '(a b c) 3) 'c)
% (eq (element_at '(a b c) 4) nil)
(defun element_at (list nth)
  (cond ((eq 1 nth) (car list))
	((< 1 nth) (element_at (cdr list) (- nth 1)))))

% Find the number of elements of a list.
% (eq (number_of_elements nil) 0)
% (eq (number_of_elements 'a) 1)
% (eq (number_of_elements '(a)) 1)
% (eq (number_of_elements '(a b)) 2)
(defun number_of_elements (list &optional acc)
  (cond ((not (listp list)) 1)
	((and (eq list nil) (numberp acc)) acc)
	((and (eq list nil) (not (numberp acc))) 0)
	((not (numberp acc)) (number_of_elements (cdr list) 1))
	(t (number_of_elements (cdr list) (+ acc 1)))))
