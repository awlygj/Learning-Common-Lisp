;Chapter 2
;;1
;;;(a)
14

;;;(b)
(1 5)

;;;(c)
7

;;;(d)
(nil 3)

;;2
(cons 'a (cons 'b (cons 'c nil)))
(cons 'a (cons 'b '(c)))
(cons 'a '(b c))

;;3
(defun our-fourth (lst)
	(car (cdr (cdr (cdr lst)))))

;;4
(defun greater (x y)
	(if (> x y)
		x
		y))

;;5
;;;(a)
;;;;判断传入的参数是否是一个包含有nil元素的列表, 如果是返回t, 不是返回nil.
(enigma '(1 nil 2))
T
(enigma '(nil 1))
T
(enigma '(2 1))
NIL
(enigma nil)
NIL

;;;(b)
;;;;查找原子x是否在列表y中, 如果找到了则返回第一次出现的位置(0索引), 没找到返回nil.
(mystery 'b '(e d c b a))
3
(mystery 'c '(e d c b a))
2
(mystery 'f '(e d c b a))
NIL

;;6
;;;(a)
(car (car (cdr '(a (b c) d))))

;;;(b)
(or 13 (/ 1 0))

;;;(c)
(apply #'list 1 nil)


;;7
(defun contains-list? (lst)
	(if (null lst)
		nil
		(let ((obj (car lst)))
			(if (listp obj)
				obj
				(contains-list? (cdr lst))))))

;;8
;;;(a)
(defun print-dot-1 (i)
	(if (and (numberp i) (> i 0))
		(progn (format t "*")
			(print-dot-1 (- i 1)))
		'done))

(defun print-dot-2 (i)
	(and (numberp i)
		(do ((j 1 (+ j 1)))
			((> j i) 'DONE)
			(format t "*"))))

;;;(b)
(defun find-times-1 (a lst)
	(if (null lst)
		0
		(if (eql a (car lst))
			(+ 1 (find-times-1 a (cdr lst)))
			(find-times-1 a (cdr lst)))))

(defun find-times-2 (a lst)
	(let ((i 0))
		(dolist (obj lst)
			(if (eql a obj)
				(setf i (+ i 1))))
		i))

;;9
;;;(a)
(defun summit (lst)
      (apply #'+ (remove nil lst)))

;;;(b)
(defun summit (lst)
	(if (null lst)
		0
		(let ((x (car lst)))
			(if (null x)
				(summit (cdr lst))
				(+ x (summit (cdr lst)))))))

;Chapter 3
;;1
;;;(a)
(cons 'a (cons 'b (cons (cons 'c (cons 'd nil)) nil)))
;;;(b)
(cons 'a (cons (cons 'b (cons (cons 'c (cons (cons 'd nil) nil)) nil)) nil))
;;;(c)
(cons (cons (cons 'a (cons 'b nil)) (cons 'c nil)) (cons 'd nil))
;;;(d)
(cons 'a (cons (cons 'b 'c) (cons 'd nil)))

;;2
(defun new-union (set1 set2)
	(or (and (null set1) set2)
		(and (null set2) set1)
		(new-union
			(let ((etl (car set2)))
				(if (member etl set1)
					set1
					(append set1 (list etl))))
			(cdr set2))))

;;3
(defun occurrences (x)
	(let ((y))
		(dolist (obj x)
			(let ((z (assoc obj y)))
				(if z
					(setf (cdr z) (+ 1 (cdr z)))
					(setf y (append y (list (cons obj 1)))))))
		(sort y #'> :key #'cdr)))

;;4
;;;一般情况下， member 使用 eql 来比较对象。
;;;加上 :key #'equal 就对了, 用 equal 去比较元素就对了.

;;5
> (pos+ '(7 5 1 4))
(7 6 3 7)

;;;a)
(defun pos+ (lst)
	(let ((len (length lst))
		  (letl (car (last lst))))
		(if (eql len 1)
			lst
			(append (pos+ (reverse (cdr (reverse lst)))) (list (+ letl len -1))))))
;;;b)
(defun pos+ (lst)
	(do ((i 0 (+ i 1))
		 (k lst (cdr k))
		 (j nil (append j (list (+ i (car k))))))
		((null k) j)))

;;;c)
(defun pos+ (lst)
	(let ((i -1))
		(mapcar #'(lambda (x) (+ x (setf i (+ i 1)))) lst)))

;;6
;;;a)
(defun cons (x y)
	(let ((i '(nil.nil)))
		(setf (cdr i) x)
		(setf (car i) y)
		i))

;;;b)
(defun list (&rest x) x)
	
;;;c)
(defun length (x)
	(if (null x)
		0
		(+ 1 length (car x))))

;;;d)
(defun member (x y)
	(if y
		if (eql (cdr x) x)
			y
			(member x (car y))))

;;7)
(defun n-elts (elt n)
  (if (> n 1)
      (cons n elt)
      elt))

;;8)
;;;1)
(defun showdots (x)
	(if (consp x)
		(progn (format t "(")
			   (if (atom (car x))
					(format t "~A" (car x))
					(showdots (car x)))
			   (format t " . ")
			   (if (atom (cdr x))
					(format t "~A" (cdr x))
					(showdots (cdr x)))
			   (format t ")"))
		(format t "~A" x)))
 ;;;2)
(defun showdots-rec (x)
	(if (consp x)
		(format nil "(~A . ~A)"
			(showdots-rec (car x))
			(showdots-rec (cdr x)))
		x))

(defun showdots (x)
	(format t "~A" (showdots-rec x)))

;;9
(defun longest-path (start end net)
  (bfs end (list (list start)) net nil))

(defun bfs (end queue net findPath)
  (if (null queue)
      (reverse findPath)
      (let ((path (car queue)))
        (let ((node (car path)))
              (bfs end
                   (append (cdr queue)
                           (new-paths path node net))
                   net
				   (if (eql node end) path findPath)
				   )))))

(defun new-paths (path node net)
	(let ((newPaths))
		(dolist (nextNode (cdr (assoc node net)))
			(or (member nextNode path)
				(push (cons nextNode path) newPaths)))
	newPaths))

;Chapter 4
;;1
(defun quarter-turn (square-array)
	(let ((dims (array-dimensions square-array)))
		(let ((dim (car dims))
			  (out-array (make-array dims)))
		(do ((i 0 (+ i 1))) ((= i dim) out-array)
			(do ((j 0 (+ j 1))) ((= j dim))
				(setf (aref out-array j (- dim i 1)) (aref square-array i j)))))))

;;2
;;;a)
(defun our-copy-list (lst)
	(reduce #'cons lst :from-end t :initial-value nil))

;;;b)
(defun our-reverse (lst)
	(reduce #'(lambda (x y) (cons y x)) lst :initial-value nil))

;;3
(defstruct (my-tree-node
				(:print-function (lambda (node stream depth)
										(progn (format stream "#<")
											   (dolist (x (my-tree-node-cols node))
													(format stream "~A, " x))
											   (format stream "~A~A" #\Backspace #\Backspace) 
											   (format stream ">")))))
										
	cols
	child1
	child2
	child3)

(defun copy-my-tree (mt)
	(if (null mt)
		nil
		(let ((n (copy-my-tree-node mt)))
			(setf (my-tree-node-child1 n) (copy-my-tree (my-tree-node-child1 mt)))
			(setf (my-tree-node-child2 n) (copy-my-tree (my-tree-node-child2 mt)))
			(setf (my-tree-node-child3 n) (copy-my-tree (my-tree-node-child3 mt)))
			n)))

(defun find-my-tree (elt mt)
	(if (null mt)
		nil
		(or (find elt (my-tree-node-cols mt))
		    (find-my-tree elt (my-tree-node-child1 mt))
		    (find-my-tree elt (my-tree-node-child2 mt))
		    (find-my-tree elt (my-tree-node-child3 mt)))))

;;4
(defun bst-to-list (bst)
	(let ((lst nil))
		(bst-traverse #'(lambda (x) (setf lst (cons x lst))) bst)
		lst))

;;5

;;6
(defun ht->al (ht)
	(let ((al nil))
		(maphash #'(lambda (k v)
					(setf al (cons (cons k v) al)))
				ht)
		al))

(defun al->ht (al)
	(let ((ht (make-hash-table)))
		(dolist (elt al ht)
			(setf (gethash (car elt) ht) (cdr elt)))))

;Chapter 5
;;1
;;;a)
((lambda (x) (cons x x)) (car y))

;;;b)
((lambda (w)
	((lambda (y) (cons w y)) (+ w z)))  
	(car x))

;;2
(defun mystery (x y)
	(cond ((null y) nil)
		  ((eql (car y) x) 0)
		  (t (let ((z (mystery x (cdr y))))
                (and z (+ z 1))))))

;;3
(defun sq (x)
	(if (and (integerp x) (< 0 x 6))
		x
		(* x x)))
		
;;4
(defun month-num (m y)
	(+ (case (- m 1)
		(( 0)   0)
		(( 1)  31)
		(( 2)  59)
		(( 3)  90)
		(( 4) 120)
		(( 5) 151)
		(( 6) 181)
		(( 7) 212)
		(( 8) 243)
		(( 9) 273)
		((10) 304)
		((11) 334)
		((12) 365))
	   (if (and (> m 2) (leap? y)) 1 0)))

;;5
(defun precedes (x v)
	(let ((n))
		(do ((i 1 (+ i 1)))
			((>= i (length v)) n)
			(if (eql x (aref v i))
					(pushnew (aref v (- i 1)) n)))))

(defun precedes (x v)
	(if (> (length v) 1)
		(if (eql x (aref v 1))
			(adjoin (aref v 0)
					(precedes x (subseq v 1)))
			(precedes x (subseq v 1)))))

;;6
(defun intersperse (obj lst)
	(let ((n))
		(dolist (x lst (reverse (cdr n)))
			(progn (push x n)
				   (push obj n)))))
				   
(defun intersperse (obj lst)
	(and lst
		 (if (cdr lst)
			(cons (car lst)
				(cons obj (intersperse obj (cdr lst))))
			(cons (car lst) (intersperse obj (cdr lst))))))				   

;;7
;;;a)
(defun suc (number)
	(and (cdr number)
		(if (= 1 (abs (- (first number) (second number))))
			(if (cddr number)
				(suc (cdr number))
				t))))
				
;;;b)
(defun suc (number)
	(if (cdr number)
		(do ((x number (cdr x))
			 (y (cdr number) (cdr y)))
			((not (and x y (= 1 (abs (- (car x) (car y))))))
			 (and x (null y))))))

;;;c)
(defun suc (number)
	(if (cdr number)
		(block nil
			(mapc #'(lambda (x y)
						(if (/= 1 (abs (- x y)))
							(return nil)))
					number
					(cdr number))
				t)))

;;8
(defun max&min (v)
	(if (> (length v) 1)
		(multiple-value-bind
			(x y) (max&min (subseq v 1))
			(values (max (svref v 0) x)
				   (min (svref v 0) y)))
		(values (svref v 0) (svref v 0))))
		
;;9
;;;a)
(defun shortest-path (start end net)
	(if (eql start end)
		(list start)
		(catch 'found
			(bfs end (list (list start)) net))))

(defun bfs (end queue net)
  (if (null queue)
      nil
      (let* ((path (car queue))
             (node (car path)))       
			(bfs end
                 (append (cdr queue)
                         (new-paths end path node net))
                  net))))

(defun new-paths (end path node net)
  (mapcar #'(lambda (n)
				(let ((x (cons n path)))
					(if (eql n end)
						(throw 'found (reverse x))
						x)))              
          (cdr (assoc node net))))

;;;b)
(defun shortest-path (start end net)
	(if (eql start end)
		(list start)
		(bfs end (list (list start)) net)))

(defun bfs (end queue net)
  (if (null queue)
      nil
      (let* ((path (car queue))
             (node (car path))
			 (new-paths))
			(dolist (new-node (cdr (assoc node net)) 
						(bfs end (append (cdr queue) new-paths) net))
				(let ((new-path (cons new-node path)))
					(if (eql new-node end)
						(return (reverse new-path))
						(push new-path new-paths)))))))

;Chapter 6
;;1
(defun tokens (str &key (test #'constituent) (start 0))
  (let ((p1 (position-if test str :start start)))
	(if p1
		(let ((p2 (position-if #'(lambda (c)
								   (not (funcall test c)))
							   str :start p1)))
		  (cons (subseq str p1 p2)
				(if p2
					(tokens str :test test :start p2)
					nil)))
		nil)))

;;2
(defun bin-search (obj vec &key (key #'identity) (test #'eql) (start 0) end)
  (let ((len (length vec)))
    (if (or (null end) (> end len))
		(setf end len))
    (and (not (zerop end))
		 (> (- end start) 0)
         (finder obj vec start (- end 1) key test))))

(defun finder (obj vec start end key test)
  (let* ((range (- end start)))
    (if (zerop range)
		(let ((obj_start (aref vec start)))
			(if (funcall test obj (funcall key obj_start))
				obj_start))
        (let* ((mid (+ start (round (/ range 2))))
               (obj_mid (aref vec mid))
			   (obj_mid_key (funcall key obj_mid)))
            (cond ((funcall test obj obj_mid_key) obj_mid)
			      ((and (< obj obj_mid_key) (< start mid)) (finder obj vec start (- mid 1) key test))
                  ((> obj obj_mid_key) (finder obj vec (+ mid 1) end key test)))))))

;;3
(defun identity2 (&rest x) x)

;;4
(defun most (fn lst)
  (cond ((null lst) (values nil nil))
		((null (cdr lst)) (values (car lst) (car lst)))
		(t (let* ((wins1 (car lst))
				  (max1  (funcall fn wins1))
				  (wins2)
				  (max2))
				(dolist (obj (cdr lst))
					(let ((score (funcall fn obj)))
						(cond ((> score max1) (setf wins2 wins1
													max2  max1
													wins1 obj
													max1  score))
							  ((or (null wins2) (> score max2)) (setf wins2 obj
																	  max2  score)))))
				(values wins1 wins2)))))

;;5
(defun remove-if (fn lst)
	(filter #'(lambda (x) (and (not (funcall fn x)) x)) lst))
	
;;6
(let ((greatest))
	(defun greatest-number (number)
		(if (or (null greatest) (> number greatest))
			(setf greatest number)
			greatest)))

;;7
(let ((prev))
	(defun greater-number (number)
		(let ((i prev))
			(setf prev number)
			(and i (> number i)))))

;;8
(let ((seen (make-hash-table)))
	(defun frugal (x)
		(or (gethash x seen)
			(setf (gethash x seen) (expensive x)))))

;;9
(defun our-apply (fn &rest lst)
	(let ((*print-base* 8))
		(apply fn (reduce #'cons lst :from-end t))))


;Chapter 7
;;1
(defun get-line-list (file)
        (with-open-file (str file :direction :input)
                (do ((line (read-line str nil 'EOF)
                           (read-line str nil 'EOF))
                     (lst nil (cons line lst)))
                    ((eql line 'EOF) (nreverse lst)))))

;;2
(defun get-exp-list (file)
        (with-open-file (str file :direction :input)
                (do ((e (read str nil 'the-end-of-file)
                        (read str nil 'the-end-of-file))
                     (lst nil (cons e lst)))
                    ((eql e 'the-end-of-file) (nreverse lst)))))

;;3
(defun remove-comment (file-in file-out)
        (with-open-file (str-in file-in :direction :input)
        (with-open-file (str-out file-out :direction :output :if-exists :supersede)
                (do ((line (read-line str-in nil 'EOF)
                           (read-line str-in nil 'EOF)))
                    ((eql line 'EOF))
                    (let ((find (position #\% line)))
                        (format str-out "~A~%"
                                (if find
                                    (subseq line 0 find)
                                    line)))))))

;;4
(defun print-a2d-float (arr)
        (let ((line (first (array-dimensions arr)))
              (col  (second (array-dimensions arr))))
             (dotimes (l line)
                (dotimes (c col)
                        (format t "~10,2,,,F" (aref arr l c))
                        (if (= c (- col 1))
                            (terpri)
                            (format t "|"))))))

;;5
(defun stream-subst (old new in out)
  (let* ((pos 0)
         (len (length old))
         (buf (new-buf len))
         (from-buf nil))
    (do ((c (read-char in nil :eof)
            (or (setf from-buf (buf-next buf))
                (read-char in nil :eof))))
        ((eql c :eof))
      (cond ((or (char= #\+ (char old pos)) (char= c (char old pos)))
             (incf pos)
             (cond ((= pos len)            ; 3
                    (princ new out)
                    (setf pos 0)
                    (buf-clear buf))
                   ((not from-buf)         ; 2
                    (buf-insert c buf))))
            ((zerop pos)                   ; 1
             (princ c out)
             (when from-buf
               (buf-pop buf)
               (buf-reset buf)))
            (t                             ; 4
             (unless from-buf
               (buf-insert c buf))
             (princ (buf-pop buf) out)
             (buf-reset buf)
             (setf pos 0))))
    (buf-flush buf out)))

;;6
(defun parse-pattern-str (str &optional (pos 0) (stat nil))
        (and (< pos (length str))
             (let ((c (char str pos)))
                  (if stat
                      (cons (case c
                                ((#\d) 'digit)
                                ((#\c) 'char)
                                ((#\a) 'all)
                                (otherwise c))
                            (parse-pattern-str str (incf pos) nil))
                      (if (char= #\% c)
                          (parse-pattern-str str (incf pos) t)
                          (cons c (parse-pattern-str str (incf pos) nil)))))))

(defun stream-subst (old new in out)
        (let* ((pattern (parse-pattern-str old))
               (pos pattern)
               (len (length old))
               (buf (new-buf len))
               (from-buf nil))
              (labels ((char-pattern-matchp (c)
                                (let ((pat (car pos)))
                                     (case pat
                                           ((digit) (digit-char-p c))
                                           ((char) (alpha-char-p c))
                                           ((all) t)
                                           (otherwise (char= c pat)))))
                       (pos->zero () (setf pos pattern))
                       (pos+1 () (setf pos (cdr pos)))
                       (pos-endp () (null pos))
                       (pos-zerop () (eql pos pattern)))
                      (do ((c (read-char in nil :eof)
                              (or (setf from-buf (buf-next buf))
                                  (read-char in nil :eof))))
                          ((eql c :eof) (buf-flush buf out))
                        (cond ((char-pattern-matchp c)
                                        (pos+1)
                                        (cond ((pos-endp)            ; 3
                                                        (princ new out)
                                                        (pos->zero)
                                                        (buf-clear buf))
                                              ((not from-buf)         ; 2
                                                        (buf-insert c buf))))
                              ((pos-zerop)                   ; 1
                                        (princ c out)
                                        (when from-buf
                                                (buf-pop buf)
                                                (buf-reset buf)))
                              (t                             ; 4
                                        (unless from-buf
                                                (buf-insert c buf))
                                        (princ (buf-pop buf) out)
                                        (buf-reset buf)
                                        (pos->zero)))))))


;Chapter 8
;;1 可能, 这两个符号在不同的包里面.

;;2
; string "FOO": 3 byte
; symbol "FOO": (x64)   (x86)
;         name:   3       3   byte
;      package:   8       4   byte
;        value:   8       4   byte
;     function:   8       4   byte
;        plist:   8       4   byte
;        total:  35      19   byte

;;3
;;可能是预防: 包名称的字符串里可能会包含大小写和空白字符或者其他特殊字符等.
;;在使用包名称(修饰符)时候, 不注意就会引发错误.

;;4
;;;ring.lisp
(defpackage "RING"
            (:use "COMMON-LISP")
            (:nicknames "RING")
            (:export "NEW-BUF"
                     "BUF-INSERT"
                     "BUF-POP"
                     "BUF-NEXT"
                     "BUF-RESET"
                     "BUF-CLEAR"
                     "BUF-FLUSH"))

(in-package ring)

; 7.1 ...

;;;file.lisp
(defpackage "FILE"
            (:use "COMMON-LISP" "RING")
            (:nicknames "FILE")
            (:export "FILE-SUBST"))

(in-package file)

; 7.2 ...

;;5
(defparameter *words* (make-hash-table :size 10000))

(defconstant maxword 100)

(defun check-generate-text-p (file)
        (with-open-file (str file :direction :input)
                (let ((buffer (make-string maxword))
                      (pos 0))
                     (do ((c (read-char str nil :eof)
                             (read-char str nil :eof)))
                         ((eql c :eof) t)
                         (if (or (alpha-char-p c) (char= c #\'))
                             (progn (setf (aref buffer pos) c)
                                    (incf pos))
                             (progn (unless (zerop pos)
                                            (if (not (check
                                                        (intern (string-downcase
                                                        (subseq buffer 0 pos)))))
                                                (return))
                                            (setf pos 0))
                                    (let ((p (punc c)))
                                         (if (and p (not (check p)))
                                             (return)))))))))

(let ((prev `|.|))
        (defun check (symb)
                (prog1 (if (assoc symb (gethash prev *words*))
                                t
                                nil)
                (setf prev symb))))

(defun punc (c)
  (case c
    (#\. '|.|) (#\, '|,|) (#\; '|;|)
    (#\! '|!|) (#\? '|?|) ))


;;6
(defparameter *words-next* (make-hash-table :size 10000))
(defparameter *words-prev* (make-hash-table :size 10000))

(defconstant maxword 100)

(defun read-text (pathname)
  (with-open-file (s pathname :direction :input)
    (let ((buffer (make-string maxword))
          (pos 0))
      (do ((c (read-char s nil :eof)
              (read-char s nil :eof)))
          ((eql c :eof))
        (if (or (alpha-char-p c) (char= c #\'))
            (progn
              (setf (aref buffer pos) c)
              (incf pos))
            (progn
              (unless (zerop pos)
                (see (intern (string-downcase
                               (subseq buffer 0 pos))))
                (setf pos 0))
              (let ((p (punc c)))
                (if p (see p)))))))))

(defun punc (c)
  (case c
    (#\. '|.|) (#\, '|,|) (#\; '|;|)
    (#\! '|!|) (#\? '|?|) ))

(let ((prev `|.|))
  (defun see (symb)
    (let ((pair (assoc symb (gethash prev *words-next*))))
      (if (null pair)
          (push (cons symb 1) (gethash prev *words-next*))
          (incf (cdr pair))))
    (let ((pair (assoc prev (gethash symb *words-prev*))))
      (if (null pair)
          (push (cons prev 1) (gethash symb *words-prev*))
          (incf (cdr pair))))
    (setf prev symb)))

(defun generate-text<-word (word)
        (if (and (gethash word *words-next*)
                 (gethash word *words-prev*))
            (progn (generate-text-prev<-word word)
                   (generate-text-next<-word word)
                   t)
            (format t "The word \"~A\" isn't likeilhood." word)))

(defun generate-text-next<-word (word)
        (if (eql word '|.|)
            (terpri)
            (let ((next (random-text word t)))
                 (format t "~A " next)
                 (generate-text-next<-word next))))

(defun generate-text-prev<-word (word)
        (if (not (eql word '|.|))
            (let ((prev (random-text word nil)))
                 (generate-text-prev<-word prev)
                 (format t "~A " prev))))

(defun random-text (word next&prev)
        (let* ((choices (gethash word (if next&prev *words-next* *words-prev*)))
               (i (random (reduce #'+ choices :key #'cdr))))
              (dolist (pair choices)
                      (if (minusp (decf i (cdr pair)))
                          (return (car pair))))))


;Chapter 9
;;1
(defun nondecreasing (lst) (not (apply #'> lst)))

;;2
(defun coins(cents)
        (let* ((coin25 (floor cents 25))
               (coin-25 (mod cents 25))
               (coin10 (floor coin-25 10))
               (coin-10 (mod coin-25 10))
               (coin5 (floor coin-10 5))
               (coin-5 (mod coin-10 5)))
              (values coin25 coin10 coin5 coin-5)))

;;3
(defun top10singer-10years ()
        (let (out)
                (dotimes (i 10 out)
                        (let ((which_win (1- (random 3))))
                                (push (list (- 10 i) 
                                            (+ 5 which_win)
                                            (+ 5 (- which_win)))
                                      out)))))
;yes, the judges can select the real best singer. 

;;4
(defun segments-intersect (x1 y1 x2 y2 x3 y3 x4 y4)
        (let* ((dx1 (- x2 x1))
               (dy1 (- y2 y1))
               (dx2 (- x4 x3))
               (dy2 (- y4 y3))
               (dx3 (- x3 x1))
               (dy3 (- y3 y1))
               (d (- (* dx1 dy2) (* dx2 dy1))))
              (unless (zerop d)
                      (let ((k1 (/ (- (* dx3 dy2) (* dx2 dy3)) d))
                            (k2 (/ (- (* dx3 dy1) (* dx1 dy3)) d)))
                           (if (and (<= 0 k1 1) (<= 0 k2 1))
                                (cons (+ x1 (* dx1 k1)) (+ y1 (* dy1 k1))))))))

;;5
(defun get-fn-root (f min max epsilon)
        (let (i0)
             (do* ((i (+ min epsilon) (+ i epsilon))
                   (root (funcall f i) (funcall f i)))
                  ((>= i max) i0)
                  (if (< (abs root) epsilon)
                      (push i i0)))))

;;6
(defun horner (x &rest parms)
        (labels ((rec (parms acc)
                      (if parms
                          (rec (cdr parms) (+ (* acc x) (car parms)))
                          acc)))
                (rec parms 0)))

;;7
(log (- most-positive-fixnum most-negative-fixnum) 2)
;sbcl: 63 bit

;;8
most-positive-short-float
most-positive-single-float
most-positive-double-float
most-positive-long-float
;sbcl: 2 types

;Chapter 10
;;1
;;;a
`(,z ,x Z)

;;;b
`(X ,y ,@z)

;;;c
`((,@z ,x) Z)

;;2
(defmacro our-if (test expr then)
        `(cond (,test ,expr)
               (t ,then)))

;;3
(defmacro nth-expr (n &rest exprs)
     `(case ,n
            ,@(let ((i 0))
                   (mapcar #'(lambda (expr) `(,(incf i) ,expr))
                           exprs))))

;;4
(defmacro ntimes-rc (n &rest body)
        (let ((fn (gensym))
              (i (gensym)))
        `(labels ((,fn (,i)
                       (when (> ,i 0)
                           ,@body 
                           (,fn (decf ,i)))))
                 (,fn ,n))))

;5
(defmacro n-of (n expr)
        (let ((fn (gensym))
              (i (gensym)))
             `(labels ((,fn (,i)
                            (and (> ,i 0)
                                 (cons ,expr (,fn (decf ,i))))))
                      (,fn ,n))))

;6
(defmacro retain (parms &rest body)
        `((lambda ,parms ,@body) ,@parms))

;7
;如果lst是表达式, 而且有副作用, 其副作用会重复执行并影响表达式的结果.
(defmacro push-err (obj lst)
  `(setf ,lst (cons ,obj ,lst)))

(let* ((lst '(A B C))
       (i -1))
      (push 'D (nth (incf i) lst))
      lst)

(let* ((lst '(A B C))
       (i -1))
      (push-err 'D (nth (incf i) lst))
      lst)

;8
(define-modify-macro our-double () (lambda (x) (* 2)))

;Chapter 11
;;1
(defclass rectangle ()
  ((height :accessor rectangle-height
           :initform 1
           :initarg :height)
   (width :accessor  rectangle-width
          :initform 1
          :initarg :width)))

(defclass circle ()
  ((radius :accessor circle-radius
           :initform 1
           :initarg :height)))

(defmethod area ((x rectangle))
  (* (rectangle-height x) 
     (rectangle-width x)))

(defmethod area ((x circle))
  (* pi (expt (circle-radius x) 2)))

(let ((r (make-instance 'rectangle
                        :height 2
                        :width 3)))
     (area r))

;;2
(defun sq (x) (* x x))

(defun mag (x y z)
  (sqrt (+ (sq x) (sq y) (sq z))))

(defun unit-vector (x y z)
  (let ((d (mag x y z)))
    (values (/ x d) (/ y d) (/ z d))))

(defun minroot (a b c)
  (if (zerop a)
      (/ (- c) b)
      (let ((disc (- (sq b) (* 4 a c))))
        (unless (minusp disc)
          (let ((discrt (sqrt disc)))
            (min (/ (+ (- b) discrt) (* 2 a))
                 (/ (- (- b) discrt) (* 2 a))))))))

(defparameter *world* nil)

(defclass point ()
        ((x :accessor x :initarg :x)
         (y :accessor y :initarg :y)
         (z :accessor z :initarg :z)))

(defclass surface ()
        ((color :accessor surface-color :initarg :color)))

(defclass sphere (surface)
  ((radius :accessor sphere-radius :initarg :radius)
   (center :accessor sphere-center :initarg :center)))

(defun defsphere (x y z r c)
  (let ((s (make-instance 'sphere
             :radius r
             :center (make-instance 'point :x x :y y :z z)
             :color  c)))
    (push s *world*)
    s))

(defmethod intersect ((s sphere) pt xr yr zr)
  (let* ((c (sphere-center s))
         (n (minroot (+ (sq xr) (sq yr) (sq zr))
                     (* 2 (+ (* (- (x pt) (x c)) xr)
                             (* (- (y pt) (y c)) yr)
                             (* (- (z pt) (z c)) zr)))
                     (+ (sq (- (x pt) (x c)))
                        (sq (- (y pt) (y c)))
                        (sq (- (z pt) (z c)))
                        (- (sq (sphere-radius s)))))))
    (if n
        (make-instance 'point
                    :x  (+ (x pt) (* n xr))
                    :y  (+ (y pt) (* n yr))
                    :z  (+ (z pt) (* n zr))))))

(defmethod normal ((s sphere) pt)
  (let ((c (sphere-center s)))
    (unit-vector (- (x c) (x pt))
                 (- (y c) (y pt))
                 (- (z c) (z pt)))))

;;3
      t
      |
standard-object
|     |       |
|     |       h
|     |       |
|     |     |---|
|     e     f   g
|     |_____|___|
|           |
c           d
|___________|
      |
      a

a > c > d > e > f > g > h > standard-object > t


       t
       |
standard-object
|      |      |
|      h      |
|      |      |
|    |---|    |
e    f   g    |
|____|___|    |
     |        |
     d        c
     |________|
          |
          b

b > d > e > f > g > h > c > standard-object > t

;;4
(defun precedence (obj)
        (case obj
              (a3 '(a3 a2 a))
              (a2 '(a2 a))
              (a  '(a))
              (b3 '(b3 b2 b))
              (b2 '(b2 b))
              (b  '(b))
              (c3 '(c3 c2 c))
              (c2 '(c2 c))
              (c  '(c))))

(defun methods (gf)
        (case gf
                (fun '(fun1 fun2 fun3 fun4 fun5 fun6))))

(defun specializations (mtd)
        (case mtd
                (fun1 '(c b3 c3))
                (fun2 '(c2 b3 c3))
                (fun3 '(a2 b  c ))
                (fun4 '(t  b3 c3))
                (fun5 '(a3 b2 c2))
                (fun6 '((eql a) b c))))

(defun num-list< (lst1 lst2)
        (do ((obj1 lst1 (cdr obj1))
             (obj2 lst2 (cdr obj2)))
            ((and (null obj1) (null obj2)))
            (cond ((and (null obj1) obj2) (return t))
                  ((and (null obj2) obj1) (return nil))
                  ((< (car obj1) (car obj2)) (return t))
                  ((< (car obj2) (car obj1)) (return nil)))))

(defun get-score (mtd idx args)
        (let ((arg (nth idx args)))
             (position (nth idx (specializations mtd))
                       (append (list (list 'eql arg))
                               (precedence arg)
                               (list t))
                       :test #'equal)))

(defun most-spec-app-meth (gf args)
        (let ((pmtds (make-hash-table))
              (pmtd)
              (pmtd-score))
             (dolist (mtd (methods gf))
                (dotimes (i (length args))
                        (multiple-value-bind (pmtd-s has) (gethash mtd pmtds)
                                (if (or (not has) (and has pmtd-s))
                                    (let ((s (get-score mtd i args)))
                                         (setf (gethash mtd pmtds)
                                               (and s (push s pmtd-s))))))))

             (maphash #'(lambda (k v)
                        (format t "mtd=~S,score=~S.~%" k (reverse v)))
                      pmtds)

             (maphash #'(lambda (k v)
                                (let ((rv (reverse v)))
                                     (cond ((null rv))
                                           ((or (null pmtd-score)
                                                (num-list< rv pmtd-score))
                                            (setf pmtd k pmtd-score v)))))
                      pmtds)
             pmtd))

(most-spec-app-meth 'fun '(a3 b3 c3))

;;5
(let ((counter 0))
        (defmethod area :before (x)
                (incf counter))
        (defun get-area-call-couter () counter))


;;6
(defmethod foo (x y)
        (print 'foo))

(defmethod foo ((x (eql 'a)) y)
        (print 'sfoo)
        (if (eql y 'b) (print 'bad)))

(foo 'a 'b)

(defmethod foo ((x (eql 'a)) (y (eql 'b)))
        (print 'sfoo2)
        (if (eql y 'b) (print 'sucess)))

(foo 'a 'b)

;Chapter 12
;;1
(list (list 'A) (list 'A) (list 'A))
  car.cdr---car.cdr---car.cdr--nil
   |         |         |
car.cdr   car.cdr   car.cdr
 |   |     |   |     |   | 
 A  nil    A  nil    A  nil

(let ((elt (list 'A)))
        (list elt elt elt))

  car.cdr---car.cdr---car.cdr--nil
   |_________|_________|
             |
            car.cdr
             |   | 
             A  nil

(let ((elt (list 'A)))
        (list (list 'A) elt elt))

  car.cdr---car.cdr---car.cdr--nil
   |         |_________|
   |              |
car.cdr        car.cdr
 |   |          |   |
 A  nil         A  nil

;;2
q = car.cdr
     |   |
    nil nil

q = car.cdr--|
     |_______|
     |
  car.cdr
   |   |
   A  nil

q = car.cdr----|
     |         |
     |         |
  car.cdr---car.cdr
   |         |   |
   A         B  nil

q = car.cdr--|
     |_______|
     |
  car.cdr
   |   |
   B  nil

;;3
(defun copy-queue (old-q)
        (let ((new-q (copy-list (car old-q))))
             (cons new-q (last new-q))))

;;4
(defun push-enqueue (object queue)
        (if (null (car queue))
            (setf (cdr queue) (setf (car queue) (list object)))
            (setf (car queue) (cons object (car queue))))
        (car queue))

;;5
(defun q-move-front (object queue)
        (labels ((pe-rc (lst prev)
                        (cond ((null lst) (car queue))
                              ((eql object (car lst))
                                (if (eql lst (car queue))
                                    (car queue)
                                    (progn (setf (cdr prev) (cdr lst))
                                           (if (null (cdr lst))
                                               (setf (cdr queue) prev))
                                           (setf (cdr lst) (car queue))
                                           (setf (car queue) lst))))
                              (t (pe-rc object (cdr lst) lst)))))
                (pe-rc object (car queue) nil)))

;;6
(defun find-dc (obj lst)
        (labels ((fdc-rc (lst1)
                (cond ((or (null lst1) (eql lst1 lst)) nil)
                      ((eql obj (car lst1)) t)
                      (t (fdc-rc (cdr lst1))))))
                (if (eql obj (car lst))
                    t
                    (fdc-rc (cdr lst)))))

;;7
(defun cdr-circular-p (lst)
        (let ((points (make-hash-table)))
                (labels ((chk-rc (lst is-cdr)
                        (cond ((atom lst) nil)
                              ((gethash lst points) is-cdr)
                              (t (setf (gethash lst points) t)
                                 (or (chk-rc (cdr lst) t)
                                     (chk-rc (car lst) nil))))))
                        (chk-rc lst nil))))

;;8
(defun car-circular-p (lst)
        (let ((points (make-hash-table)))
                (labels ((chk-rc (lst is-car)
                        (cond ((atom lst) nil)
                              ((gethash lst points) is-car)
                              (t (setf (gethash lst points) t)
                                 (or (chk-rc (cdr lst) nil)
                                     (chk-rc (car lst) t))))))
                        (chk-rc lst nil))))

;Chapter 13
;;1
(defun ob-inline-1 (x) (1+ x))
(defun ob-inline-2 (x) (ob-inline-1 x))
(disassemble 'ob-inline-2)

(declaim (inline ob-inline-1))
(defun ob-inline-1 (x) (1+ x))
(defun ob-inline-2 (x) (ob-inline-1 x))
(disassemble 'ob-inline-2)
;yes.

;;2
(defun foo (x)
  (if (zerop x)
      0
      (1+ (foo (1- x)))))

(defun foo-tail (x &optional (acc 0))
  (if (zerop x)
      0
      (foo-tail (1- x) (1+ acc))))

(time (foo 10000))
;Evaluation took:
;  0.000 seconds of real time
;  0.000169 seconds of total run time (0.000169 user, 0.000000 system)
;  100.00% CPU
;  343,192 processor cycles
;  0 bytes consed
;
;10000

(time (foo-tail 10000))
;Evaluation took:
;  0.000 seconds of real time
;  0.000079 seconds of total run time (0.000079 user, 0.000000 system)
;  100.00% CPU
;  154,562 processor cycles
;  0 bytes consed
;
;0

;(/ 0.000169 0.000079)
;2.1392407

;;3
;;;a
(declaim (optimize (speed 3)
                   (compilation-speed 0)
                   (safety 0)
                   (debug 0)))

(declaim (type fixnum yzero))

(declaim (type (vector fixnum 13) month))

(declaim (inline leap? date->num month-num year-num year-days
                       num->date num-month num-year nmon date+))

(defconstant month
  #(0 31 59 90 120 151 181 212 243 273 304 334 365))

(defconstant yzero 2000)

(defun leap? (y)
  (declare (type fixnum y))
  (and (zerop (mod y 4))
       (or (zerop (mod y 400))
           (not (zerop (mod y 100))))))

(defun year-days (y)
  (declare (type fixnum y))
  (if (leap? y) 366 365))

(defun month-num (m y)
  (declare (type fixnum m y))
  (+ (the fixnum (svref month (- m 1)))
     (if (and (> m 2) (leap? y)) 1 0)))

(defun year-num (y)
  (declare (type fixnum y))
  (let ((d 0))
    (declare (type fixnum d))
    (if (>= y yzero)
        (dotimes (i (- y yzero) d)
          (declare (type fixnum i))
          (incf d (year-days (+ yzero i))))
        (dotimes (i (- yzero y) (- d))
          (declare (type fixnum i))
          (incf d (year-days (+ y i)))))))

(defun date->num (d m y)
  (declare (type fixnum d m y))
  (+ (- d 1) (month-num m y) (year-num y)))

(defun num-year (n)
  (declare (type fixnum n))
  (if (< n 0)
      (do* ((y (- yzero 1) (- y 1))
            (d (- (year-days y)) (- d (year-days y))))
           ((<= d n) (values y (- n d)))
           (declare (type fixnum y d)))
      (do* ((y yzero (+ y 1))
            (prev 0 d)
            (d (year-days y) (+ d (year-days y))))
           ((> d n) (values y (- n prev)))
           (declare (type fixnum y prev d)))))

(defun nmon (n)
  (declare (type fixnum n))
  (let ((m (position n month :test #'<)))
    (declare (type fixnum m))
    (values m (+ 1 (- n (the fixnum (svref month (- m 1))))))))

(defun num-month (n y)
  (declare (type fixnum n y))
  (if (leap? y)
      (cond ((= n 59) (values 2 29))
            ((> n 59) (nmon (- n 1)))
            (t        (nmon n)))
      (nmon n)))

(defun num->date (n)
  (declare (type fixnum n))
  (multiple-value-bind (y left) (num-year n)
    (declare (type fixnum y left))
    (multiple-value-bind (m d) (num-month left y)
      (declare (type fixnum m d))
      (values d m y))))

(defun date+ (d m y n)
  (declare (type fixnum d m y n))
  (num->date (+ (date->num d m y) n)))

(time (dotimes (o 10000) (date+ 3 2 1923 1234)))
;Evaluation took:
;  0.000 seconds of real time
;  0.002043 seconds of total run time (0.001852 user, 0.000191 system)
;  100.00% CPU
;  4,284,303 processor cycles
;  0 bytes consed

;original
;Evaluation took:
;  0.040 seconds of real time
;  0.040384 seconds of total run time (0.040054 user, 0.000330 system)
;  100.00% CPU
;  85,254,243 processor cycles
;  0 bytes consed

;;;b
(declaim (optimize (speed 3)
                   (compilation-speed 0)
                   (safety 0)
                   (debug 0)))

(declaim (inline sq mag unit-vector distance minroot
                 defsphere sphere-intersect intersect sphere-normal normal
                 first-hit color-at lambert sendray tracer))

(defstruct (point (:conc-name nil))
  (x 0e0 :type single-float)
  (y 0e0 :type single-float)
  (z 0e0 :type single-float))

(defstruct surface
        (color 0e0 :type single-float))

(defstruct (sphere (:include surface))
  (radius 0e0 :type single-float)
  center)

(defparameter *world* nil)

(defconstant eye (make-point :x 0e0 :y 0e0 :z 2e2))

(defun sq (x)
        (declare (type single-float x))
        (* x x))

(defun mag (x y z)
  (declare (type single-float x y z))
  (sqrt (+ (sq x) (sq y) (sq z))))

(defun unit-vector (x y z)
  (declare (type single-float x y z))
  (let ((d (mag x y z)))
    (declare (type single-float d))
    (values (/ x d) (/ y d) (/ z d))))

(defun distance (p1 p2)
  (mag (- (x p1) (x p2))
       (- (y p1) (y p2))
       (- (z p1) (z p2))))

(defun minroot (a b c)
  (declare (type single-float a b c))
  (if (zerop a)
      (/ (- c) b)
      (let ((disc (- (sq b) (* 4 a c))))
        (declare (type single-float disc))
        (unless (minusp disc)
          (let ((discrt (sqrt disc)))
            (min (/ (+ (- b) discrt) (* 2 a))
                 (/ (- (- b) discrt) (* 2 a))))))))

(defun defsphere (x y z r c)
  (declare (type single-float c))
  (let ((s (make-sphere
             :radius (float r)
             :center (make-point :x (float x) :y (float y) :z (float z))
             :color  c)))
    (push s *world*)
    s))

(defun sphere-intersect (s pt xr yr zr)
  (declare (type single-float xr yr zr))
  (let* ((c (sphere-center s))
         (n (minroot (+ (sq xr) (sq yr) (sq zr))
                     (* 2 (+ (* (- (x pt) (x c)) xr)
                             (* (- (y pt) (y c)) yr)
                             (* (- (z pt) (z c)) zr)))
                     (+ (sq (- (x pt) (x c)))
                        (sq (- (y pt) (y c)))
                        (sq (- (z pt) (z c)))
                        (- (sq (sphere-radius s)))))))
    (if n
        (make-point :x  (+ (x pt) (* n xr))
                    :y  (+ (y pt) (* n yr))
                    :z  (+ (z pt) (* n zr))))))

(defun intersect (s pt xr yr zr)
  (declare (type single-float xr yr zr))
  (funcall (typecase s (sphere #'sphere-intersect))
           s pt xr yr zr))

(defun sphere-normal (s pt)
  (let ((c (sphere-center s)))
    (unit-vector (- (x c) (x pt))
                 (- (y c) (y pt))
                 (- (z c) (z pt)))))

(defun normal (s pt)
  (funcall (typecase s (sphere #'sphere-normal))
           s pt))

(defun first-hit (pt xr yr zr)
  (declare (type single-float xr yr zr))
  (let (surface hit (dist 0e0))
    (declare (type single-float dist))
    (dolist (s *world*)
      (let ((h (intersect s pt xr yr zr)))
        (when h
          (let ((d (distance h pt)))
            (declare (type single-float d))
            (when (or (zerop dist) (< d dist))
              (setf surface s hit h dist d))))))
    (values surface hit)))

(defun lambert (s int xr yr zr)
  (declare (type single-float xr yr zr))
  (multiple-value-bind (xn yn zn) (normal s int)
    (declare (type single-float xn yn zn))
    (max 0e0 (+ (* xr xn) (* yr yn) (* zr zn)))))

(defun sendray (pt xr yr zr)
  (declare (type single-float xr yr zr))
  (multiple-value-bind (s int) (first-hit pt xr yr zr)
    (if s
        (* (lambert s int xr yr zr) (surface-color s))
        0e0)))

(defun color-at (x y)
  (declare (type single-float x y))
  (multiple-value-bind (xr yr zr)
                       (unit-vector (- x (x eye))
                                    (- y (y eye))
                                    (- 0e0 (z eye)))
    (declare (type single-float xr yr zr))
    (round (* (sendray eye xr yr zr) 255e0))))

(defun tracer (pathname &optional (res 1))
  (with-open-file (p pathname :direction :output)
    (format p "P2 ~A ~A 255" (* res 100) (* res 100))
    (let ((inc (/ res)))
      (do ((y -50 (+ y inc)))
          ((< (- 50 y) inc))
        (do ((x -50 (+ x inc)))
            ((< (- 50 x) inc))
          (print (color-at (float x) (float y)) p))))))

(defun ray-test (&optional (res 1))
  (setf *world* nil)
  (defsphere 0 -300 -1200 200 .8)
  (defsphere -80 -150 -1200 200 .7)
  (defsphere 70 -100 -1200 200 .9)
  (do ((x -2 (1+ x)))
      ((> x 2))
    (do ((z 2 (1+ z)))
        ((> z 7))
      (defsphere (* x 200) 300 (* z -400) 40 .75)))
  (tracer (make-pathname :name "spheres.pgm") res))

(time (ray-test))
;Evaluation took:
;  0.000 seconds of real time
;  0.002681 seconds of total run time (0.002562 user, 0.000119 system)
;  100.00% CPU
;  7,591,853 processor cycles
;  262,128 bytes consed

;original
;Evaluation took:
;  0.027 seconds of real time
;  0.025980 seconds of total run time (0.025841 user, 0.000139 system)
;  96.30% CPU
;  56,842,301 processor cycles
;  262,144 bytes consed

;;4
(defconstant min-net '((a b c) (b c) (c d)))

(defun shortest-path (start end net)
  (bfs end (list (list start)) net))

(defun bfs (end queue net)
  ;(princ queue)
  ;(terpri)
  (if (null queue)
      nil
      (let ((path (car queue)))
        (let ((node (car path)))
          (if (eql node end)
              (nreverse path)
              (bfs end
                   (nconc (cdr queue)
                           (new-paths path node net))
                   net))))))

(defun new-paths (path node net)
  (mapcar #'(lambda (n)
              (cons n path))
          (cdr (assoc node net))))

(time (shortest-path 'a 'd min-net))

;;5
(defstruct (node (:print-function
                        (lambda (n s d)
                                (declare (ignore d))
                                (format s "#<elt=~A>"
                                        (node-elt n)))))
        (elt nil)
        (l nil)
        (r nil))

(defparameter *pool* (make-array 10000 :fill-pointer t))

(dotimes (i (length *pool*))
        (setf (aref *pool* i) (make-node)))

(defun get-node (elt &optional (l nil) (r nil))
        (let ((o (if (plusp (length *pool*))
                     (vector-pop *pool*)
                     (make-node))))
             (setf (node-elt o) elt
                   (node-l   o) l
                   (node-r   o) r)
             o))

(defun delete-node (n)
        (setf (node-elt n) nil
              (node-l   n) nil
              (node-r   n) nil)
        (vector-push n *pool*))

(defun get-set-node-l (n)
        (lambda (v) (setf (node-l n) v)))

(defun get-set-node-r (n)
        (lambda (v) (setf (node-r n) v)))

(defun bst-insert (obj bst <)
        (let ((root bst))
             (labels ((rc (bst set-parent)
                        (if bst
                            (let ((elt (node-elt bst))
                                  (l   (node-l bst))
                                  (r   (node-r bst)))
                                 (cond ((funcall < obj elt) (rc l (get-set-node-l bst)))
                                       ((funcall < elt obj) (rc r (get-set-node-r bst)))))
                            (funcall set-parent (get-node obj)))))
                     (rc bst (lambda (v) (setf root v))))
             root))

(defun bst-remove (obj bst <)
        (let ((root bst))
             (labels ((remove-min (bst set-parent)
                        (let ((elt (node-elt bst))
                              (l   (node-l bst))
                              (r   (node-r bst)))
                             (if l
                                 (remove-min l (get-set-node-l bst))
                                 (progn (funcall set-parent r)
                                        (delete-node bst)
                                        elt))))
                      (remove-max (bst set-parent)
                        (let ((elt (node-elt bst))
                              (l   (node-l bst))
                              (r   (node-r bst)))
                             (if r
                                 (remove-max r (get-set-node-r bst))
                                 (progn (funcall set-parent l)
                                        (delete-node bst)
                                        elt))))
                      (rc (bst set-parent)
                        (if bst
                            (let ((elt (node-elt bst))
                                  (l   (node-l bst))
                                  (r   (node-r bst)))
                                 (cond ((funcall < obj elt) (rc l (get-set-node-l bst)))
                                       ((funcall < elt obj) (rc r (get-set-node-r bst)))
                                       ((null (and l r)) (funcall set-parent (or l r))
                                                         (delete-node bst))
                                       (t (let ((nelt (if (zerop (random 2))
                                                          (remove-max l (get-set-node-l bst))
                                                          (remove-min r (get-set-node-r bst)))))
                                               (setf (node-elt bst) nelt))))))))
                     (rc bst (lambda (v) (setf root v))))
             root))

(defun bst-traverse (fn bst &optional (level 0))
  (when bst
    (format t "level = ~A; obj = ~A.~%" level bst)
    (funcall fn bst)
    (bst-traverse fn (node-l bst) (1+ level))
    (bst-traverse fn (node-r bst) (1+ level))))

(defparameter *bst* nil)

(setf *bst* nil)
(dolist (x '(5 8 4 2 1 9 6 7 3))
    (setf *bst* (bst-insert x *bst* #'<)))
(bst-traverse #'identity *bst*)
(setf *bst* (bst-remove 6 *bst* #'<))
(bst-traverse #'identity *bst*)
