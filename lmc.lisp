(defun one-instruction (state)
  (cond 
  ((= (floor (nth (nth 4 state) (nth 6 state)) 100) 1)  ;add
		(setf (nth 2 state)(+ (mod (nth (nth 4 state) (nth 6 state)) 100) (nth 2 state)))
		(cond ((> (nth 2 state) 1000)
			(setf (nth 2 state)(mod (nth 2 state) 1000))
			(setf (nth 12 state) T)
			(T (setf (nth 12 state) nil)))))
	((= (floor (nth (nth 4 state) (nth 6 state)) 100) 2) ;sub
		(setf (nth 2 state) (- (nth 2 state) (mod (nth (nth 4 state) (nth 6 state)) 100)))
		(cond ((< (nth 2 state) 0)
			(setf (nth 2 state)(mod (nth 2 state) 1000))
			(setf (nth 12 state) T))
			(T (setf (nth 12 state) nil))))
	((= (floor (nth (nth 4 state) (nth 6 state)) 100) 3) ;store
		(setf (nth (mod (nth (nth 4 state) (nth 6 state)) 100) (nth 6 state)) (nth 2 state)))
	((= (floor (nth (nth 4 state) (nth 6 state)) 100) 5) ;load
		(setf (nth 2 state) (nth (mod (nth (nth 4 state) (nth 6 state)) 100) (nth 6 state))))
	((= (floor (nth (nth 4 state) (nth 6 state)) 100) 6) ;branch
		(setf (nth 4 state) (mod (nth (nth 4 state) (nth 6 state)) 100)))
	((= (floor (nth (nth 4 state) (nth 6 state)) 100) 7) ;branch if zero
		(cond ((and (= (nth 2 state) 0)(null (nth 12 state)))
			(setf (nth 4 state) (mod (nth (nth 4 state) (nth 6 state)) 100)))))
	((= (floor (nth (nth 4 state) (nth 6 state)) 100) 8) ;branch if zero
		(cond ((null (nth 12 state))
			(setf (nth 4 state) (mod (nth (nth 4 state) (nth 6 state)) 100)))))
	((= (floor (nth (nth 4 state) (nth 6 state)) 100) 9) ;(nth 8 state) e (nth 10 state)put
		(cond ((= (floor (nth (nth 4 state) (nth 6 state)) 100) 9)
			(cond ((= (mod (nth (nth 4 state) (nth 6 state)) 100) 01) ;(nth 8 state)
				(setf (nth 2 state) (first (nth 8 state)))
				(setf (nth 8 state) (rest (nth 8 state)))))
			(cond ((= (mod (nth (nth 4 state) (nth 6 state)) 100) 02) ;(nth 10 state)put
				(setf (nth 10 state) (insertLast (nth 10 state) (nth 2 state)))))
				)))
	((= (floor (nth (nth 4 state) (nth 6 state)) 100) 0) ;halt
		(return 0))
	)
	(setf (nth 4 state) (1+ (nth 4 state)))
	state
)

(defun insertLast (lista element)
	(cond ((null lista)
		(list element))
	(T
		(append lista element))))