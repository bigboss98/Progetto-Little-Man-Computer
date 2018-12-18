(defun one-instruction (state)
  (cond
	((is-halted-state state)
		(fail-instruction))
	((= (fetch-instruction state) 1)  ;add
		(add-instruction state))
	((= (fetch-instruction state) 2) ;sub
		(sub-instruction state))
	((= (fetch-instruction state) 3) ;store
		(store-instruction state))
	((= (fetch-instruction state) 5) ;load
		(load-instruction state))
	((= (fetch-instruction state) 6) ;branch
		(branch-instruction state))
	((= (fetch-instruction state) 7) ;branch if zero
		(branchz-instruction state))
	((= (fetch-instruction state) 8) ;branch if positive
		(branchp-instruction state))
	((= (fetch-instruction state) 9) ;input e output
			(cond ((= (mod (nth (nth 4 state) (nth 6 state)) 100) 01) ;input
					(if (null (nth 8 state))
						(fail-instruction)
						(input-instruction state)))
				  ((= (mod (nth (nth 4 state) (nth 6 state)) 100) 02) ;output
					(output-instruction state))
				  (T
					(fail-instruction))))
	((= (fetch-instruction state) 0) ;halt
		(halt-instruction state)
	)
	(T
		(fail-instruction)) ;istruzione non valida
))

(defun insertLast (lista element)
	(cond ((null lista)
		(list element))
	(T
		(append lista element))))

(defun fetch-instruction (state)
	(floor (nth (nth 4 state) (nth 6 state)) 100))
	
(defun value-instruction (state)
	(mod (nth (nth 4 state) (nth 6 state)) 100))
	
(defun add-instruction (state)
	(cond 
		((> (+ (nth 2 state) (nth (value-instruction state) (nth 6 state))) 1000)
			((lambda (acc flag pc)
				(list 'STATE 
				:acc acc 
				:pc pc 
				:mem (nth 6 state)
				:in (nth 8 state)
				:out (nth 10 state)
				:flag flag ))
				(mod (+ (nth 2 state) (nth (value-instruction state) (nth 6 state))) 1000) 'flag (1+ (nth 4 state))))
		(T 
			((lambda (acc flag pc)
				(list 'STATE 
				:acc acc 
				:pc pc 
				:mem (nth 6 state)
				:in (nth 8 state)
				:out (nth 10 state)
				:flag flag ))
				(+ (nth 2 state) (nth (value-instruction state) (nth 6 state))) 'noflag (1+ (nth 4 state)))
		)
	))

(defun sub-instruction (state)
	(cond 
		((< (- (nth 2 state) (nth (value-instruction state) (nth 6 state))) 0)
			((lambda (acc flag pc)
				(list 'STATE 
				:acc acc 
				:pc pc 
				:mem (nth 6 state)
				:in (nth 8 state)
				:out (nth 10 state)
				:flag flag ))
				(mod (- (nth 2 state) (nth (value-instruction state) (nth 6 state))) 1000) 'flag (1+ (nth 4 state))))
		(T 
			((lambda (acc flag pc)
				(list 'STATE 
				:acc acc 
				:pc pc 
				:mem (nth 6 state)
				:in (nth 8 state)
				:out (nth 10 state)
				:flag flag ))
				(- (nth 2 state) (nth (value-instruction state) (nth 6 state))) 'noflag (1+ (nth 4 state))))
		)
	)

(defun store-instruction (state)
	(setf (nth (value-instruction state) (nth 6 state)) (nth 2 state))
	((lambda (pc)
		(list 'STATE 
				:acc (nth 2 state) 
				:pc pc 
				:mem (nth 6 state)
				:in (nth 8 state)
				:out (nth 10 state)
				:flag (nth 12 state) ))
				(1+ (nth 4 state))))

(defun load-instruction (state)
	((lambda (acc pc)
		(list 'STATE 
				:acc acc 
				:pc pc 
				:mem (nth 6 state)
				:in (nth 8 state)
				:out (nth 10 state)
				:flag (nth 12 state) ))
				(nth (value-instruction state) (nth 6 state)) (1+ (nth 4 state))))

(defun branch-instruction (state)
	((lambda (pc)
		(list 'STATE 
				:acc (nth 2 state) 
				:pc pc 
				:mem (nth 6 state)
				:in (nth 8 state)
				:out (nth 10 state)
				:flag (nth 12 state) ))
				(value-instruction state)))

(defun branchz-instruction (state)
	(cond
		((and (= (nth 2 state) 0) (eq (nth 12 state) 'noflag))
			((lambda (pc)
				(list 'STATE 
					:acc (nth 2 state) 
					:pc pc 
					:mem (nth 6 state)
					:in (nth 8 state)
					:out (nth 10 state)
					:flag (nth 12 state) ))
				(value-instruction state)))
		(T state)))

(defun branchp-instruction (state)
	(cond
		((eq (nth 12 state) 'noflag)
			((lambda (pc)
				(list 'STATE 
					:acc (nth 2 state) 
					:pc pc 
					:mem (nth 6 state)
					:in (nth 8 state)
					:out (nth 10 state)
					:flag (nth 12 state) ))
				(value-instruction state)))
		(T state)))
	
(defun input-instruction(state)
	((lambda (acc pc in)
				(list 'STATE 
				:acc acc 
				:pc pc 
				:mem (nth 6 state)
				:in in
				:out (nth 10 state)
				:flag (nth 12 state) ))
				(first (nth 8 state)) (1+ (nth 4 state)) (rest (nth 8 state))))

(defun output-instruction(state)
	((lambda (pc out)
				(list 'STATE 
				:acc (nth 2 state) 
				:pc pc 
				:mem (nth 6 state)
				:in (nth 8 state)
				:out out
				:flag (nth 12 state) ))
				(1+ (nth 4 state)) (insertLast (nth 10 state) (nth 2 state))))

(defun halt-instruction(state)
	((lambda ()
				(list 'HALTED-STATE 
				:acc (nth 2 state) 
				:pc  (nth 4 state) 
				:mem (nth 6 state)
				:in (nth 8 state)
				:out (nth 10 state)
				:flag (nth 12 state) ))
				))

(defun fail-instruction ()
	nil)

(defun is-halted-state (state)
	(if (eq (nth 0 state) 'halted-state)
		T
		nil))
	
(defun execution-loop (state)
	(cond 
		((is-halted-state state)
			(nth 10 state))
		((null state)
			nil)
		(T
			(execution-loop (one-instruction state)))))
	