(define-table mop-preds (mop) (mop-table 'preds))

(defun predicted-mops ()
  (table-keys (mop-table 'preds)))

(defvar *pred-slots-table* nil)
(define-table pred-slots (pred) *pred-slots-table*)

(defvar *predictions* nil)
(define-table pos-preds (pos) *predictions*)

(defvar *sent-pos* 1)

(defun dmap-init ()
  (setf *predictions* nil)
  (setf *pred-slots-table* nil)
  (setf *sent-pos* 1))

(defun pred-filler (role pred)
  (role-filler role (pred-slots pred)))

(defun add-pred-filler (role pred value)
  (insist add-pred-filler
          (mopp value) (null (pred-filler role pred)))
  (setf (pred-slots pred)
    (cons (make-slot role value)
          (pred-slots pred)))
  pred)

(defun pred-target (pred) (first pred))
(defun pred-phrase (pred) (second pred))
(defun pred-base (pred) (third pred))
(defun pred-start (pred) (fourth pred))

(defun make-pred (phrase base slots start)
  (let ((target (get-target phrase base)))
    (insist make-pred (or (null phrase) target))
    (let ((pred (list target phrase base start)))
      (setf (pred-slots pred) slots)
      pred)))

(defun get-target (phrase base)
  (cond ((null phrase) nil)
        ((role-specifierp (car phrase))
         (get-filler (role-specifier-role (car phrase))
                     base))
        (t (car phrase))))

(defun role-specifierp (x) (listp x))
(defun role-specifier-role (x) (car x))

(defun get-triggered-preds (mop start)
  (append (mop-dynamic-predictions mop start)
          (mop-default-predictions mop)))

(defun mop-dynamic-predictions (mop start)
  (for (pred :in (pos-preds start))
       :when (refersp (pred-target pred) mop)
       :save pred))

(defun refersp (mop1 mop2)
  (or (abstp mop1 mop2) (abstp mop2 mop1)))

(defun mop-default-predictions (mop)
  (for (target :in (predicted-mops))
       :when (abstp target mop)
       :splice (for (pred :in (mop-preds target))
                    :save pred)))

(defun advance-pred (pred mop)
  (let ((phrase (pred-phrase pred)))
    (let ((new-pred (make-pred (cdr phrase)
                               (pred-base pred)
                               (pred-slots pred)
                               (or (pred-start pred)
                                   *sent-pos*))))
      (cond ((role-specifierp (car phrase))
             (add-pred-filler
              (role-specifier-role (car phrase))
              new-pred mop)))
      (cond ((null (cdr phrase))
             (pred->mop new-pred))
            (t (let ((next-pos (+ *sent-pos* 1)))
                 (format t "~&Adding ~S to ~S" new-pred (pos-preds next-pos))
                 (setf (pos-preds next-pos)
                   (cons new-pred
                         (pos-preds next-pos)))))))))

(defun pred->mop (pred)
  (let ((mop (slots->mop (pred-slots pred)
                         (list (pred-base pred))
                         t))
        (start (pred-start pred)))
    (format t "~&Activating ~S" mop)
    (for (next-pred :in (get-triggered-preds mop start))
         :do (advance-pred next-pred mop))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro defphrase (mop &rest phrase)
    `(let ((pred (make-pred ',phrase ',mop nil nil)))
       (let ((target (pred-target pred)))
         (setf (mop-preds target)
           (cons pred (mop-preds target)))
         ',phrase)))
  )

(defun dp (n)
  (display-preds (pos-preds n))
  nil)

(defun dap ()
  (for (n :in (table-keys *predictions*))
       :do (format t "~&Predictions on Position ~S" n)
       (display-preds (pos-preds n)))
  nil)

(defun ddp ()
  (for (mop :in (predicted-mops))
       :do (display-preds (mop-preds mop)))
  nil)

(defun display-preds (preds)
  (let ((i 0))
    (for (pred in preds)
         :do (format t "~&~S: ~S ==> ~S in ~S"
               (setf i (+ i 1))
               (pred-target pred)
               (car (pred-phrase pred))
               (pred-base pred)))))

(defmop m-actor (m-root))
(defmop m-economist (m-actor))
(defmop m-monetarist (m-economist))
(defmop I-m-Friedman (m-monetarist) instance)
(defmop m-supply-sider (m-economist))
(defmop I-m-Laffer (m-supply-sider) instance)

(defmop m-act (m-root))
(defmop I-m-mtrans-act (m-act) instance)

(defmop m-variable (m-root))
(defmop m-bad-var (m-variable))
(defmop m-neutral-var (m-variable))
(defmop I-m-interest-rates (m-bad-var) instance)
(defmop I-m-money-supply (m-neutral-var) instance)

(defmop m-behavior (m-root))
(defmop I-m-increase (m-behavior) instance)
(defmop I-m-decrease (m-behavior) instance)

(defmop m-state-change (m-root))

(defmop m-vtrans (m-state-change)
  (var m-variable) (beh m-behavior))

(defmop m-good-vtrans (m-vtrans)
  (var m-variable))
(defmop m-bad-vtrans (m-vtrans)
  (var m-variable))
(defmop m-neutral-vtrans (m-vtrans)
  (var m-neutral-var))

(defmop m-bad-down (m-good-vtrans)
  (var m-bad-var) (beh I-m-decrease))
(defmop m-bad-up (m-bad-vtrans)
  (var m-bad-var) (beh I-m-increase))

(defmop I-m-ms-up (m-neutral-vtrans)
  (var I-m-money-supply))

(defmop m-causal (m-root)
  (ante m-state-change) (cnsq m-state-change))
(defmop m-causal/econ (m-causal)
  (ante m-vtrans) (cnsq m-vtrans))
(defmop m-causal/mon (m-causal/econ)
  (ante I-m-ms-up) (cnsq m-bad-vtrans))

(defmop m-mobject (m-root)
  (content nil))
(defmop m-mobject/state-change (m-mobject)
  (content m-state-change))
(defmop m-arg (m-mobject)
  (content m-causal))
(defmop m-arg/econ (m-arg)
  (content m-causal/econ))
(defmop m-arg/mon (m-arg/econ)
  (content m-causal/mon))

(defmop m-mtrans (m-event)
  (action I-m-mtrans-act) (actor m-actor)
  (info m-mobject))

(defmop m-mtrans/state-change (m-mtrans)
  (info m-mobject/state-change))

(defmop m-mtrans/econ (m-mtrans)
  (actor m-economist)
  (info m-arg (content m-causal/econ)))

(defphrase I-m-Friedman Milton Friedman)
(defphrase I-m-Laffer Arthur Laffer)

(defphrase I-m-interest-rates interest rates)
(defphrase I-m-money-supply money supply)

(defphrase I-m-increase rise)
(defphrase I-m-increase increase)
(defphrase I-m-increase go up)
(defphrase I-m-decrease drop)
(defphrase I-m-decrease decrease)
(defphrase I-m-decrease go down)

(defphrase m-vtrans (var) will (beh))

(defphrase I-m-ms-up monetary explosion)

(defphrase m-causal (cnsq) as a consequence of (ante))
(defphrase m-causal (cnsq) because of (ante))
(defphrase m-causal (ante) can cause (cnsq))
(defphrase m-causal if (ante) then (cnsq))

(defphrase m-mobject/state-change (content))
(defphrase m-arg (content))

(defphrase m-mtrans (actor) says (info) period)

(defvar *sent1*)
(defvar *sent2*)

(defun dmap-demo ()
  (format t "~&----------------")
  (dmap-init)
  (setq *sent1*
    '(Milton Friedman says interest rates will rise because of the monetary explosion period))
  (format t "~&Parsing ~S" *sent1*)
  (dmap *sent1*)
  (format t "~&----------------")
  (dmap-init)
  (setq *sent2*
    '(Arthur Laffer says interest rates will drop period))
  (format t "~&Parsing ~S" *sent2*)
  (dmap *sent2*)
  nil)

(defun dmap (word-list)
  (for (word :in word-list)
       :do (format t "~&~%Reading ~S" word)
       (let ((preds (get-triggered-preds word *sent-pos*)))
         (cond (preds
                (for (pred :in preds)
                     :do (advance-pred pred word))
                (setf *sent-pos* (+ *sent-pos* 1))))))
  (dp (+ *sent-pos* 1)))