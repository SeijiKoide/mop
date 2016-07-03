(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-insist-forms (fnname exps)
    (and (not (null exps))
         (cons `(or ,(car exps)
                    (error "~S failed in ~S"
                      ',(car exps) ',fnname))
               (make-insist-forms fnname (cdr exps)))))
  
  (defmacro insist (fnname &rest exps)
    `(and ,@(make-insist-forms fnname exps)))
  )

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro define-table (fn vars place)
    (let ((key (car vars))
          (set-fn (gentemp "set-fn."))
          (val (gentemp "val.")))
      `(progn (defun ,fn (,key) (getf ,place ,key))
         (defun ,set-fn (,key ,val)
           (setf (getf ,place ,key) ,val))
         (defsetf ,fn ,set-fn)
         ',fn)))
  )

(defun delete-key (table key)
  (remf table key) table)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *for-keys* nil)
  (define-table for-key (key) *for-keys*)
  
  (defun for-var-forms (l)
    (and l (listp (car l))
         (cons (car l) (for-var-forms (cdr l)))))
  
  (defun for-body (l)
    (and l (or (and (for-key (car l)) l)
               (for-body (cdr l)))))
  
  (defun for-expander (var-forms when-form body-forms)
    (insist for
            (not (null var-forms))
            (not (null body-forms)))
    (let ((vars (mapcar #'car var-forms))
          (lists (mapcar #'(lambda (var-form)
                             (caddr var-form))
                   var-forms))
          (mapfn-body (funcall (for-key (car body-forms))
                               when-form
                               `(progn ,@(cdr body-forms)))))
      `(,(car mapfn-body)
          #'(lambda ,vars ,(cadr mapfn-body))
          ,@lists)))
  
  (defmacro for (&rest for-clauses)
    (let ((when-part (member ':when for-clauses)))
      (for-expander (for-var-forms for-clauses)
                    (and when-part (cadr when-part))
                    (for-body for-clauses))))
  
  (defmacro define-for-key (key vars mapfn body)
    `(progn (setf (for-key ',key)
              #'(lambda ,vars (list ,mapfn ,body)))
       ',key))
  
  (define-for-key :always (test body)
    'every
    (cond (test `(or (not ,test) ,body)) (t body)))
  
  (define-for-key :do (test body)
    'mapc
    (cond (test `(and ,test ,body)) (t body)))
  
  (define-for-key :filter (test body)
    'mapcan
    (let ((fbody `(let ((x ,body)) (and x (list x)))))
      (cond (test `(and ,test ,fbody)) (t fbody))))
  
  (define-for-key :first (test body)
    'some
    (cond (test `(and ,test ,body)) (t body)))
  
  (define-for-key :save (test body)
    (cond (test 'mapcan) (t 'mapcar))
    (cond (test `(and ,test (list ,body)))
          (t body)))
  
  (define-for-key :splice (test body)
    'mapcan
    `(copy-list
      ,(cond (test `(and ,test ,body)) (t body))))
  )

(defun table-keys (table)
  (and table
       (cons (car table)
             (table-keys (cddr table)))))

(defvar *mop-tables* nil)
(define-table mop-table (table-name) *mop-tables*)

(define-table mop-absts (mop) (mop-table 'absts))
(define-table mop-all-absts (mop) (mop-table 'all-absts))
(define-table mop-specs (mop) (mop-table 'specs))
(define-table mop-slots (mop) (mop-table 'slots))
(define-table mop-type (mop) (mop-table 'type))

(defun mopp (x)
  (or (numberp x) (and (symbolp x) (mop-type x))))

(defun instance-mopp (x)
  (and (mopp x)
       (or (numberp x)
           (eql (mop-type x) 'instance))))

(defun abst-mopp (x)
  (and (mopp x)
       (eql (mop-type x) 'mop)))

(defun abstp (abst spec)
  (or (eql abst spec)
      (member abst (mop-all-absts spec))))

(defun patternp (x) (abstp 'm-pattern x))

(defun groupp (x) (abstp 'm-group x))

(defun slot-role (slot) (car slot))
(defun slot-filler (slot) (cadr slot))
(defun make-slot (role mop) (list role mop))

(defun role-slot (role x)
  (insist role-slot
          (or (mopp x) (listp x)))
  (assoc role
         (cond ((mopp x) (mop-slots x))
               (t x))))

(defun role-filler (role x)
  (slot-filler (role-slot role x)))

(defun add-role-filler (role mop filler)
  (insist add-role-filler
          (mopp mop) (null (role-filler role mop)))
  (format t "~&~S:~S <= ~S" mop role filler)
  (setf (mop-slots mop)
    (cons (make-slot role filler)
          (mop-slots mop)))
  filler)

(defun link-abst (spec abst)
  (insist link-abst (abst-mopp abst) (mopp spec)
          (not (abstp spec abst)))
  (cond ((not (abstp abst spec))
         (setf (mop-absts spec)
           (cons abst (mop-absts spec)))
         (setf (mop-specs abst)
           (cons spec (mop-specs abst)))
         (redo-all-absts spec)))
  spec)

(defun unlink-abst (spec abst)
  (cond ((abstp abst spec)
         (setf (mop-absts spec)
           (remove abst (mop-absts spec)))
         (setf (mop-specs abst)
           (remove spec (mop-specs abst)))
         (redo-all-absts spec)))
  spec)

(defun calc-all-absts (mop)
  (remove-duplicates
   (cons mop (for (abst :in (mop-absts mop))
                  :splice (mop-all-absts abst)))))

(defun redo-all-absts (mop)
  (setf (mop-all-absts mop) (calc-all-absts mop))
  (for (spec :in (mop-specs mop))
       :do (redo-all-absts spec)))

(defun satisfiedp (constraint filler slots)
  (cond ((null constraint))
        ((patternp constraint)
         (funcall (inherit-filler 'abst-fn constraint)
                  constraint filler slots))
        ((abstp constraint filler))
        ((instance-mopp constraint) (null filler))
        (filler (slots-abstp constraint filler))
        (t nil)))

(defun slots-abstp (mop slots)
  (and (abst-mopp mop)
       (not (null (mop-slots mop)))
       (for (slot :in (mop-slots mop))
            :always (satisfiedp (slot-filler slot)
                                (get-filler (slot-role slot) slots)
                                slots))))

(defun new-mop (name absts type slots)
  (insist new-mop
          (symbolp name)
          (for (abst :in absts) :always (mopp abst)))
;;;  (and (or (symbolp name) (error "~S failed in ~S" '(symbolp name) 'new-mop))
;;;       (or (for (abst :in absts) :always (mopp abst))
;;;           (error "~S failed in ~S" '(for (abst :in absts) :always (mopp abst)) 'new-mop)))
  (or type (setf type (calc-type absts slots)))
  (or name (setf name (spec-name absts type)))
  (setf (mop-type name) type)
  (and slots (setf (mop-slots name) slots))
  (for (abst :in absts) :do (link-abst name abst))
  name)

(defun calc-type (absts slots)
  (or (for (abst :in absts)
           :when (patternp abst)
           :first 'mop)
      (and (null slots) 'mop)
      (for (slot :in slots)
           :when (not (instance-mopp (slot-filler slot)))
           :first 'mop)
      'instance))

(defun spec-name (absts type)
  (gentemp (format nil (cond ((eql type 'mop) "~S.")
                             (t "I-~S."))
             (car absts))))

(defun clear-memory ()
  (setf *mop-tables* nil)
  (new-mop 'm-root nil 'mop nil)
  (setf (mop-all-absts 'm-root)
    (calc-all-absts 'm-root))
  'm-root)

(defun all-mops ()
  (table-keys (mop-table 'type)))

(defun remove-mop (name)
  (for (abst :in (mop-absts name))
       :do (unlink-abst name abst))
  (for (table-name :in (table-keys *mop-tables*))
       :do (setf (mop-table table-name)
             (delete-key (mop-table table-name)
                         name))))

(defun inherit-filler (role mop)
  (for (abst :in (mop-all-absts mop))
       :first (role-filler role abst)))

(defun get-filler (role mop)
  (or (role-filler role mop)
      (let ((filler (inherit-filler role mop)))
        (and filler
             (or (and (instance-mopp filler) filler)
                 (and (abstp 'm-function filler) filler)
                 (let ((fn (get-filler 'calc-fn filler)))
                   (and fn
                        (let ((new-filler (funcall fn filler mop)))
                          (and new-filler
                               (add-role-filler role mop new-filler))))))))))

(defun path-filler (path mop)
  (and (for (role :in path)
            :always (setf mop (get-filler role mop)))
       mop))

(defun mop-includesp (mop1 mop2)
  (and (eql (mop-type mop1) (mop-type mop2))
       (for (slot :in (mop-slots mop2))
            :always (eql (slot-filler slot)
                         (get-filler (slot-role slot)
                                     mop1)))
       mop1))

(defun mop-equalp (mop1 mop2)
  (and (mop-includesp mop2 mop1)
       (mop-includesp mop1 mop2)))

(defun get-twin (mop)
  (for (abst :in (mop-absts mop))
       :first (for (spec :in (mop-specs abst))
                   :when (not (eql spec mop))
                   :first (mop-equalp spec mop))))

(defun refine-instance (instance)
  (for (abst :in (mop-absts instance))
       :when (mops-abstp (mop-specs abst) instance)
       :first (unlink-abst instance abst)
       (refine-instance instance)))

(defun mops-abstp (mops instance)
  (not (null (for (mop :in mops)
                  :when (slots-abstp mop instance)
                  :save (link-abst instance mop)))))

(defun install-instance (instance)
  (refine-instance instance)
  (let ((twin (get-twin instance)))
    (cond (twin (remove-mop instance) twin)
          ((has-legal-absts-p instance) instance)
          (t (remove-mop instance) nil))))

(defun has-legal-absts-p (instance)
  (for (abst :in (mop-absts instance))
       :when (not (legal-abstp abst instance))
       :do (unlink-abst instance abst))
  (mop-absts instance))

(defun legal-abstp (abst instance)
  (declare (ignore instance))
  (and (mop-slots abst)
       (for (spec :in (mop-specs abst))
            :always (instance-mopp spec))))

(defun install-abstraction (mop)
  (let ((twin (get-twin mop)))
    (cond (twin (remove-mop mop) twin)
          (t (reindex-siblings mop)))))

(defun reindex-siblings (mop)
  (for (abst :in (mop-absts mop))
       :do (for (spec :in (mop-specs abst))
                :when (and (instance-mopp spec)
                           (slots-abstp mop spec))
                :do (unlink-abst spec abst)
                (link-abst spec mop)))
  mop)

(defun slots->mop (slots absts must-work)
  (insist slots->mop
          (not (null absts))
          (for (abst :in absts) :always (mopp abst)))
  (or (and (null slots) (null (cdr absts)) (car absts))
      (let ((type (and slots (atom (car slots))
                       (car slots))))
        (and type (setf slots (cdr slots)))
        (let ((mop (new-mop nil absts type slots)))
          (let ((result
                 (cond ((instance-mopp mop)
                        (install-instance mop))
                       (t (install-abstraction mop)))))
            (insist slots->mop
                    (or result (null must-work)))
            result)))))

(defmacro defmop (name absts &rest args)
  (let ((type (and args (atom (car args)) (car args))))
    (let ((slot-forms (cond (type (cdr args))
                            (t args))))
      `(new-mop ',name ',absts ',type
                (forms->slots ',slot-forms)))))

(defun forms->slots (slot-forms)
  (for (slot-form :in slot-forms)
       :save
       (cond ((atom slot-form) slot-form)
             (t (make-slot (slot-role slot-form)
                           (let ((abst (cadr slot-form)))
                             (insist forms->slots (atom abst))
                             (and abst
                                  (slots->mop
                                   (forms->slots (cddr slot-form))
                                   (list abst)
                                   t))))))))

(defun group-size (x)
  (and (groupp x) (length (mop-slots x))))

(defun group->list (group)
  (and group
       (insist group->list (groupp group))
       (for (index :in (make-m-n 1 (group-size group)))
            :filter (role-filler index group))))

(defun list->group (l)
  (cond ((null l) 'I-M-EMPTY-GROUP)
        (t (slots->mop
            (for (x :in l)
                 (i :in (make-m-n 1 (length l)))
                 :save (make-slot i x))
            '(m-group)
            t))))

(defun make-m-n (m n)
  (insist make-m-n (integerp m) (integerp n))
  (cond ((eql m n) (list n))
        ((< m n) (cons m (make-m-n (+ m 1) n)))
        (t (cons m (make-m-n (- m 1) n)))))

(defun dah (mop)
  (pprint (tree->list mop #'specs->list nil)))

(defun dph (mop)
  (pprint (tree->list mop #'slots->forms nil)))

(defun specs->list (mop visited)
  (for (spec :in (mop-specs mop))
       :save (tree->list spec #'specs->list visited)))

(defun slots->forms (mop visited)
  (for (slot :in (mop-slots mop))
       :save (cons (slot-role slot)
                   (mop->form (slot-filler slot)
                              visited))))

(defun mop->form (mop visited)
  (tree->list mop #'slots->forms visited))

(defun tree->list (mop fn visited)
  (cond ((member mop visited) (list mop))
        (t (setf visited (cons mop visited))
           `(,mop ,@(funcall fn mop visited)))))
