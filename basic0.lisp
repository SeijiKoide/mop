(clear-memory)

(defmop m-event (m-root))
(defmop m-state (m-root))
(defmop m-act (m-root))
(defmop m-actor (m-root))

(defmop m-group (m-root))
(defmop m-empty-group (m-group))
(defmop i-m-empty-group (m-empty-group) instance)

(defmop m-function (m-root))
(defmop constraint-fn (m-function))

(defmop m-pattern (m-root) (abst-fn constraint-fn))

(defmop get-sibling (m-function))

(defmop m-case (m-root)
  (old m-pattern (calc-fn get-sibling)))

(defmop m-role (m-root))

(defmop not-constraint (constraint-fn))
(defmop m-not (m-pattern) (abst-fn not-constraint))

(defmop m-failed-solution (m-root))

(defun constraint-fn (constraint filler slots)
  (declare (ignore constraint filler slots))
  t)

(defun not-constraint (constraint filler slots)
  (insist no-constraint (not (null filler)))
  (not (satisfiedp (get-filler 'object constraint)
                   filler slots)))

(defun get-sibling (pattern mop)
  (declare (ignore pattern))
  (for (abst :in (mop-absts mop))
       :first (for (spec :in (mop-specs abst))
                   :when (and (instance-mopp spec)
                              (not (eql spec mop))
                              (not (abstp
                                    'm-failed-solution
                                    spec)))
                   :first spec)))
