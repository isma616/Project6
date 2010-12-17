(def (concat self xs ys) (cond ((null? xs) ys) (else (cons (car xs) (self self (cdr xs) ys)))) (concat concat (cons 1 (cons 2 nil)) (cons 3 (cons 4 nil))))
(def (reverse self xs) (cond ((null? xs) nil) (else (concat concat (self self (cdr xs)) (cons (car xs) nil)))) (reverse reverse (cons 1 (cons 2 (cons 3 (cons 4 nil))))))
