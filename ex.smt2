(set-logic QF_SLIA)

(set-option :strings-fmf true)

(declare-fun x () String)
(declare-fun a () String)

(assert (= (str.replace_all (str.replace_all (str.replace_all (str.replace_all a "a" "100") "b" "0") "c" "1") "d" "1") x))
(assert (= (str.replace_all (str.replace_all (str.replace_all (str.replace_all a "a" "1") "b" "100") "c" "00") "d" "111") x))

(assert (> (str.len x) 0))

(assert (str.in_re a (re.+ (re.union (str.to_re "a") (str.to_re "b") (str.to_re "c") (str.to_re "d")) )))

(check-sat)
(get-model)
