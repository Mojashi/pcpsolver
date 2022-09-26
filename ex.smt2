(declare-fun x () String)
(declare-fun a () String)
(declare-fun b () String)

(assert (= (str.replace_all (str.replace_all (str.replace_all (str.replace_all a "1" "111") "2" "111") "3" "10) "4" "0") x))
(assert (= (str.replace_all (str.replace_all (str.replace_all (str.replace_all b "1" "010") "2" "1") "3" "100) "4" "111") x))

(check-sat)
(get-model)
