(load "./ex3-3.scm")
;; 参考
;; http://www.serendip.ws/archives/1216
;; make-account には手を加えずに、make-joint で追加口座のパスワードを管理する
(define (make-joint account account-password new-password)
  (define (dispatch password m)
    (if (eq? password new-password)
	;; 新しいパスワードと一致した場合は口座の本来のパスワードに変換
        (account account-password m)
	;; 新しいパスワードと一致しなかった場合はパスワードを空で渡す
        (account '() m)))
  dispatch)
