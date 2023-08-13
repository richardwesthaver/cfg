;;; ledger-cfg.el --- Ledger Config -*- lexical-binding: t; -*-
(add-packages 'ledger-mode)
(autoload 'ledger-mode "ledger-mode" "A major mode for Ledger" t)
(add-to-list 'auto-mode-alist '("\\.ledger$" . ledger-mode))
(setq ledger-default-date-format "%Y-%m-%d")
(when-sys= "zor"
  (setq ledger-master-file "/mnt/k/bank/bank.ledger"))
(provide 'ledger-cfg)
;; ledger-cfg.el ends here
