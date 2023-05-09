;;; ledger-cfg.el --- Ledger Config -*- lexical-binding: t; -*-
(add-packages 'hledger-mode)
(autoload 'ledger-mode "ledger-mode" "A major mode for Ledger" t)
(add-to-list 'auto-mode-alist '("\\.ledger$" . ledger-mode))
(provide 'ledger-cfg)
;; ledger-cfg.el ends here
