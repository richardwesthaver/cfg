;; dired-cfg
(require 'default)
(add-to-list 'package-selected-packages 'all-the-icons-dired)
;; TODO 2023-08-19: for GUI only, not in tui
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
(provide 'dired-cfg)
