;; dired-cfg
(require 'default)
(add-to-list 'package-selected-packages 'all-the-icons-dired)
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
(provide 'dired-cfg)
