;;; lang/asm/config.el -*- lexical-binding: t; -*-

(setq-hook! asm-mode tab-width 8)
(add-hook! 'asm-mode-hook
  (indent-tabs-mode +1))
