;;; tools/nix/config.el -*- lexical-binding: t; -*-

(defvar +nix-config-directory "~/.config/nix-configuration"
  "Directory containing the user's nix configuration.")

(add-hook! 'nix-mode-hook
  (format-all-mode +1)
  (setq-local format-all-formatters '(("Nix" nixpkgs-fmt))))
