;;; tools/nix/config.el -*- lexical-binding: t; -*-

(defvar +nix-config-directory "~/.config/nix-configuration"
  "Directory containing the user's nix configuration.")

(after! format-all
  (define-format-all-formatter nixpkgs-fmt
                               (:executable "nixpkgs-fmt")
                               (:install "nix-env -f https://github.com/nix-community/nixpkgs-fmt/archive/master.tar.gz -i")
                               (:modes nix-mode)
                               (:format (format-all--buffer-easy executable))))
