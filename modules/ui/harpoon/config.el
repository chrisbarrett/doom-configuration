;;; ui/harpoon/config.el -*- lexical-binding: t; -*-

(map! :mn "U" 'harpoon-quick-menu-hydra
      :mn "C-s" 'harpoon-add-file)

(map! :leader
      "j c" 'harpoon-clear
      "j s" 'harpoon-add-file
      "j f" 'harpoon-toggle-file
      "1" 'harpoon-go-to-1
      "2" 'harpoon-go-to-2
      "3" 'harpoon-go-to-3
      "4" 'harpoon-go-to-4)
