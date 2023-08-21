;;; lang/org-exts/+slack.el -*- lexical-binding: t; -*-

(add-load-path! "autoload")

(after! ox
  (require 'ox-gfm)
  (org-export-define-derived-backend
   'slack 'gfm
   :translate-alist '((bold . ox-slack--bold)
                      (example-block . ox-slack--fixed-width-block)
                      (fixed-width . ox-slack--fixed-width-block)
                      (headline . ox-slack--markup-headline)
                      (italic . ox-slack--italic)
                      (item . ox-slack--item)
                      (link . ox-slack--link)
                      (src-block . ox-slack--fixed-width-block)
                      (strike-through . ox-slack--strike-through)
                      (timestamp . ox-slack--timestamp)
                      (subscript . ox-slack--passthrough)
                      (superscript . ox-slack--passthrough)
                      (underline . ox-slack--italic))
   :menu-entry
   '(?s "Export to Slack Markup"
     ((?c "To clipboard" ox-slack-export-to-clipboard)
      (?s "To temporary buffer" ox-slack-export-to-buffer)))))

(add-to-list 'safe-local-variable-values '(ox-slack-postprocess-function . ox-slack-translate-names))
