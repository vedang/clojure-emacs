;;; nrepl-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (nrepl nrepl-jack-in nrepl-disable-on-existing-clojure-buffers
;;;;;;  nrepl-enable-on-existing-clojure-buffers nrepl-interaction-mode)
;;;;;;  "nrepl/nrepl" "nrepl/nrepl.el" (21001 15566 0 0))
;;; Generated autoloads from nrepl/nrepl.el

(autoload 'nrepl-interaction-mode "nrepl/nrepl" "\
Minor mode for nrepl interaction from a Clojure buffer.

\\{nrepl-interaction-mode-map}

\(fn &optional ARG)" t nil)

(autoload 'nrepl-enable-on-existing-clojure-buffers "nrepl/nrepl" "\
Enable interaction mode on existing Clojure buffers.
See command `nrepl-interaction-mode'.

\(fn)" t nil)

(autoload 'nrepl-disable-on-existing-clojure-buffers "nrepl/nrepl" "\
Disable interaction mode on existing Clojure buffers.
See command `nrepl-interaction-mode'.

\(fn)" t nil)

(autoload 'nrepl-jack-in "nrepl/nrepl" "\
Start a nREPL server for the current project and connect to it.
If PROMPT-PROJECT is t, then prompt for the project for which to
start the server.

\(fn &optional PROMPT-PROJECT)" t nil)

(add-hook 'nrepl-connected-hook 'nrepl-enable-on-existing-clojure-buffers)

(autoload 'nrepl "nrepl/nrepl" "\
Connect nrepl to HOST and PORT.

\(fn HOST PORT)" t nil)

(eval-after-load 'clojure-mode '(progn (define-key clojure-mode-map (kbd "C-c M-j") 'nrepl-jack-in) (define-key clojure-mode-map (kbd "C-c M-c") 'nrepl)))

;;;***

(provide 'nrepl-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; nrepl-autoloads.el ends here
