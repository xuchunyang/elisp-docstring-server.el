EMACS = emacs

all: elisp-docstring-server.elc

elisp-docstring-server.elc: elisp-docstring-server.el web-server.elc web-server-status-codes.elc
	$(EMACS) -Q --batch -L . --eval "(setq byte-compile-error-on-warn t)" -f batch-byte-compile $<

web-server.elc: web-server.el web-server-status-codes.elc
	$(EMACS) -Q --batch -L . -f batch-byte-compile $<

web-server-status-codes.elc: web-server-status-codes.el
	$(EMACS) -Q --batch -L . -f batch-byte-compile $<

web-server.el:
	curl -O https://raw.githubusercontent.com/eschulte/emacs-web-server/master/web-server.el

web-server-status-codes.el:
	curl -O https://raw.githubusercontent.com/eschulte/emacs-web-server/master/web-server-status-codes.el

.PHONY: clean
clean:
	rm -v *.elc
