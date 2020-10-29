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

.PHONY: daemon
daemon: elisp-docstring-server.elc
	PORT=3000 HOST=127.0.0.1 $(EMACS) --daemon -Q -L . -l elisp-docstring-server -f elisp-docstring-server-start

.PHONY: start
start:
	if pgrep emacs; then emacsclient --eval '(kill-emacs 0)'; fi
	if pgrep caddy; then caddy stop; fi
	make daemon
	caddy start
