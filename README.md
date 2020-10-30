# Emacs Lisp Docstring Server

elisp-docstring-server.el provides a web server for Emacs Lisp Docstring.

![screenshot of the website](2020-10-29-screenshot.png)

the above screenshot is showing https://elisp-docstring.xuchunyang.me/?q=car

## Usage

1. M-x elisp-docstring-server-start
2. http://localhost:3000

## Demo

![is elisp-docstring.xuchunyang.me down?](https://github.com/xuchunyang/elisp-docstring-server.el/workflows/is%20elisp-docstring.xuchunyang.me%20down%3F/badge.svg)

https://elisp-docstring.xuchunyang.me/

Emacs => Varnish (for cache) => Caddy (for HTTPS)

## Requires

- Emacs 25.1 or later
- https://github.com/eschulte/emacs-web-server (M-x package-install web-server)
