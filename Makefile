# Ensure emacs always runs from this makefile's PWD
EMACS_FLAGS=--eval '(setq user-emacs-directory default-directory)' -l init.el
EMACS=emacs --quick --batch $(EMACS_FLAGS)
EMACSI=emacs -q $(EMACS_FLAGS)

autoloads: init.el
	@$(EMACS) -f doom//reload-autoloads

compile: init.el
	@$(EMACS) -f doom//byte-compile

recompile: init.el
	@$(EMACS) -f doom//byte-compile -- -r

clean: init.el
	@$(EMACS) -f doom//clean-byte-compiled-files

# Runs Emacs from a different folder than ~/.emacs.d
run:
	@$(EMACSI) -l init.el

init.el:
	@$(error No init.el file; create one or copy init.example.el)
