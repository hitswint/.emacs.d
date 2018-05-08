#!/bin/bash

emacsclient -a '' -c -F "((name . \"ec_float\")(top . -1))" -e "(let ((browse-url-browser-function 'browse-url-firefox) helm-quit-hook helm-after-action-hook) (dolist (hook '(helm-quit-hook helm-after-action-hook)) (add-hook hook 'delete-frame)) (helm-firefox-history))"
