;; NOTE: You may need to run following shell commands to create some
;;       files that are expected to exist in your user-emacs-directory
;;       $ touch custom.el
;;       $ mkdir elpa-<emacs-version> (e.g. 'elpa-28')
;;       $ touch settings.org
;; Probably forgetting one but it will be obvious if an error pops up.       

;; Do not use `init.el` for `custom-*` code - use `custom.el`.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(when (file-exists-p custom-file)
  (load-file custom-file))


;; Require and initialize `package`.
(require 'package)

;; set package.el repositories.
(setq package-archives
      '(("org"   . "https://orgmode.org/elpa/")
        ("gnu"   . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))

;; Put downloaded packages inside "<user-emacs-directory>/elpa-<version>".
(let ((path (format
             (concat user-emacs-directory "elpa-%s") emacs-major-version)))
  (if (file-accessible-directory-p path)
      (setq package-user-dir path)))

;; initialize built-in package management.
(package-initialize)

;; update packages list if we are on a new install.
(unless package-archive-contents
  (package-refresh-contents))

;; a list of pkgs to programmatically install.
(setq my-package-list '(use-package evil helm))

;; programmatically install/ensure installed
;; pkgs in your personal list
(dolist (package my-package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; Can now "(use-package pkgname)" for package configuring.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; For packages not installed via elpa, need to add them to load path.
(let ((path 
       (expand-file-name "lisp"))) ;; i.e. <user-emacs-directory>/lisp  
  (if (file-accessible-directory-p path)
      (add-to-list 'load-path path t)))


(use-package org
  :config
  (setq org-startup-folded nil
        org-src-preserve-indentation t
        org-src-tab-acts-natively t)
  )


;; Configure rest of packages in "settings.org" file.
(org-babel-load-file
 ;; NOTE: settings.org must contain at least one "#+begin_src emacs-lisp"
 ;;       block for there to be no error on startup.
 (expand-file-name "settings.org"
                   user-emacs-directory))
;;
;; MISC NOISE PUT HERE SO "settings.org" is not polluted.
;;

;;    ______            ____   ____             __                   
;;   / ____/___  ____  / __/  / __ )____ ______/ /____  ______  _____
;;  / /   / __ \/ __ \/ /_   / __  / __ `/ ___/ //_/ / / / __ \/ ___/
;; / /___/ /_/ / / / / __/  / /_/ / /_/ / /__/ ,< / /_/ / /_/ (__  ) 
;; \____/\____/_/ /_/_/    /_____/\__,_/\___/_/|_|\__,_/ .___/____/  
;;                                                    /_/  
;;
;;
;; Default and per-save backups go here:
(setq-default backup-directory-alist
              `((".*" . ,(expand-file-name (concat user-emacs-directory "backup")))))
(setq-default auto-save-file-name-transforms
              `((".*" ,(expand-file-name (concat user-emacs-directory "backup")) nil)))

;; From https://stackoverflow.com/questions/151945/how-do-i-control-how-emacs-makes-backup-files

;; NOTE: backup files are placed in these folders but to see them you
;;       must do a 'ls -a .' in the folders. #<file># files are lock
;;       files and cannot be moved to a separate directory.
(setq version-control t     ;; Use version numbers for backups.
      kept-new-versions 10  ;; Number of newest versions to keep.
      kept-old-versions 0   ;; Number of oldest versions to keep when a new numbered backup is made.
      delete-old-versions t ;; Don't ask to delete excess backup versions.
      backup-by-copying t)  ;; Copy all files, don't rename them.

(setq vc-make-backup-files t)

(defun force-buffer-backup ()
  ;; Make a special "per session" backup at the first save of each
  ;; emacs session.
  (when (not buffer-backed-up)
    ;; per-session backups: once on the first save of the buffer in each
    ;; Emacs session. These simulate Emac's default backup behavior.
    (let ((backup-directory-alist
           `((".*" .
              ,(concat (expand-file-name (concat user-emacs-directory "backup")) "/session"))))
          (kept-new-versions 3))
      (backup-buffer)))
  ;; Make a "per save" backup on each save.  The first save results in
  ;; both a per-session and a per-save backup, to keep the numbering
  ;; of per-save backups consistent.
  (let ((buffer-backed-up nil))
    (backup-buffer)))

(add-hook 'before-save-hook 'force-buffer-backup)



;; enable escaping from helm using <escape>.
(global-set-key (kbd "<escape>") 'helm-keyboard-quit)
