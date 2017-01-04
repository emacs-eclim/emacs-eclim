;;; eclim-problems.el --- an interface to the Eclipse IDE.  -*- lexical-binding: t -*-
;;
;; Copyright (C) 2009, 2012  Tassilo Horn <tassilo@member.fsf.org>
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Contributors
;;
;;  - Nikolaj Schumacher <bugs * nschum de>
;;  - Yves Senn <yves senn * gmx ch>
;;  - Fredrik Appelberg <fredrik * bitbakery se>
;;  - Alessandro Arzilli <alessandro.arzilli * gmail com>
;;
;;; Commentary:
;;
;;
;;; Code:

(require 'popup)
(require 'hl-line)
(require 'cl-lib)
(require 'eclim-common)
(eval-when-compile (require 'cl)) ;; lexical-let
(eval-when-compile (require 'eclim-macros))

(defgroup eclim-problems nil
  "Problems: settings for displaying the problems buffer and highlighting errors in code."
  :group 'eclim)

(defface eclim-problems-highlight-error-face
  '((t (:underline "red")))
  "Face used for highlighting errors in code"
  :group 'eclim-problems)

(defface eclim-problems-highlight-warning-face
  '((t (:underline "orange")))
  "Face used for highlighting errors in code"
  :group 'eclim-problems)

(defvar eclim-problems-mode-hook nil)

(defvar eclim-problems-mode-map
      (let ((map (make-keymap)))
        (suppress-keymap map t)
        (define-key map (kbd "a") 'eclim-problems-show-all)
        (define-key map (kbd "e") 'eclim-problems-show-errors)
        (define-key map (kbd "g") 'eclim-problems-buffer-refresh)
        (define-key map (kbd "q") 'eclim-quit-window)
        (define-key map (kbd "w") 'eclim-problems-show-warnings)
        (define-key map (kbd "f") 'eclim-problems-toggle-filefilter)
        (define-key map (kbd "c") 'eclim-problems-correct)
        (define-key map (kbd "RET") 'eclim-problems-open-current)
        map))

(define-key eclim-mode-map (kbd "C-c C-e b") 'eclim-problems)
(define-key eclim-mode-map (kbd "C-c C-e o") 'eclim-problems-open)

(defconst eclim--problems-buffer-name "*eclim: problems*")
(defconst eclim--problems-compilation-buffer-name "*compilation: eclim*")

(defun eclim--problems-mode ()
  (kill-all-local-variables)
  (buffer-disable-undo)
  (setq major-mode 'eclim-problems-mode
        mode-name "eclim/problems"
        mode-line-process ""
        truncate-lines t
        buffer-read-only t
        default-directory (eclim/workspace-dir))
  (setq-local line-move-visual nil)
  (setq mode-line-format
        (list "-"
              'mode-line-mule-info
              'mode-line-modified
              'mode-line-frame-identification
              'mode-line-buffer-identification

              "   "
              'mode-line-position

              "  "
              'eclim--problems-filter-description

              "  "
              'mode-line-modes
              '(which-func-mode ("" which-func-format "--"))

              'global-mode-string
              "-%-"))
  (hl-line-mode t)
  (use-local-map eclim-problems-mode-map)
  (run-mode-hooks 'eclim-problems-mode-hook))

(defun eclim--problems-apply-filter (f)
  (setq eclim--problems-filter f)
  (eclim-problems-buffer-refresh))

(defun eclim-problems-show-errors ()
  (interactive)
  (eclim--problems-apply-filter "e"))

(defun eclim-problems-toggle-filefilter ()
  (interactive)
  (setq eclim--problems-filefilter (not eclim--problems-filefilter))
  (eclim--problems-buffer-redisplay))

(defun eclim-problems-show-warnings ()
  (interactive)
  (eclim--problems-apply-filter "w"))

(defun eclim-problems-show-all ()
  (interactive)
  (eclim--problems-apply-filter nil))

(defadvice find-file (after eclim-problems-highlight-on-find-file activate)
  (eclim-problems-highlight))
(defadvice find-file-other-window (after eclim-problems-highlight-on-find-file-other-window activate)
  (eclim-problems-highlight))
(defadvice other-window (after eclim-problems-highlight-on-other-window activate)
  (eclim-problems-highlight))
(defadvice switch-to-buffer (after eclim-problems-highlight-switch-to-buffer activate)
  (eclim-problems-highlight))

(defun eclim--problems-get-current-problem ()
  (let ((buf (get-buffer "*eclim: problems*")))
    (if (eq buf (current-buffer))
        ;; we are in the problems buffer
        (let ((problems (eclim--problems-filtered))
              (index (1- (line-number-at-pos))))
          (if (>= index (length problems))
              (error "No problem on this line.")
            (aref problems index)))
      ;; we need to figure out which problem corresponds to this pos
      (save-restriction
        (widen)
        (let ((line (line-number-at-pos)))
          (or (cl-find-if (lambda (p) (and (string= (assoc-default 'filename p) (file-truename buffer-file-name))
                                           (= (assoc-default 'line p) line)))
                          eclim--problems-list)
              (error "No problem on this line")))))))

(defun eclim-problems-open-current (&optional same-window)
  (interactive)
  (let* ((p (eclim--problems-get-current-problem)))
    (funcall (if same-window
                 'find-file
               'find-file-other-window)
             (assoc-default 'filename p))
    (eclim--problem-goto-pos p)))

(defun eclim-problems-correct ()
  "Pops up a suggestion for the current correction. This can be
invoked in either the problems buffer or a source code buffer."
  (interactive)
  (let ((p (eclim--problems-get-current-problem)))
    (unless (string-match "\\.\\(groovy\\|java\\)$" (cdr (assoc 'filename p)))
      (error "Not a Java or Groovy file.  Corrections are currently supported only for Java or Groovy"))
    (if (eq major-mode 'eclim-problems-mode)
        (let ((p-buffer (find-file-other-window (assoc-default 'filename p))))
          (with-selected-window (get-buffer-window p-buffer t)
            ;; Intentionally DON'T save excursion. Often times we need edits.
            (eclim--problem-goto-pos p)
            (eclim-java-correct (cdr (assoc 'line p)) (eclim--byte-offset))))
      ;; source code buffer
      (eclim-java-correct (cdr (assoc 'line p)) (eclim--byte-offset)))))

(defun eclim--warning-filterp (x)
  (eq t (assoc-default 'warning x)))

(defun eclim--error-filterp (x)
  (not (eclim--warning-filterp x)))

(defun eclim--get-problems-buffer ()
  "Return the eclim problems buffer, if it exists. Otherwise,
create and initialize a new buffer."
  (or (get-buffer "*eclim: problems*")
      (let ((buf (get-buffer-create "*eclim: problems*")))
        (save-excursion
          ;; (setq eclim--problems-project (eclim-project-name))
          (setq eclim--problems-file buffer-file-name)
          (set-buffer buf)
          (eclim--problems-mode)
          ;;(eclim-problems-buffer-refresh)
          (goto-char (point-min))))))

(defun eclim--problems-mode-init (&optional quiet)
  "Create and initialize the eclim problems buffer. If the
argument QUIET is non-nil, open the buffer in the background
without switching to it."
  (let ((buf (get-buffer-create "*eclim: problems*")))
    (save-excursion
      (setq eclim--problems-project (eclim-project-name))
      (setq eclim--problems-file buffer-file-name)
      (set-buffer buf)
      (eclim--problems-mode)
      (eclim-problems-buffer-refresh)
      (goto-char (point-min)))
    (if (not quiet)
        (switch-to-buffer buf))))

(defun eclim-problems ()
  "Show current compilation problems in a separate window."
  (interactive)
  (if (eclim-project-name)
      (eclim--problems-mode-init)
    (error "Could not figure out the current project. Is this an eclim managed buffer?")))

(defun eclim-problems-open ()
  "Opens a new (emacs) window inside the current frame showing the current project compilation problems"
  (interactive)
  (let ((w (selected-window)))
    (select-window (split-window nil (round (* (window-height w) 0.75)) nil))
    (eclim-problems)
    (select-window w)))

(add-hook 'find-file-hook
          (lambda () (when (and (eclim--accepted-p (buffer-file-name))
                                (not (get-buffer eclim--problems-buffer-name)))
                       (eclim--problems-mode-init t))))

(defun eclim-problems-refocus ()
  (interactive)
  (when (eclim--project-dir)
    (setq eclim--problems-project (eclim-project-name))
    (setq eclim--problems-file buffer-file-name)
    (with-current-buffer eclim--problems-buffer-name
      (eclim-problems-buffer-refresh))))

(defun eclim-problems-next (&optional same-window)
  (interactive)
  (let ((prob-buf (get-buffer eclim--problems-buffer-name)))
    (when prob-buf
      (set-buffer prob-buf)
      (if (boundp 'eclim--problems-list-at-first)
          (setq eclim--problems-list-at-first nil)
        (forward-line 1))
      (hl-line-move hl-line-overlay)
      (eclim-problems-open-current same-window))))

(defun eclim-problems-previous (&optional same-window)
  (interactive)
  (let ((prob-buf (get-buffer eclim--problems-buffer-name)))
    (when prob-buf
      (set-buffer prob-buf)
      (forward-line -1)
      (hl-line-move hl-line-overlay)
      (eclim-problems-open-current same-window))))

(defun eclim-problems-next-same-window ()
  (interactive)
  (eclim-problems-next t))

(defun eclim-problems-previous-same-window ()
  (interactive)
  (eclim-problems-previous t))

(defun eclim-problems-compilation-buffer ()
  "Creates a compilation buffer from eclim error messages. This
is convenient as it lets the user navigate between errors using
`next-error' (\\[next-error])."
  (interactive)
  (lexical-let ((filecol-size (eclim--problems-filecol-size))
                (project-directory (concat (eclim--project-dir) "/"))
                (compil-buffer (get-buffer-create eclim--problems-compilation-buffer-name))
                (project-name (eclim-project-name))) ; To store it in buffer.

    (with-current-buffer compil-buffer
      (setq default-directory project-directory)
      (setq mode-line-process
            (concat ": " (propertize "refreshing"
                                     'face 'compilation-mode-line-run))))
    ;; Remember that the part below is asynchronous. This can be tricky.
    (eclim--with-problems-list _problems
      (let (saved-user-pos)
        (with-current-buffer compil-buffer
          (buffer-disable-undo)
          (setq buffer-read-only nil)
          (setq saved-user-pos (point))
          (erase-buffer)
          (let ((errors 0) (warnings 0))
            (cl-loop for problem across (eclim--problems-filtered) do
                     (eclim--insert-problem-compilation
                      problem filecol-size project-directory)
                     (if (eq t (assoc-default 'warning problem)) ; :json-false, WTH
                         (setq warnings (1+ warnings))
                       (setq errors (1+ errors))))
            (let ((msg (format
                        "Compilation results: %d errors, %d warnings [%s].\n"
                        errors warnings (current-time-string))))
              (insert "\n" msg)
              (goto-char (point-min))
              (insert msg "\n"))
            (compilation-mode)
            ;; The above killed local variables, so recover our lexical-lets
            (setq default-directory project-directory)
            (setq eclim--project-name project-name)
            ;; Remap the very dangerous "g" command :)  A make -k in some of
            ;; my projects would throw Eclipse off-balance by cleaning .classes.
            ;; May look funky, but it's safe.
            (local-set-key "g" 'eclim-problems-compilation-buffer)

            (setq mode-line-process
                  (concat ": "
                          (propertize (format "%d/%d" errors warnings)
                                      'face (when (> errors 0)
                                              'compilation-mode-line-fail))))))
        ;; Sometimes, buffer was already current. Note outside with-current-buf.
        (unless (eq compil-buffer (current-buffer))
          (display-buffer compil-buffer 'other-window))
        (with-selected-window (get-buffer-window compil-buffer t)
          (when (< saved-user-pos (point-max))
            (goto-char saved-user-pos)))))))

(defun eclim--insert-problem-compilation (problem _filecol-size project-directory)
  (let ((filename (cl-first (split-string (assoc-default 'filename problem) project-directory t)))
        (description (assoc-default 'message problem))
        (type (if (eq t (assoc-default 'warning problem)) "W" "E")))
    (let ((line (assoc-default 'line problem))
          (col (assoc-default 'column problem)))
      (insert (format "%s:%s:%s: %s: %s\n" filename line col (upcase type) description)))))

(defun eclim--count-current-errors ()
  (length
   (eclim--filter-problems "e" t (buffer-file-name (current-buffer)) eclim--problems-list)))

(defun eclim--count-current-warnings ()
  (length
   (eclim--filter-problems "w" t (buffer-file-name (current-buffer)) eclim--problems-list)))

(defun eclim-problems-next-same-file (&optional up)
  "Moves to the next problem in the current file, with wraparound. If UP
or prefix arg, moves to previous instead; see `eclim-problems-prev-same-file'."
  (interactive "P")
  ;; This seems pretty inefficient, but it's fast enough. Would be even
  ;; more inefficient if we didn't assume problems were sorted.
  (let ((problems-file
         (eclim--filter-problems nil t (buffer-file-name (current-buffer))
                                 eclim--problems-list))
        (pass-line (line-number-at-pos))
        (pass-col (+ (current-column) (if up 0 1)))
        (first-passed nil) (last-not-passed nil))
    (when (= 0 (length problems-file)) (error "No problems in this file"))
    (cl-loop for p across problems-file until first-passed do
             (let ((line (assoc-default 'line p))
                   (col (assoc-default 'column p)))
               (if (or (> line pass-line)
                       (and (= line pass-line) (> col pass-col)))
                   (setq first-passed p)
                 (setq last-not-passed p))))
    (eclim--problem-goto-pos
     (or
      (if up last-not-passed first-passed)
      (when up (message "Moved past first error, continuing to last")
            (elt problems-file (- (length problems-file) 1))) ; Ugh, vector
      (progn (message "Moved past last error, continuing to first")
             (elt problems-file 0))))))

(defun eclim-problems-prev-same-file ()
  "Moves to the previous problem in the same file, with wraparound."
  (interactive)
  (eclim-problems-next-same-file t))


(defun eclim-problems-modeline-string ()
  "Returns modeline string with additional info about
problems for current file"
  (concat (format ": %s/%s"
                  (eclim--count-current-errors)
                  (eclim--count-current-warnings))
          (when eclim--problems-refreshing "*")))

(provide 'eclim-problems)
;;; eclim-problems ends here
