;;; nano-theme-support.el --- NANO theme (support) -*- lexical-binding: t -*-

;; Copyright (C) 2021,2025 Free Software Foundation, Inc.

;; Maintainer: Nicolas P. Rougier <Nicolas.Rougier@inria.fr>
;; URL: https://github.com/rougier/nano-theme
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: theme, dark, light, gray, mono

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; N Λ N O theme is a consistent theme that comes in four flavors:
;;  - a light theme based on Material (https://material.io/)
;;  - a gray theme based on Material (https://material.io/)
;;  - a dark theme based on Nord (https://www.nordtheme.com/).
;;  - a mono theme that is black and white with two shades of gray
;;
;; A theme is fully defined by a set of 8 faces as
;; explained in this paper https://arxiv.org/abs/2008.06030:
;;
;; - Default face is the face for regular information.
;;
;; - Critical face is for information that requires immediate action.
;;
;;     It should be of high constrast when compared to other
;;     faces.  This can be realized (for example) by setting an intense
;;     background color, typically a shade of red.  It must be used
;;     scarcely.
;;
;; - Popout face is used for information that needs attention.
;;
;;     To achieve such effect, the hue of the face has to be
;;     sufficiently different from other faces such that it attracts
;;     attention through the popout effect.
;;
;; - Strong face is used for information of a structural nature.
;;
;;     It has to be the same color as the default color and only the
;;     weight differs by one level (e.g., light/regular or
;;     regular/bold).  IT is generally used for titles, keywords,
;;     directory, etc.
;;
;; - Salient face is used for information that are important.
;;
;;     To suggest the information is of the same nature but important,
;;     the face uses a different hue with approximately the same
;;     intensity as the default face.  This is typically used for
;;     links.

;; - Faded face is for information that are less important.
;;
;;     It is made by using the same hue as the default but with a
;;     lesser intensity than the default.  It can be used for comments,
;;     secondary information and also replace italic (which is
;;     generally abused anyway
;;
;; - Subtle face is used to suggest a physical area on the screen.
;;
;;     It is important to not disturb too strongly the reading of
;;     information and this can be made by setting a very light
;;     background color.
;;
;; - Highlight face is used to highlight a physical area on the screen.
;;
;;     Highlight is mostly used to transientyl highlight a region by
;;     setting a very light background color that is barely perceptible.
;;
;; Any known package can be customized through the customization interface.

;; Currently know packages are:
;; - Core Emacs
;; - Org
;; - Org agenda
;; - Dired
;; - Vertico
;; - Marginalias
;; - Magit
;; - Corfu
;; - Mu4e
;; - Gnus
;; - Elfeed
;; - Deft
;; - Ledger
;; - Message
;; - EPA
;; - Popup
;; - Diff
;; - Icomplete
;; - Markdown
;; - SHR
;; - Elpher
;; - Buffer box

;; If you want a package to be added, please make a pull request at:
;; https://github.com/rougier/nano-theme

;;; Usage example:

;; (require 'nano-theme)
;; (load-theme 'nano-light t)
;; (load-theme 'nano-gray t)
;; (load-theme 'nano-dark t)
;; (load-theme 'nano-mono t)

;;; NEWS:

;; Version 1.0.0
;; - NANO Theme is now a regular theme
;; - Added nano-gray (light gray background)
;; - Added nano-mono (monochrome with two shades of gray)
;; - Customization for each known packages

;; Version 0.3.5
;; - Added diff-hl faces
;; - Modified line-num er faces

;; Version 0.3.3
;; - Removed debug message
;; - Minor changes in agenda

;; Version 0.3.2
;; - Fix magit diff whitespace
;; - Update mu4e faces (1.8.x release)
;; - Added rounded corners for emacs-plus@29

;; Version 0.3.1
;; - Modified vertico and org modes
;; - Added imenu-list, ansi-color and SHR faces

;; Version 0.3.0
;; - Added italic (Victor Mono)
;; - Less salient critical face
;; - Added orderles, marginalia & corfu faces

;; Version 0.2.1
;; - Added nano-modeline faces

;; Version 0.2
;; - Split light / dark themes properly
;; - Added a nano-new-frame function
;;
;; Version 0.1
;; - Submission to ELPA

;;; Code:

(defcustom nano-theme-weights
  '((regular . bold) . (light . regular))
  "Font weights for regular and bold faces, depending on Emacs display mode (TTY or GUI).

Structure: ((TTY-REGULAR . TTY-BOLD) . (GUI-REGULAR . GUI-BOLD))"
  :group 'nano-theme
  :type `(cons
          :tag "Regular and bold weights"
          (cons
           :tag "TTY"
           (choice :tag ,(propertize "  Regular" 'face 'regular)
                   :format "%t: %[%{WEIGHT%}%] %v"
                   (const :tag "Light" light)
                   (const :tag "Regular" regular)
                   (const :tag "Medium" medium)
                   (const :tag "Bold" bold))
           (choice :tag ,(propertize "     Bold" 'face 'bold)
                   :format "%t: %[%{WEIGHT%}%] %v"
                   (const :tag "Light" light)
                   (const :tag "Regular" regular)
                   (const :tag "Medium" medium)
                   (const :tag "Bold" bold)))
          (cons
           :tag "GUI"
           (choice :tag ,(propertize "  Regular" 'face 'regular)
                   :format "%t: %[%{WEIGHT%}%] %v"
                   (const :tag "Light" light)
                   (const :tag "Regular" regular)
                   (const :tag "Medium" medium)
                   (const :tag "Bold" bold))
           (choice :tag ,(propertize "     Bold" 'face 'bold)
                   :format "%t: %[%{WEIGHT%}%] %v"
                   (const :tag "Light" light)
                   (const :tag "Regular" regular)
                   (const :tag "Medium" medium)
                   (const :tag "Bold" bold)))))

(defconst nano-theme-mapping-type
  `(repeat
    (cons
     (symbol :tag "Face")
     (choice :tag "NANO"
             :format "%t: %[%{BASE%}%]  %v"

             (choice :tag ,(propertize "default" 'face 'nano-default)
                     :format "%t: %[%{VARIANT%}%]  %v"
                     (const :tag ,(propertize " regular "   'face 'nano-default)    nano-default)
                     (const :tag ,(propertize " strong "    'face 'nano-default-s)  nano-default-s)
                     (const :tag ,(propertize " highlight " 'face 'nano-default-h)  nano-default-h)
                     (const :tag ,(propertize " inverse "   'face 'nano-default-i)  nano-default-i))

             (choice :tag ,(propertize " strong " 'face 'nano-strong)
                     :format "%t: %[%{VARIANT%}%]  %v"
                     (const :tag ,(propertize " regular "   'face 'nano-strong)    nano-strong)
                     (const :tag ,(propertize " strong "    'face 'nano-strong-s)  nano-strong-s)
                     (const :tag ,(propertize " highlight " 'face 'nano-strong-h)  nano-strong-h)
                     (const :tag ,(propertize " inverse "   'face 'nano-strong-i)  nano-strong-i))

             (choice :tag ,(propertize "faded" 'face 'nano-faded)
                     :format "%t: %[%{VARIANT%}%]  %v"
                     (const :tag ,(propertize " regular "   'face 'nano-faded)    nano-faded)
                     (const :tag ,(propertize " strong "    'face 'nano-faded-s)  nano-faded-s)
                     (const :tag ,(propertize " highlight " 'face 'nano-faded-h)  nano-faded-h)
                     (const :tag ,(propertize " inverse "   'face 'nano-faded-i)  nano-faded-i))

             (choice :tag ,(propertize "popout" 'face 'nano-popout)
                     :format "%t: %[%{VARIANT%}%]  %v"
                     (const :tag ,(propertize " regular "   'face 'nano-popout)    nano-popout)
                     (const :tag ,(propertize " strong "    'face 'nano-popout-s)  nano-popout-s)
                     (const :tag ,(propertize " highlight " 'face 'nano-popout-h)  nano-popout-h)
                     (const :tag ,(propertize " inverse "   'face 'nano-popout-i)  nano-popout-i))

             (choice :tag ,(propertize "salient" 'face 'nano-salient)
                     :format "%t: %[%{VARIANT%}%]  %v"
                     (const :tag ,(propertize " regular "   'face 'nano-salient)    nano-salient)
                     (const :tag ,(propertize " strong "    'face 'nano-salient-s)  nano-salient-s)
                     (const :tag ,(propertize " highlight " 'face 'nano-salient-h)  nano-salient-h)
                     (const :tag ,(propertize " inverse "   'face 'nano-salient-i)  nano-salient-i))

             (choice :tag ,(propertize "subtle" 'face 'nano-subtle)
                     :format "%t: %[%{VARIANT%}%]  %v"
                     (const :tag ,(propertize " regular "   'face 'nano-subtle)    nano-subtle)
                     (const :tag ,(propertize " strong "    'face 'nano-subtle-s)  nano-subtle-s)
                     (const :tag ,(propertize " highlight " 'face 'nano-subtle-h)  nano-subtle-h)
                     (const :tag ,(propertize " inverse "   'face 'nano-subtle-i)  nano-subtle-i))

             (choice :tag ,(propertize "highlight" 'face 'nano-highlight)
                     :format "%t: %[%{VARIANT%}%]  %v"
                     (const :tag ,(propertize " regular "   'face 'nano-highlight)    nano-highlight)
                     (const :tag ,(propertize " strong "    'face 'nano-highlight-s)  nano-highlight-s)
                     (const :tag ,(propertize " highlight " 'face 'nano-highlight-h)  nano-highlight-h)
                     (const :tag ,(propertize " inverse "   'face 'nano-highlight-i)  nano-highlight-i))
             (choice :tag ,(propertize "critical" 'face 'nano-critical)
                     :format "%t: %[%{VARIANT%}%]  %v"
                     (const :tag ,(propertize " regular "   'face 'nano-critical)    nano-critical)
                     (const :tag ,(propertize " strong "    'face 'nano-critical-s)  nano-critical-s)
                     (const :tag ,(propertize " highlight " 'face 'nano-critical-h)  nano-critical-h)
                     (const :tag ,(propertize " inverse "   'face 'nano-critical-i)  nano-critical-i))
             )))
  "Custom type for mapping faces to nano faces, grouped by Base then Variant with live previews.")

(defun nano-theme-color-mix (c1 c2 &optional alpha)
  "Mix C1 and C2 colors by ALPHA."
  (let* ((alpha (or alpha 0.05))
         (rgb1 (color-name-to-rgb c1))
         (rgb2 (color-name-to-rgb c2)))
    (color-rgb-to-hex
     (+ (* (- 1 alpha) (nth 0 rgb1)) (* alpha (nth 0 rgb2)))
     (+ (* (- 1 alpha) (nth 1 rgb1)) (* alpha (nth 1 rgb2)))
     (+ (* (- 1 alpha) (nth 2 rgb1)) (* alpha (nth 2 rgb2))) 2)))


(defun nano-theme-defface (theme name &optional foreground background weight)
  "Set THEME face NAME and related '-s', '-h' and '-i' faces."
  (let ((attrs (apply #'append
                      (delq nil
                            (list
                             (when foreground `(:foreground ,foreground))
                             (when background `(:background ,background))
                             (when weight `(:weight ,weight)))))))

    ;; Base face
    (custom-declare-face (intern (concat (symbol-name name) ""))    '((t)) "")
    (custom-theme-set-faces
     theme
     `(,name ((t ,attrs))))

    ;; -s variant
    (custom-declare-face (intern (concat (symbol-name name) "-s"))  '((t)) "")
    (custom-theme-set-faces
     theme
     `(,(intern (concat (symbol-name name) "-s"))
        ((t ,(append attrs '(:weight bold))))))

    ;; -h variant
    (custom-declare-face (intern (concat (symbol-name name) "-h"))  '((t)) "")
    (let* ((fg (or foreground (face-foreground 'default)))
           (fg (nano-theme-color-mix (face-foreground 'default) fg 0.850))
           (bg (nano-theme-color-mix (face-background 'default) fg 0.075)))      
      (custom-theme-set-faces
       theme
       `(,(intern (concat (symbol-name name) "-h"))
         ((t ,(append attrs `(:foreground ,fg)
                            `(:background ,bg)
                            `(:extend t)))))))

    ;; -i variant
    (custom-declare-face (intern (concat (symbol-name name) "-i"))  '((t)) "")
    (let ((fg (or background (face-background 'default)))
          (bg (or foreground (face-foreground 'default))))
      (custom-theme-set-faces
       theme
       `(,(intern (concat (symbol-name name) "-i"))
         ((t ,(append attrs `(:foreground ,fg)
                            `(:background ,bg)
                            `(:extend t)
                            '(:weight bold)))))))))

(defun nano-theme-build-bases (theme)
  "Set base faces for the NANO light theme in THEME."

  (let* ((tty-weights (car nano-theme-weights))
         (gui-weights (cdr nano-theme-weights))
         (weights (if (display-graphic-p) gui-weights tty-weights))
         (regular (car weights))
         (bold (cdr weights))
	 (fg nil)
	 (bg nil))
    (cond
     ((eq theme 'nano-light)
      (setq fg "#000000" bg "#FFFFFF")
      (nano-theme-defface theme 'nano-default fg bg regular)    ;; Black / White
      (nano-theme-defface theme 'nano-strong  nil nil bold)     ;; Black bold
      (nano-theme-defface theme 'nano-highlight nil "#F9F9F9")  ;; Pale highlight
      (nano-theme-defface theme 'nano-subtle    nil "#ECEFF1")  ;; Blue Grey / L50
      (nano-theme-defface theme 'nano-faded     "#607D8B")      ;; Blue Grey / L500
      (nano-theme-defface theme 'nano-salient   "#673AB7")      ;; Deep Purple / L500
      (nano-theme-defface theme 'nano-popout    "#FFAB91")      ;; Deep Orange / L200
      (nano-theme-defface theme 'nano-critical  "#FF6F00"))     ;; Amber / L900

     ((eq theme 'nano-gray)
      (setq fg "#000000" bg "#ECEFF1")
      (nano-theme-defface theme 'nano-default fg bg regular)    ;; Black / Blue Grey L800
      (nano-theme-defface theme 'nano-strong nil nil bold)      ;; Black bold
      (nano-theme-defface theme 'nano-highlight nil "#F9F9F9")  ;; Pale highlight
      (nano-theme-defface theme 'nano-subtle    nil "#FFFFFF")  ;; Blue Grey L50
      (nano-theme-defface theme 'nano-faded     "#607D8B")      ;; Blue Grey L500
      (nano-theme-defface theme 'nano-salient   "#673AB7")      ;; Deep Purple L500
      (nano-theme-defface theme 'nano-popout    "#FFAB91")      ;; Deep Orange L200
      (nano-theme-defface theme 'nano-critical  "#FF6F00"))     ;; Amber L900

     ((eq theme 'nano-mono)
      (setq fg "#000000" bg "#FFFFFF")
      (nano-theme-defface theme 'nano-default fg bg regular)    ;; Black / White
      (nano-theme-defface theme 'nano-strong nil nil bold)      ;; Black bold
      (nano-theme-defface theme 'nano-highlight nil "#dddddd")  ;; Black / Light gray
      (nano-theme-defface theme 'nano-subtle fg "#999999")      ;; Black / Dark gray
      (nano-theme-defface theme 'nano-faded "#999999")          ;; Dark gray
      (nano-theme-defface theme 'nano-salient fg nil bold)      ;; Black bold
      (nano-theme-defface theme 'nano-popout fg nil bold)       ;; Black bold
      (nano-theme-defface theme 'nano-critical fg nil bold))    ;; Black bold

     ((eq theme 'nano-dark)
      (setq fg "#ECEFF4" bg "#2E3440")
      (nano-theme-defface theme 'nano-default fg bg regular)    ;; Polar Night 0 / Snow Storm
      (nano-theme-defface theme 'nano-strong nil nil bold)      ;; Polar Night 0
      (nano-theme-defface theme 'nano-highlight nil "#3B4252")  ;; Polar Night 1
      (nano-theme-defface theme 'nano-subtle    nil "#434C5E")  ;; Polar Night 2
      (nano-theme-defface theme 'nano-faded     "#677691")      ;; Faded
      (nano-theme-defface theme 'nano-salient   "#81A1C1")      ;; Frost 2
      (nano-theme-defface theme 'nano-popout    "#D08770")      ;; Aurora 1
      (nano-theme-defface theme 'nano-critical  "#EBCB8B"))     ;; Aurora 2
     (t
      (error "Unknown theme variant (%s)" theme)))

    (custom-theme-set-faces
     theme
     `(default ((t (:foreground ,fg :background ,bg :weight ,regular)))))))

(defun nano-theme-build-faces (theme)
  "Automatically generate inherited faces for THEME
from all variables listed in `nano-theme-packages`."
  (dolist (var nano-theme-packages)
    (let ((faces-alist (symbol-value var)))
      (when (listp faces-alist)
        (dolist (entry faces-alist)
          (let ((face (car entry))
                (base (cdr entry)))
            (when (symbolp face)
              (custom-theme-set-faces
               theme
               `(,face ((t (:inherit ,base))))))))))))

(defun nano-theme-build-ansi-term (theme)
  "Generate ansi color faces for THEME."

  (custom-theme-set-faces
   theme
   '(ansi-color-black          ((t (:inherit nano-default))))
   '(term-black                ((t (:inherit nano-default))))
   '(ansi-color-bold           ((t (:inherit nano-strong))))
   '(term-bold                 ((t (:inherit nano-strong))))
   '(ansi-color-bright-black   ((t (:inherit nano-strong))))
   '(term-bright-black         ((t (:inherit nano-strong))))
   '(ansi-color-faint          ((t (:inherit nano-faded))))
   '(term-faint                ((t (:inherit nano-faded))))
   '(ansi-color-fast-blink     ((t (:inherit nano-faded))))
   '(term-fast-blink           ((t (:inherit nano-faded))))
   '(ansi-color-slow-blink     ((t (:inherit nano-faded))))
   '(term-slow-blink           ((t (:inherit nano-faded))))
   '(ansi-color-inverse        ((t (:inherit nano-default-i))))
   '(ansi-color-italic         ((t (:slant italic))))
   '(term-italic               ((t (:slant italic))))
   '(ansi-color-underline      ((t (:underline t))))
   '(term-underline            ((t (:underline t))))

   '(ansi-color-blue           ((t (:foreground "#42A5F5"))))   ;; Material blue L400
   '(ansi-color-bright-blue    ((t (:foreground "#BBDEFB"))))   ;; Material blue L100
   '(ansi-color-cyan           ((t (:foreground "#26C6DA"))))   ;; Material cyan L400
   '(ansi-color-bright-cyan    ((t (:foreground "#B2EBF2"))))   ;; Material cyan L100
   '(ansi-color-green          ((t (:foreground "#66BB6A"))))   ;; Material green L400
   '(ansi-color-bright-green   ((t (:foreground "#C8E6C9"))))   ;; Material green L100
   '(ansi-color-magenta        ((t (:foreground "#AB47BC"))))   ;; Material purple L400
   '(ansi-color-bright-magenta ((t (:foreground "#E1BEE7"))))   ;; Material purple L100
   '(ansi-color-red            ((t (:foreground "#EF5350"))))   ;; Material red L400
   '(ansi-color-bright-red     ((t (:foreground "#FFCDD2"))))   ;; Material red L100
   '(ansi-color-white          ((t (:inherit nano-default))))   ;; Default white
   '(ansi-color-bright-white   ((t (:inherit nano-default))))   ;; Default white
   '(ansi-color-yellow         ((t (:foreground "#FFEE58"))))   ;; Material yellow L400
   '(ansi-color-bright-yellow  ((t (:foreground "#FFF9C4"))))   ;; Material yellow L100

   '(term-color-blue           ((t (:foreground "#42A5F5"))))   ;; Material blue L400
   '(term-color-bright-blue    ((t (:foreground "#BBDEFB"))))   ;; Material blue L100
   '(term-color-cyan           ((t (:foreground "#26C6DA"))))   ;; Material cyan L400
   '(term-color-bright-cyan    ((t (:foreground "#B2EBF2"))))   ;; Material cyan L100
   '(term-color-green          ((t (:foreground "#66BB6A"))))   ;; Material green L400
   '(term-color-bright-green   ((t (:foreground "#C8E6C9"))))   ;; Material green L100
   '(term-color-magenta        ((t (:foreground "#AB47BC"))))   ;; Material purple L400
   '(term-color-bright-magenta ((t (:foreground "#E1BEE7"))))   ;; Material purple L100
   '(term-color-red            ((t (:foreground "#EF5350"))))   ;; Material red L400
   '(term-color-bright-red     ((t (:foreground "#FFCDD2"))))   ;; Material red L100
   '(term-color-white          ((t (:inherit nano-default))))   ;; Default white
   '(term-color-bright-white   ((t (:inherit nano-default))))   ;; Default white
   '(term-color-yellow         ((t (:foreground "#FFEE58"))))   ;; Material yellow L400
   '(term-color-bright-yellow  ((t (:foreground "#FFF9C4")))))) ;; Material yellow L100

(defcustom nano-theme-emacs-completion
  '((completions-annotations        . nano-faded)
    (completions-common-part        . nano-salient)
    (completions-first-difference   . nano-critical)
    (completions-highlight          . nano-highlight)
    (completions-group-separator    . nano-default)
    (completions-group-title        . nano-strong))

  "Completion faces"
  :tag "(completion faces)"
  :type nano-theme-mapping-type
  :group 'nano-theme-emacs)

(defcustom nano-theme-emacs-font-lock
  '((font-lock-misc-punctuation-face       . nano-default)
    (font-lock-delimiter-face              . nano-default)
    (font-lock-bracket-face                . nano-default)
    (font-lock-punctuation-face            . nano-default)
    (font-lock-property-use-face           . nano-strong)
    (font-lock-property-name-face          . nano-strong)
    (font-lock-operator-face               . nano-default)
    (font-lock-number-face                 . nano-default)
    (font-lock-escape-face                 . nano-strong)
    (font-lock-regexp-grouping-construct   . nano-strong-s)
    (font-lock-regexp-grouping-backslash   . nano-strong-s)
    (font-lock-regexp-face                 . nano-faded-s)
    (font-lock-preprocessor-face           . nano-salient)
    (font-lock-negation-char-face          . nano-default)
    (font-lock-warning-face                . nano-popout)
    (font-lock-constant-face               . nano-salient-s)
    (font-lock-type-face                   . nano-salient)
    (font-lock-variable-use-face           . nano-strong)
    (font-lock-variable-name-face          . nano-strong)
    (font-lock-function-call-face          . nano-strong)
    (font-lock-function-name-face          . nano-strong)
    (font-lock-builtin-face                . nano-salient)
    (font-lock-keyword-face                . nano-salient)
    (font-lock-doc-markup-face             . nano-salient-s)
    (font-lock-doc-face                    . nano-faded)
    (font-lock-string-face                 . nano-faded-s)
    (font-lock-comment-delimiter-face      . nano-faded)
    (font-lock-comment-face                . nano-faded))

  "Font-lock faces"
  :tag "(font-lock faces)"
  :type nano-theme-mapping-type
  :group 'nano-theme-emacs)

(defcustom nano-theme-emacs-search
  '((isearch                    . nano-salient)
    (isearch-group-1            . nano-salient)
    (isearch-group-2            . nano-salient)
    (lazy-highlight             . nano-highlight)
    (isearch-fail               . nano-critical)
    (match                      . nano-highlight)
    (query-replace              . nano-highlight))
  "Faces used for search and match highlighting."
  :tag "(search faces)"
  :type nano-theme-mapping-type
  :group 'nano-theme-emacs)

(defcustom nano-theme-emacs-vc
  '((vc-ignored-state        . nano-faded)
    (vc-edited-state         . nano-default)
    (vc-missing-state        . nano-critical)
    (vc-removed-state        . nano-critical)
    (vc-conflict-state       . nano-critical)
    (vc-locally-added-state  . nano-salient)
    (vc-locked-state         . nano-popout)
    (vc-needs-update-state   . nano-popout)
    (vc-up-to-date-state     . nano-faded)
    (vc-state-base           . nano-default))
  "Faces used for VC status indicators."
  :tag "(vc faces)"
  :type nano-theme-mapping-type
  :group 'nano-theme-emacs)

(defcustom nano-theme-emacs-paren
  '((show-paren-match             . nano-faded-i)
    (show-paren-match-expression  . nano-faded-i)
    (show-paren-mismatch          . nano-critical-i))
  "Faces for parenthesis matching."
  :tag "(paren matching)"
  :type nano-theme-mapping-type
  :group 'nano-theme-emacs)

(defcustom nano-theme-emacs-widgets
  '((tty-menu-selected-face    . nano-faded-i)
    (tty-menu-enabled-face     . nano-subtle)
    (tty-menu-disabled-face    . nano-faded)
    (read-multiple-choice-face . nano-subtle)
    (help-key-binding          . nano-strong)
    (help-argument-name        . nano-default)
    (button                    . nano-salient)
    (abbrev-table-name         . nano-default)
    (menu                      . nano-subtle))
  "Faces for TTY menus, buttons and widget UI elements."
  :tag "(widgets)"
  :type nano-theme-mapping-type
  :group 'nano-theme-emacs)

(defcustom nano-theme-emacs-diagnostics
  '((success        . nano-salient)
    (warning        . nano-popout)
    (error          . nano-critical)
    (glyphless-char . nano-faded))
  "Faces for status, warnings, and errors."
  :tag "(diagnostic faces)"
  :type nano-theme-mapping-type
  :group 'nano-theme-emacs)

(defcustom nano-theme-emacs-ui
  '((mode-line                  . nano-subtle)
    (mode-line-active           . nano-subtle)
    (mode-line-inactive         . nano-faded)
    (mode-line-highlight        . nano-highlight)
    (mode-line-emphasis         . nano-strong)
    (mode-line-buffer-id        . nano-strong)
    (header-line                . nano-subtle)
    (header-line-highlight      . nano-highlight)
    (tab-line                   . nano-default)
    (tab-bar                    . nano-default)
    (tab-bar-tab                . nano-highlight)
    (tab-bar-tab-inactive       . nano-default)
    (tab-bar-tab-group-current  . nano-strong)
    (tab-bar-tab-group-inactive . nano-faded)
    (tab-bar-tab-ungrouped      . nano-default)
    (tool-bar                   . nano-subtle))
  "Faces for modeline, header-line, tab-line, tool-bar."
  :tag "(ui faces)"
  :type nano-theme-mapping-type
  :group 'nano-theme-emacs)

(defcustom nano-theme-emacs-borders
  '((border                     . nano-default)
    (internal-border            . nano-default)
    (child-frame-border         . nano-default)
    (vertical-border            . nano-default)
    (window-divider             . nano-default)
    (window-divider-first-pixel . nano-default)
    (window-divider-last-pixel  . nano-default)
    (scroll-bar                 . nano-default)
    (fringe                     . nano-default))
  "Faces for borders & window dividers."
  :tag "(border faces)"
  :type nano-theme-mapping-type
  :group 'nano-theme-emacs)

(defcustom nano-theme-emacs-glyphs
  '((nobreak-space          . nano-default)
    (nobreak-hyphen         . nano-default)
    (escape-glyph           . nano-default)
    (homoglyph              . nano-default)
    (fill-column-indicator  . nano-default))
  "Faces for invisible glyphs and whitespace indicators."
  :tag "(glyph faces)"
  :type nano-theme-mapping-type
  :group 'nano-theme-emacs)

(defcustom nano-theme-emacs-line-numbers
  '((line-number              . nano-faded)
    (line-number-current-line . nano-subtle)
    (line-number-major-tick   . nano-default)
    (line-number-minor-tick   . nano-default))
  "Faces for line numbers."
  :tag "(line number faces)"
  :type nano-theme-mapping-type
  :group 'nano-theme-emacs)

(defcustom nano-theme-emacs-selection
  '((region              . nano-subtle)
    (secondary-selection . nano-highlight)
    (highlight           . nano-highlight)
    (link                . nano-salient)
    (link-visited        . nano-faded)
    (shadow              . nano-faded)
    (trailing-whitespace . nano-critical))
  "Faces related to selection & highlighting."
  :tag "(selection & links)"
  :type nano-theme-mapping-type
  :group 'nano-theme-emacs)

(defcustom nano-theme-emacs-face-attributes
  '((variable-pitch        . nano-default)
    (variable-pitch-text   . nano-default)
    (fixed-pitch           . nano-default)
    (fixed-pitch-serif     . nano-default)
    (bold                  . nano-strong)
    (italic                . nano-default)
    (bold-italic           . nano-strong)
    (underline             . nano-default))
  "Faces describing font attributes and pitch."
  :tag "(attribute faces)"
  :type nano-theme-mapping-type
  :group 'nano-theme-emacs)

(defcustom nano-theme-emacs-misc
  '((tooltip                     . nano-default)
    (eldoc-highlight-function-argument . nano-salient)
    (elisp-shorthand-font-lock-face   . nano-default)
    (file-name-shadow           . nano-faded)
    (buffer-menu-buffer         . nano-default)
    (tabulated-list-fake-header . nano-faded)
    (mouse                      . nano-default)
    (cursor                     . nano-default)
    (next-error                 . nano-default)
    (next-error-message         . nano-default)
    (separator-line             . nano-default)
    (confusingly-reordered      . nano-default)
    (help-for-help-header       . nano-default)
    (ns-working-text-face       . nano-default)
    (mouse-drag-and-drop-region . nano-default))
  "Miscellaneous faces not fitting other groups."
  :tag "(misc faces)"
  :type nano-theme-mapping-type
  :group 'nano-theme-emacs)

(defcustom nano-theme-org-headings
  '((org-level-1                 . nano-strong)
    (org-level-2                 . nano-strong)
    (org-level-3                 . nano-strong)
    (org-level-4                 . nano-strong)
    (org-level-5                 . nano-default)
    (org-level-6                 . nano-default)
    (org-level-7                 . nano-default)
    (org-level-8                 . nano-default)
    (org-document-title          . nano-strong)
    (org-document-info           . nano-default)
    (org-document-info-keyword   . nano-faded)
    (org-meta-line               . nano-faded)
    (org-special-keyword         . nano-faded))
  "Org headings and document structure faces."
  :tag "(org headings)"
  :type nano-theme-mapping-type
  :group 'nano-theme-org)

(defcustom nano-theme-org-todo
  '((org-todo                        . nano-salient-s)
    (org-done                        . nano-faded-s)
    (org-headline-done               . nano-faded-s)
    (org-checkbox-statistics-todo    . nano-salient-s)
    (org-checkbox-statistics-done    . nano-faded-s))
  "Faces for TODO and DONE states in Org."
  :tag "(org todo)"
  :type nano-theme-mapping-type
  :group 'nano-theme-org)

(defcustom nano-theme-org-tags
  '((org-tag              . nano-salient)
    (org-priority         . nano-salient)
    (org-property-value   . nano-default)
    (org-column-title     . nano-strong)
    (org-column           . nano-default))
  "Faces for tags, priorities and property drawers."
  :tag "(org tags)"
  :type nano-theme-mapping-type
  :group 'nano-theme-org)

(defcustom nano-theme-org-links
  '((org-link               . nano-salient)
    (org-footnote           . nano-salient)
    (org-footnote-special   . nano-highlight)
    (org-target             . nano-faded))
  "Faces for links, footnotes and targets."
  :tag "(org links)"
  :type nano-theme-mapping-type
  :group 'nano-theme-org)

(defcustom nano-theme-org-dates
  '((org-date               . nano-default)
    (org-date-selected      . nano-highlight)
    (org-scheduled          . nano-default)
    (org-scheduled-today    . nano-salient)
    (org-upcoming-deadline  . nano-popout)
    (org-deadline           . nano-critical)
    (org-time-grid          . nano-faded))
  "Faces for Org timestamps and scheduling."
  :tag "(org dates)"
  :type nano-theme-mapping-type
  :group 'nano-theme-org)

(defcustom nano-theme-org-blocks
  '((org-block-begin-line    . nano-subtle-s)
    (org-block-end-line      . nano-subtle-s)
    (org-block               . nano-highlight)
    (org-code                . nano-default)
    (org-verbatim            . nano-default)
    (org-formula             . nano-salient)
    (org-table-formula       . nano-salient)
    (org-src                 . nano-default))
  "Faces for Org code blocks and verbatim elements."
  :tag "(org blocks)"
  :type nano-theme-mapping-type
  :group 'nano-theme-org)

(defcustom nano-theme-org-tables
  '((org-table                 . nano-default)
    (org-table-header          . nano-strong)
    (org-agenda-column-dateline . nano-default))
  "Faces for Org tables."
  :tag "(org tables)"
  :type nano-theme-mapping-type
  :group 'nano-theme-org)

(defcustom nano-theme-org-lists
  '((org-list-dt        . nano-strong)
    (org-checkbox       . nano-default)
    (org-indent         . nano-faded)
    (org-ellipsis       . nano-faded))
  "Faces for Org lists, bullets and indentation."
  :tag "(org lists)"
  :type nano-theme-mapping-type
  :group 'nano-theme-org)

(defcustom nano-theme-org-agenda
  '((org-agenda-structure        . nano-strong)
    (org-agenda-date             . nano-default)
    (org-agenda-date-weekend     . nano-faded)
    (org-agenda-date-today       . nano-salient)
    (org-agenda-done             . nano-faded)
    (org-agenda-clocking         . nano-highlight)
    (org-agenda-dimmed-todo-face . nano-faded)
    (org-agenda-restriction-lock . nano-default))
  "Faces used in Org Agenda buffers."
  :tag "(org agenda)"
  :type nano-theme-mapping-type
  :group 'nano-theme-org)

(defcustom nano-theme-org-habit
  '((org-habit-clear-face          . nano-default)
    (org-habit-ready-face          . nano-salient)
    (org-habit-alert-face          . nano-popout)
    (org-habit-overdue-face        . nano-critical)
    (org-habit-clear-future-face   . nano-faded)
    (org-habit-ready-future-face   . nano-default)
    (org-habit-alert-future-face   . nano-popout)
    (org-habit-overdue-future-face . nano-critical))
  "Faces used by Org Habit."
  :tag "(org habit)"
  :type nano-theme-mapping-type
  :group 'nano-theme-org)

(defcustom nano-theme-org-misc
  '((org-sexp-date        . nano-default)
    (org-quote            . nano-default)
    (org-mode-line-clock  . nano-default))
  "Assorted Org faces not covered in other groups."
  :tag "(org misc)"
  :type nano-theme-mapping-type
  :group 'nano-theme-org)

(defcustom nano-theme-dired
  '((dired-ignored              . nano-faded)
    (dired-special              . nano-highlight)
    (dired-broken-symlink       . nano-critical)
    (dired-symlink              . nano-salient)
    (dired-directory            . nano-strong)
    (dired-set-id               . nano-strong)
    (dired-perm-write           . nano-salient)
    (dired-warning              . nano-popout-s)
    (dired-flagged              . nano-critical-s)
    (dired-marked               . nano-salient-s)
    (dired-mark                 . nano-strong-s)
    (dired-header               . nano-salient-s))
  "Faces for Dired file manager."
  :tag "(dired)"
  :type nano-theme-mapping-type
  :group 'nano-theme-dired)

(defcustom nano-theme-diredfl
  '((diredfl-write-priv             . nano-default)
    (diredfl-read-priv              . nano-default)
    (diredfl-rare-priv              . nano-default)
    (diredfl-other-priv             . nano-default)
    (diredfl-no-priv                . nano-default)
    (diredfl-link-priv              . nano-default)
    (diredfl-exec-priv              . nano-default)
    (diredfl-dir-priv               . nano-default)

    (diredfl-flag-mark-line         . nano-salient-s)
    (diredfl-flag-mark              . nano-salient-s)

    (diredfl-tagged-autofile-name   . nano-default)
    (diredfl-symlink                . nano-default)

    (diredfl-number                 . nano-default)
    (diredfl-date-time              . nano-default)
    (diredfl-file-name              . nano-default)
    (diredfl-file-suffix            . nano-default)
    (diredfl-dir-name               . nano-strong)
    (diredfl-compressed-file-name   . nano-default)
    (diredfl-compressed-file-suffix . nano-default)

    (diredfl-ignored-file-name      . nano-faded)
    (diredfl-deletion-file-name     . nano-critical-s)
    (diredfl-autofile-name          . nano-default)
    (diredfl-executable-tag         . nano-default)
    (diredfl-dir-heading            . nano-strong)
    (diredfl-deletion               . nano-critical-s))

  "Faces for Dired font-lock."
  :tag "(diredfl)"
  :type nano-theme-mapping-type
  :group 'nano-theme-dired)

(defcustom nano-theme-stripes
  '((stripes . nano-highlight))
  "Faces for stripes."
  :tag "(stripes)"
  :type nano-theme-mapping-type
  :group 'nano-theme-sripes)

(defcustom nano-theme-vertico
  '((vertico-current             . nano-subtle)
    (vertico-group-separator     . nano-faded)
    (vertico-group-title         . nano-faded)
    (vertico-multiline           . nano-faded))
  "Faces for Vertico (completion UI)."
  :tag "(vertico)"
  :type nano-theme-mapping-type
  :group 'nano-theme-vertico)

(defcustom nano-theme-marginalia-file
  '((marginalia-file-priv-rare         . nano-faded)
    (marginalia-file-priv-other        . nano-faded)
    (marginalia-file-priv-exec         . nano-salient)
    (marginalia-file-priv-write        . nano-salient)
    (marginalia-file-priv-read         . nano-salient)
    (marginalia-file-priv-link         . nano-salient)
    (marginalia-file-priv-dir          . nano-strong)
    (marginalia-file-priv-no           . nano-strong)
    (marginalia-file-owner            . nano-salient)
    (marginalia-file-name             . nano-strong))
  "Faces for file-related annotations in Marginalia."
  :tag "(marginalia-file)"
  :type nano-theme-mapping-type
  :group 'nano-theme-marginalia)

(defcustom nano-theme-marginalia-misc
  '((marginalia-string                . nano-faded)
    (marginalia-number                . nano-faded)
    (marginalia-size                  . nano-faded)
    (marginalia-installed             . nano-faded)
    (marginalia-symbol                . nano-faded)
    (marginalia-function              . nano-faded)
    (marginalia-true                  . nano-salient)
    (marginalia-null                  . nano-faded)
    (marginalia-value                 . nano-faded)
    (marginalia-off                   . nano-faded)
    (marginalia-on                    . nano-salient)
    (marginalia-lighter               . nano-faded)
    (marginalia-char                  . nano-faded)
    (marginalia-type                  . nano-faded)
    (marginalia-key                   . nano-salient))
  "Faces for miscellaneous annotations in Marginalia."
  :tag "(marginalia-misc)"
  :type nano-theme-mapping-type
  :group 'nano-theme-marginalia)

(defcustom nano-theme-marginalia-metadata
  '((marginalia-modified              . nano-strong)
    (marginalia-archive               . nano-strong)
    (marginalia-version               . nano-strong)
    (marginalia-date                  . nano-faded)
    (marginalia-mode                  . nano-faded)
    (marginalia-list                  . nano-strong))
  "Faces for metadata-related annotations in Marginalia."
  :tag "(marginalia-metadata)"
  :type nano-theme-mapping-type
  :group 'nano-theme-marginalia)

(defcustom nano-theme-magit-diff-faces
  '((magit-diff-added                   . nano-faded)
    (magit-diff-removed                 . nano-faded)
    (magit-diff-context                 . nano-faded)
    (magit-diff-added-highlight         . nano-salient-h)
    (magit-diff-removed-highlight       . nano-critical-h)
    (magit-diff-context-highlight       . nano-highlight)
    (magit-diff-file-heading            . nano-strong)
    (magit-diff-file-heading-highlight  . nano-salient-s)
    (magit-diff-file-heading-selection  . nano-faded-i)
    (magit-diff-hunk-heading            . nano-faded-i)
    (magit-diff-hunk-heading-highlight  . nano-default-i)
    (magit-diff-hunk-heading-selection  . nano-highlight))
  "Magit diff hunks and file headings."
  :tag "(magit diffs)"
  :type nano-theme-mapping-type
  :group 'nano-theme-magit)

(defcustom nano-theme-magit-section-faces
  '((magit-section-heading           . nano-strong)
    (magit-section-highlight         . nano-salient-s)
    (magit-section-heading-selection . nano-salient-i)
    (magit-section-secondary-heading . nano-salient-h))
  "Magit buffer sections."
  :tag "(magit sections)"
  :type nano-theme-mapping-type
  :group 'nano-theme-magit)

(defcustom nano-theme-magit-refs-faces
  '((magit-branch-local   . nano-salient)
    (magit-branch-remote  . nano-salient)
    (magit-branch-current . nano-strong)
    (magit-tag            . nano-salient))
  "Magit refs (branches, tags, remotes)."
  :tag "(magit refs)"
  :type nano-theme-mapping-type
  :group 'nano-theme-magit)

(defcustom nano-theme-magit-blame-faces
  '((magit-blame-highlight . nano-faded)
    (magit-blame-margin    . nano-faded)
    (magit-blame-dimmed    . nano-faded)
    (magit-blame-heading   . nano-default)
    (magit-blame-summary   . nano-default)
    (magit-blame-hash      . nano-default)
    (magit-blame-name      . nano-default)
    (magit-blame-date      . nano-default))
  "Magit blame view."
  :tag "(magit blame)"
  :type nano-theme-mapping-type
  :group 'nano-theme-magit)

(defcustom nano-theme-magit-sequence-faces
  '((magit-sequence-pick  . nano-salient)
    (magit-sequence-stop  . nano-strong)
    (magit-sequence-part  . nano-default)
    (magit-sequence-head  . nano-strong)
    (magit-sequence-drop  . nano-critical)
    (magit-sequence-done  . nano-strong)
    (magit-sequence-onto  . nano-strong))
  "Magit sequence (rebase / cherry-pick) sections."
  :tag "(magit sequence)"
  :type nano-theme-mapping-type
  :group 'nano-theme-magit)

(defcustom nano-theme-corfu-faces
  '((corfu-default     . nano-subtle)
    (corfu-current     . nano-faded-i)
    (corfu-border      . nano-faded-i)
    (corfu-annotations . nano-faded)
    (corfu-bar         . nano-default-i)
    (corfu-info        . nano-salient)
    (corfu-warning     . nano-popout)
    (corfu-error       . nano-critical))
  "Corfu faces."
  :tag "(corfu faces)"
  :type nano-theme-mapping-type
  :group 'nano-theme-corfu)

(defcustom nano-theme-mu4e-headers
  '((mu4e-header-face             . nano-default)
    (mu4e-header-title-face       . nano-strong)
    (mu4e-header-value-face       . nano-default)
    (mu4e-header-key-face         . nano-default)
    (mu4e-header-field-face       . nano-default)
    (mu4e-header-marks-face       . nano-salient)
    (mu4e-header-highlight-face   . nano-highlight)
    (mu4e-special-header-value-face . nano-default)
    (mu4e-related-face            . nano-default)
    (mu4e-forwarded-face          . nano-faded)
    (mu4e-replied-face            . nano-faded)
    (mu4e-flagged-face            . nano-salient)
    (mu4e-draft-face              . nano-faded)
    (mu4e-trashed-face            . nano-faded)
    (mu4e-unread-face             . nano-strong))
  "Mu4e message list and thread view."
  :tag "(mu4e headers)"
  :type nano-theme-mapping-type
  :group 'nano-theme-mu4e)

(defcustom nano-theme-mu4e-view
  '((mu4e-view-body-face          . nano-default)
    (mu4e-url-number-face         . nano-salient)
    (mu4e-link-face               . nano-salient)
    (mu4e-contact-face            . nano-salient)
    (mu4e-region-code             . nano-faded)
    (mu4e-ok-face                 . nano-salient)
    (mu4e-warning-face            . nano-critical)
    (mu4e-system-face             . nano-faded)
    (mu4e-footer-face             . nano-faded)
    (mu4e-title-face              . nano-strong)
    (mu4e-highlight-face          . nano-highlight))
  "Mu4e message reading view."
  :tag "(mu4e view)"
  :type nano-theme-mapping-type
  :group 'nano-theme-mu4e)

(defcustom nano-theme-mu4e-compose
  '((mu4e-compose-separator-face  . nano-faded)
    (mu4e-context-face            . nano-default)
    (mu4e-thread-fold-face        . nano-faded)
    (mu4e-modeline-face           . nano-default))
  "Mu4e compose / editor and context."
  :tag "(mu4e compose)"
  :type nano-theme-mapping-type
  :group 'nano-theme-mu4e)

(defcustom nano-theme-gnus-citations
  '((gnus-cite-1   . nano-faded)
    (gnus-cite-2   . nano-faded)
    (gnus-cite-3   . nano-faded)
    (gnus-cite-4   . nano-faded)
    (gnus-cite-5   . nano-faded)
    (gnus-cite-6   . nano-faded)
    (gnus-cite-7   . nano-faded)
    (gnus-cite-8   . nano-faded)
    (gnus-cite-9   . nano-faded)
    (gnus-cite-10  . nano-faded)
    (gnus-cite-11  . nano-faded)
    (gnus-cite-attribution . nano-faded))
  "Citation / quoted text faces in Gnus messages."
  :tag "(gnus citations)"
  :type nano-theme-mapping-type
  :group 'nano-theme-gnus)

(defcustom nano-theme-gnus-headers
  '((gnus-header             . nano-default)
    (gnus-header-name        . nano-strong)
    (gnus-header-from        . nano-strong)
    (gnus-header-subject     . nano-strong)
    (gnus-header-newsgroups  . nano-faded)
    (gnus-header-content     . nano-default)
    (gnus-signature          . nano-faded)
    (gnus-button             . nano-salient))
  "Gnus message header faces."
  :tag "(gnus headers)"
  :type nano-theme-mapping-type
  :group 'nano-theme-gnus)

(defcustom nano-theme-gnus-summary
  '((gnus-summary-normal-read       . nano-default)
    (gnus-summary-low-read          . nano-faded)
    (gnus-summary-high-read         . nano-strong)
    (gnus-summary-normal-unread     . nano-strong)
    (gnus-summary-low-unread        . nano-strong)
    (gnus-summary-high-unread       . nano-strong)
    (gnus-summary-normal-undownloaded . nano-faded)
    (gnus-summary-low-undownloaded    . nano-faded)
    (gnus-summary-high-undownloaded   . nano-faded)
    (gnus-summary-normal-ancient    . nano-faded)
    (gnus-summary-low-ancient       . nano-faded)
    (gnus-summary-high-ancient      . nano-faded)
    (gnus-summary-normal-ticked     . nano-salient)
    (gnus-summary-low-ticked        . nano-salient)
    (gnus-summary-high-ticked       . nano-salient)
    (gnus-summary-cancelled         . nano-faded)
    (gnus-summary-selected          . nano-highlight))
  "Gnus summary (message list) faces."
  :tag "(gnus summary)"
  :type nano-theme-mapping-type
  :group 'nano-theme-gnus)

(defcustom nano-theme-gnus-groups
  '((gnus-group-mail-1        . nano-strong)
    (gnus-group-mail-1-empty  . nano-faded)
    (gnus-group-mail-2        . nano-strong)
    (gnus-group-mail-2-empty  . nano-faded)
    (gnus-group-mail-3        . nano-salient)
    (gnus-group-mail-3-empty  . nano-faded)
    (gnus-group-mail-low      . nano-default)
    (gnus-group-mail-low-empty . nano-faded)
    (gnus-group-news-1        . nano-strong)
    (gnus-group-news-1-empty  . nano-faded)
    (gnus-group-news-2        . nano-strong)
    (gnus-group-news-2-empty  . nano-faded)
    (gnus-group-news-3        . nano-salient)
    (gnus-group-news-3-empty  . nano-faded)
    (gnus-group-news-4        . nano-salient)
    (gnus-group-news-4-empty  . nano-faded)
    (gnus-group-news-5        . nano-faded)
    (gnus-group-news-5-empty  . nano-faded)
    (gnus-group-news-6        . nano-faded)
    (gnus-group-news-6-empty  . nano-faded))
  "Gnus group listing faces."
  :tag "(gnus groups)"
  :type nano-theme-mapping-type
  :group 'nano-theme-gnus)

(defcustom nano-theme-gnus-emphasis
  '((gnus-emphasis-bold               . nano-strong)
    (gnus-emphasis-italic             . nano-faded)
    (gnus-emphasis-bold-italic        . nano-strong)
    (gnus-emphasis-underline          . nano-faded)
    (gnus-emphasis-underline-bold     . nano-strong)
    (gnus-emphasis-underline-italic   . nano-faded)
    (gnus-emphasis-underline-bold-italic . nano-strong)
    (gnus-emphasis-strikethru         . nano-faded)
    (gnus-emphasis-highlight-words    . nano-highlight))
  "Gnus emphasis faces inside messages."
  :tag "(gnus emphasis)"
  :type nano-theme-mapping-type
  :group 'nano-theme-gnus)

(defcustom nano-theme-gnus-misc
  '((gnus-splash . nano-faded))
  "Miscellaneous Gnus faces (splash screen)."
  :tag "(gnus misc)"
  :type nano-theme-mapping-type
  :group 'nano-theme-gnus)

(defcustom nano-theme-ledger-reconciler
  '((ledger-reconcile-last-balance-equals-target-face . nano-strong)
    (ledger-font-reconciler-pending-face                . nano-faded)
    (ledger-font-reconciler-cleared-face                . nano-default)
    (ledger-font-reconciler-uncleared-face              . nano-faded))
  "Ledger reconciler and balance faces."
  :tag "(ledger reconciler)"
  :type nano-theme-mapping-type
  :group 'nano-theme-ledger)

(defcustom nano-theme-ledger-postings
  '((ledger-font-posting-date-face                  . nano-salient)
    (ledger-font-posting-amount-face                . nano-salient-s)
    (ledger-font-posting-amount-pending-face        . nano-popout)
    (ledger-font-posting-amount-cleared-face        . nano-faded)
    (ledger-font-posting-account-face               . nano-default)
    (ledger-font-posting-account-pending-face       . nano-faded)
    (ledger-font-posting-account-cleared-face       . nano-default)
    (ledger-font-xact-highlight-face                . nano-subtle)
    (ledger-font-xact-pending-face                  . nano-popout)
    (ledger-font-xact-cleared-face                  . nano-faded)
    (ledger-font-periodic-xact-face                 . nano-salient-s)
    (ledger-font-auto-xact-face                     . nano-faded))
  "Ledger transaction / posting faces."
  :tag "(ledger postings)"
  :type nano-theme-mapping-type
  :group 'nano-theme-ledger)

(defcustom nano-theme-ledger-directives
  '((ledger-font-define-body-face       . nano-default)
    (ledger-font-define-name-face       . nano-strong)
    (ledger-font-define-directive-face  . nano-salient)
    (ledger-font-D-directive-face       . nano-salient)
    (ledger-font-include-filename-face  . nano-faded)
    (ledger-font-include-directive-face . nano-faded)
    (ledger-font-end-directive-face     . nano-default)
    (ledger-font-assert-condition-face  . nano-faded)
    (ledger-font-assert-directive-face  . nano-default)
    (ledger-font-alias-definition-face  . nano-default)
    (ledger-font-alias-directive-face   . nano-default)
    (ledger-font-apply-tag-face         . nano-faded)
    (ledger-font-apply-account-face     . nano-default)
    (ledger-font-apply-directive-face   . nano-default)
    (ledger-font-default-directive-face . nano-default)
    (ledger-font-capture-regex-face     . nano-faded)
    (ledger-font-capture-account-face   . nano-default)
    (ledger-font-capture-directive-face . nano-default)
    (ledger-font-C-amount-face          . nano-default)
    (ledger-font-C-directive-face       . nano-salient)
    (ledger-font-check-condition-face   . nano-faded)
    (ledger-font-check-directive-face   . nano-default)
    (ledger-font-expr-expression-face   . nano-default)
    (ledger-font-expr-directive-face    . nano-salient)
    (ledger-font-fixed-price-face       . nano-default)
    (ledger-font-fixed-commodity-face   . nano-salient-s)
    (ledger-font-fixed-directive-face   . nano-default)
    (ledger-font-price-face             . nano-default)
    (ledger-font-price-symbol-face      . nano-salient-s)
    (ledger-font-price-date-face        . nano-default)
    (ledger-font-price-directive-face   . nano-default)
    (ledger-font-commodity-format-face  . nano-default)
    (ledger-font-format-directive-face  . nano-default)
    (ledger-font-commodity-name-face    . nano-salient-s)
    (ledger-font-commodity-directive-face . nano-default))
  "Ledger directives and keyword faces."
  :tag "(ledger directives)"
  :type nano-theme-mapping-type
  :group 'nano-theme-ledger)

(defcustom nano-theme-ledger-entities
  '((ledger-font-payee-name-face        . nano-strong)
    (ledger-font-payee-directive-face   . nano-default)
    (ledger-font-payee-regex-face       . nano-faded)
    (ledger-font-payee-pending-face     . nano-popout)
    (ledger-font-payee-cleared-face     . nano-faded)
    (ledger-font-payee-uncleared-face   . nano-strong)
    (ledger-font-account-name-face      . nano-strong)
    (ledger-font-account-directive-face . nano-default)
    (ledger-font-bucket-account-face    . nano-default)
    (ledger-font-bucket-directive-face  . nano-default))
  "Ledger payees, accounts, and bucket faces."
  :tag "(ledger entities)"
  :type nano-theme-mapping-type
  :group 'nano-theme-ledger)

(defcustom nano-theme-ledger-misc
  '((ledger-font-comment-face          . nano-faded)
    (ledger-font-note-text-face        . nano-default)
    (ledger-font-note-directive-face   . nano-salient)
    (ledger-font-code-face             . nano-faded)
    (ledger-font-report-clickable-face . nano-highlight)
    (ledger-font-N-symbol-face         . nano-faded)
    (ledger-font-N-directive-face      . nano-faded))
  "Miscellaneous Ledger faces (comments, notes, code, symbols)."
  :tag "(ledger misc)"
  :type nano-theme-mapping-type
  :group 'nano-theme-ledger)

(defcustom nano-theme-elfeed-search
  '((elfeed-search-title-face        . nano-default)
    (elfeed-search-unread-title-face . nano-strong)
    (elfeed-search-feed-face         . nano-default)
    (elfeed-search-tag-face          . nano-salient)
    (elfeed-search-date-face         . nano-default)
    (elfeed-search-unread-count-face . nano-strong)
    (elfeed-search-last-update-face  . nano-faded)
    (elfeed-search-filter-face       . nano-salient))
  "Faces used in the Elfeed search buffer."
  :tag "(elfeed search)"
  :type nano-theme-mapping-type
  :group 'nano-theme-elfeed)

(defcustom nano-theme-elfeed-log
  '((elfeed-log-debug-level-face   . nano-faded)
    (elfeed-log-info-level-face    . nano-default)
    (elfeed-log-warn-level-face    . nano-salient)
    (elfeed-log-error-level-face   . nano-critical)
    (elfeed-log-date-face          . nano-faded))
  "Faces used in the Elfeed log buffer."
  :tag "(elfeed log)"
  :type nano-theme-mapping-type
  :group 'nano-theme-elfeed)

(defcustom nano-theme-deft
  '((deft-time-face                 . nano-faded)
    (deft-title-face                . nano-strong)
    (deft-summary-face              . nano-faded)
    (deft-header-face               . nano-strong)
    (deft-filter-string-error-face  . nano-critical)
    (deft-filter-string-face        . nano-salient)
    (deft-separator-face            . nano-faded))
  "Faces for Deft notes."
  :tag "(deft)"
  :type nano-theme-mapping-type
  :group 'nano-theme-deft)

(defcustom nano-theme-message-headers
  '((message-header-name       . nano-default)
    (message-header-other      . nano-salient)
    (message-header-xheader    . nano-default)
    (message-header-newsgroups . nano-salient)
    (message-header-subject    . nano-strong)
    (message-header-cc         . nano-default)
    (message-header-to         . nano-default))
  "Faces used for message headers."
  :tag "(message headers)"
  :type nano-theme-mapping-type
  :group 'nano-theme-message)

;; ── Cited text / quotations
(defcustom nano-theme-message-cited
  '((message-cited-text-1 . nano-faded)
    (message-cited-text-2 . nano-faded)
    (message-cited-text-3 . nano-faded)
    (message-cited-text-4 . nano-faded))
  "Faces used for cited / quoted text in messages."
  :tag "(message cited text)"
  :type nano-theme-mapping-type
  :group 'nano-theme-message)

;; ── Separators / misc
(defcustom nano-theme-message-misc
  '((message-separator           . nano-subtle)
    (message-signature-separator . nano-faded)
    (message-mml                 . nano-default))
  "Miscellaneous message faces (separators, MML)."
  :tag "(message misc)"
  :type nano-theme-mapping-type
  :group 'nano-theme-message)

(defcustom nano-theme-emacs-outline
  '((outline-1 . nano-strong)
    (outline-2 . nano-strong)
    (outline-3 . nano-strong)
    (outline-4 . nano-strong)
    (outline-5 . nano-default)
    (outline-6 . nano-default)
    (outline-7 . nano-default)
    (outline-8 . nano-default))
  "Faces used for outline headings (levels 1-8)."
  :tag "(outline headings)"
  :type nano-theme-mapping-type
  :group 'nano-theme-emacs)

(defcustom nano-theme-epa
  '((epa-field-body                 . nano-default)
    (epa-field-name                 . nano-strong)
    (epa-mark                       . nano-salient)
    (epa-string                     . nano-popout)
    (epa-validity-disabled          . nano-faded)
    (epa-validity-high              . nano-strong)
    (epa-validity-medium            . nano-default)
    (epa-validity-low               . nano-faded))
  "EPA (Enigmail)"
  :tag "(epa)"
  :type nano-theme-mapping-type
  :group 'nano-theme-epa)

(defcustom nano-theme-popup
  '((popup-face                       . nano-highlight)
    (popup-isearch-match              . nano-popout)
    (popup-menu-face                  . nano-subtle)
    (popup-menu-mouse-face            . nano-faded-i)
    (popup-menu-selection-face        . nano-salient-i)
    (popup-menu-summary-face          . nano-faded)
    (popup-scroll-bar-background-face . nano-subtle)
    (popup-scroll-bar-foreground-face . nano-subtle)
    (popup-summary-face               . nano-faded)
    (popup-tip-face                   . nano-popout-i))
  "Popup"
  :tag "(popup)"
  :type nano-theme-mapping-type
  :group 'nano-theme-popup)

(defcustom nano-theme-diff
  '((diff-header                    . nano-faded)
    (diff-file-header               . nano-strong)
    (diff-context                   . nano-default)
    (diff-removed                   . nano-faded)
    (diff-changed                   . nano-popout)
    (diff-added                     . nano-salient)
    (diff-refine-added              . nano-salient-s)
    (diff-refine-changed            . nano-popout)
    (diff-refine-removed            . nano-critical-i))
  "Diff"
  :tag "(diff)"
  :type nano-theme-mapping-type
  :group 'nano-theme-diff)

(defcustom nano-theme-icomplete
  '((icomplete-first-match          . nano-strong)
    (icomplete-selected-match       . nano-strong)
    (icomplete-section              . nano-strong))
  "Icomplete"
  :tag "(icomplete)"
  :type nano-theme-mapping-type
  :group 'nano-theme-icomplete)


(defcustom nano-theme-markdown-structure
  '((markdown-header-face-1            . nano-strong)
    (markdown-header-face-2            . nano-strong)
    (markdown-header-face-3            . nano-strong)
    (markdown-header-face-4            . nano-strong)
    (markdown-header-face-5            . nano-strong)
    (markdown-header-face-6            . nano-strong)
    (markdown-header-face              . nano-strong)
    (markdown-header-rule-face         . nano-subtle)
    (markdown-header-delimiter-face    . nano-subtle)
    (markdown-hr-face                  . nano-subtle)
    (markdown-highlighting-face        . nano-popout)
    (markdown-list-face                . nano-default))
  "Faces for headers and structural elements."
  :tag "(markdown structure)"
  :type nano-theme-mapping-type
  :group 'nano-theme-markdown)

(defcustom nano-theme-markdown-semantic
  '((markdown-bold-face                . nano-strong)
    (markdown-italic-face              . nano-faded)
    (markdown-bold-italic-face         . nano-strong)
    (markdown-strike-through-face      . nano-faded)
    (markdown-markup-face              . nano-faded)
    (markdown-comment-face             . nano-faded)
    (markdown-line-break-face          . nano-subtle))
  "Faces for emphasis, inline markup, and semantic elements."
  :tag "(markdown semantic)"
  :type nano-theme-mapping-type
  :group 'nano-theme-markdown)

(defcustom nano-theme-markdown-code
  '((markdown-code-face                . nano-popout)
    (markdown-inline-code-face         . nano-popout)
    (markdown-pre-face                 . nano-subtle)
    (markdown-language-info-face       . nano-popout)
    (markdown-language-keyword-face    . nano-popout)
    (markdown-math-face                . nano-popout))
  "Faces for code, syntax, and block elements."
  :tag "(markdown code)"
  :type nano-theme-mapping-type
  :group 'nano-theme-markdown)

(defcustom nano-theme-markdown-links
  '((markdown-url-face                 . nano-salient)
    (markdown-plain-url-face           . nano-salient)
    (markdown-link-face                . nano-salient)
    (markdown-link-title-face          . nano-salient)
    (markdown-missing-link-face        . nano-critical)
    (markdown-footnote-text-face       . nano-default)
    (markdown-footnote-marker-face     . nano-popout)
    (markdown-reference-face           . nano-faded)
    (markdown-metadata-key-face        . nano-salient)
    (markdown-metadata-value-face      . nano-default))
  "Faces for links, footnotes, references, and metadata."
  :tag "(markdown links/metadata)"
  :type nano-theme-mapping-type
  :group 'nano-theme-markdown)

(defcustom nano-theme-shr
  '((shr-h1               . nano-strong)
    (shr-h2               . nano-strong)
    (shr-h3               . nano-strong)
    (shr-h4               . nano-strong)
    (shr-h5               . nano-strong)
    (shr-h6               . nano-strong)
    (shr-abbreviation     . nano-faded)
    (shr-code             . nano-salient)
    (shr-strike-through   . nano-faded)
    (shr-sup              . nano-default)
    (shr-selected-link    . nano-salient)
    (shr-link             . nano-salient)
    (shr-sliced-image     . nano-default)
    (shr-mark             . nano-popout)
    (shr-text             . nano-default))
  "SHR faces"
  :tag "(shr faces)"
  :type nano-theme-mapping-type
  :group 'nano-theme-shr)

(defcustom nano-theme-elpher
  '((elpher-gemini-preformatted-toggle  . nano-subtle)
    (elpher-gemini-preformatted         . nano-default)
    (elpher-gemini-quoted               . nano-faded)
    (elpher-gemini-heading1             . nano-strong)
    (elpher-gemini-heading2             . nano-strong)
    (elpher-gemini-heading3             . nano-strong)
    (elpher-margin-brackets             . nano-subtle)
    (elpher-margin-key                   . nano-salient)
    (elpher-unknown                      . nano-critical)
    (elpher-binary                       . nano-faded)
    (elpher-telnet                       . nano-popout)
    (elpher-other-url                     . nano-salient)
    (elpher-gemini                        . nano-default)
    (elpher-html                          . nano-default)
    (elpher-search                        . nano-salient)
    (elpher-image                          . nano-popout)
    (elpher-info                           . nano-subtle)
    (elpher-text                           . nano-default)
    (elpher-index                          . nano-strong))
  "Elpher faces (Gemini/HTML/Telnet)"
  :tag "(elpher faces)"
  :type nano-theme-mapping-type
  :group 'nano-theme-elpher)

(defcustom nano-theme-buffer-box
  '((buffer-box-face-active   . nano-default)
    (buffer-box-face-inactive . nano-faded))

  "Buffer box faces"
  :tag "(buffer-box faces)"
  :type nano-theme-mapping-type
  :group 'nano-theme-buffer-box)

(defconst nano-theme-packages
  '(
    ;; Core Emacs
    nano-theme-emacs-completion
    nano-theme-emacs-font-lock
    nano-theme-emacs-search
    nano-theme-emacs-vc
    nano-theme-emacs-paren
    nano-theme-emacs-widgets
    nano-theme-emacs-diagnostics
    nano-theme-emacs-ui
    nano-theme-emacs-outline
    nano-theme-emacs-borders
    nano-theme-emacs-glyphs
    nano-theme-emacs-line-numbers
    nano-theme-emacs-selection
    nano-theme-emacs-face-attributes
    nano-theme-emacs-misc

    ;; Org mode
    nano-theme-org-headings
    nano-theme-org-todo
    nano-theme-org-tags
    nano-theme-org-links
    nano-theme-org-dates
    nano-theme-org-blocks
    nano-theme-org-tables
    nano-theme-org-lists
    nano-theme-org-agenda
    nano-theme-org-habit
    nano-theme-org-misc

    ;; Dired
    nano-theme-dired

    ;; Diredfl
    nano-theme-diredfl

    ;; Vertico
    nano-theme-vertico

    ;; Marginalia
    nano-theme-marginalia-file
    nano-theme-marginalia-misc
    nano-theme-marginalia-metadata

    ;; Magit
    nano-theme-magit-diff-faces
    nano-theme-magit-section-faces
    nano-theme-magit-refs-faces
    nano-theme-magit-blame-faces
    nano-theme-magit-sequence-faces

    ;; Corfu
    nano-theme-corfu-faces

    ;; Mu4e
    nano-theme-mu4e-headers
    nano-theme-mu4e-view
    nano-theme-mu4e-compose

    ;; Gnus
    nano-theme-gnus-citations
    nano-theme-gnus-headers
    nano-theme-gnus-summary
    nano-theme-gnus-groups
    nano-theme-gnus-emphasis
    nano-theme-gnus-misc

    ;; Elfeed
    nano-theme-elfeed-search
    nano-theme-elfeed-log

    ;; Deft
    nano-theme-deft

    ;; Ledger
    nano-theme-ledger-reconciler
    nano-theme-ledger-postings
    nano-theme-ledger-directives
    nano-theme-ledger-entities
    nano-theme-ledger-misc

    ;; Message
    nano-theme-message-headers
    nano-theme-message-cited
    nano-theme-message-misc

    ;; EPA
    nano-theme-epa

    ;; Popup
    nano-theme-popup

    ;; Diff
    nano-theme-diff

    ;; Icomplete
    nano-theme-icomplete

    ;; Markdown
    nano-theme-markdown-structure
    nano-theme-markdown-semantic
    nano-theme-markdown-code
    nano-theme-markdown-links

    ;; SHR
    nano-theme-shr

    ;; Elpher
    nano-theme-elpher

    ;; Buffer box
    nano-theme-buffer-box
)

  "List of variables holding face → base-face mappings for the Nano theme.")

;;;###autoload
(when load-file-name
 (add-to-list 'custom-theme-load-path
              (file-name-as-directory (file-name-directory load-file-name))))

(provide 'nano-theme-support)
;;; nano-theme-support.el ends here
