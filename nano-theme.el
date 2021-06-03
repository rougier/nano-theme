;; ---------------------------------------------------------------------
;; GNU Emacs / N Λ N O theme
;; Copyright (C) 2021 - Nicolas P. Rougier
;;
;; This program is free software; you can redistribute it and/or modify
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
;; ---------------------------------------------------------------------

(deftheme nano
  "N Λ N O theme")

(defgroup nano nil
  "N Λ N O theme.")

(defgroup nano-light nil
  "Color palette for light theme" :group 'nano)

(defgroup nano-dark nil
  "Color palette for dark theme" :group 'nano)

(defcustom nano-layout t
  "Whether to use recommended nano layout settings.

Layout setting concerns mostly frame size and margin and is only
active with 'nano-light and 'nano-dark functions."
  :type 'boolean :group 'nano)

(defface nano-mono
  '((t (:family "Roboto Mono"
	:height 140
	:weight light)))
  "Default monospaced font (Roboto Mono Light, 14pt)."
  :group 'nano)

(defface nano-mono-alt
  '((t (:family "Fira Code"
	:height 140
	:weight light)))
  "Alternative monospaced font (Fira Code Light, 14pt)."
  :group 'nano)


(defface nano-sans
  '((t (:family "Roboto"
	:height 140
	:weight light)))
  "Default proportional sans font (Roboto Light, 14pt)."
  :group 'nano)

(defface nano-serif
  '((t (:family "Roboto Slab"
	:height 140
	:weight light)))
  "Default proportional serif font (Roboto Slab Light, 14pt)."
  :group 'nano)

(defcustom nano-light-foreground "#37474F" ;; Blue Grey / L800
  "Default foreground color"
  :type 'color :group 'nano-light)

(defcustom nano-light-background "#FFFFFF" ;; White
  "Default background color"
  :type 'color :group 'nano-light)

(defcustom nano-light-highlight "#FAFAFA" ;; Very Light Grey
  "Highlight color is used to highlight part of the screen."
  :type 'color :group 'nano-light)

(defcustom nano-light-subtle "#ECEFF1" ;; Blue Grey / L50
  "Subtle color is used to suggest a physical area on the screen."
  :type 'color :group 'nano-light)

(defcustom nano-light-faded "#B0BEC5" ;; Blue Grey / L200
  "Faded face is for information that are less important."
  :type 'color :group 'nano-light)

(defcustom nano-light-salient "#673AB7" ;; Deep Purple / L500
  "Salient color is used for information that are important."
  :type 'color :group 'nano-light)

(defcustom nano-light-strong "#000000" ;; Black
  "Strong color is used for information of a structural nature."
  :type 'color :group 'nano-light)

(defcustom nano-light-popout "#FFAB91" ;; Deep Orange / L200
  "Popout colour is used for information that needs attention."
  :type 'color :group 'nano-light)

(defcustom nano-light-critical "#FF6F00" ;; Amber / L900
  "Critical face is for information that requires immediate action."
  :type 'color :group 'nano-light)

(defcustom nano-dark-foreground "#ECEFF4" ;; Snow Storm 3  / nord  6
  "Default foreground color"
  :type 'color :group 'nano-dark)

(defcustom nano-dark-background "#2E3440" ;; Polar Night 0 / nord  0
  "Default background color"
  :type 'color :group 'nano-dark)

(defcustom nano-dark-highlight "#3B4252" ;; Polar Night 1 / nord  1
  "Highdark color is used to highdark part of the screen."
  :type 'color :group 'nano-dark)

(defcustom nano-dark-subtle "#434C5E" ;; Polar Night 2 / nord  2
  "Subtle color is used to suggest a physical area on the screen."
  :type 'color :group 'nano-dark)

(defcustom nano-dark-faded "#677691" ;;
  "Faded face is for information that are less important."
  :type 'color :group 'nano-dark)

(defcustom nano-dark-salient "#81A1C1" ;; Frost         / nord  9 
  "Salient color is used for information that are important."
  :type 'color :group 'nano-dark)

(defcustom nano-dark-strong "#ECEFF4" ;; Snow Storm 3  / nord  6         
  "Strong color is used for information of a structural nature."
  :type 'color :group 'nano-dark)

(defcustom nano-dark-popout "#D08770" ;; Aurora        / nord 12
  "Popout colour is used for information that needs attention."
  :type 'color :group 'nano-dark)

(defcustom nano-dark-critical  "#EBCB8B" ;; Aurora        / nord 11
  "Critical face is for information that requires immediate action."
  :type 'color :group 'nano-dark)

(defface nano-critical nil
  "Critical face is for information that requires immediate action.
It should be of high constrast when compared to other faces. This
can be realized (for example) by setting an intense background
color, typically a shade of red. It must be used scarcely."
  :group nil)

(defface nano-critical-i nil
  "Critical face inversed." :group nil)

(defface nano-popout nil
  "Popout face is used for information that needs attention.
To achieve such effect, the hue of the face has to be
sufficiently different from other faces such that it attracts
attention through the popout effect."
  :group nil)

(defface nano-popout-i nil
  "Popout face inversed." :group nil)

(defface nano-strong nil
  "Strong face is used for information of a structural nature.
It has to be the same color as the default color and only the
weight differs by one level (e.g., light/regular or
regular/bold). IT is generally used for titles, keywords,
directory, etc."
  :group nil)

(defface nano-strong-i nil
  "Strong face inversed." :group nil)

(defface nano-salient nil
  "Salient face is used for information that are important.
To suggest the information is of the same nature but important,
the face uses a different hue with approximately the same
intensity as the default face. This is typically used for links."
  :group nil)

(defface nano-salient-i nil
  "Strong face inversed." :group nil)

(defface nano-faded nil
  "Faded face is for information that are less important.
It is made by using the same hue as the default but with a lesser
intensity than the default. It can be used for comments,
secondary information and also replace italic (which is generally
abused anyway)."
  :group nil)

(defface nano-faded-i nil
  "Faded face inversed." :group nil)

(defface nano-subtle nil
  "Subtle face is used to suggest a physical area on the screen.
It is important to not disturb too strongly the reading of
information and this can be made by setting a very light
background color that is barely perceptible."
  :group nil)

(defface nano-subtle-i nil
  "Subtle face inversed." :group nil)

(defface nano-default nil
  "Default face." :group nil)

(defface nano-default-i nil
  "Default face inversed." :group nil)

(defun nano-mode ()
  "Nano theme mode settings (dark & light)"

  (when nano-layout
    (set-frame-parameter nil 'internal-border-width 24)
    (set-frame-parameter nil 'width 81)
    (set-frame-parameter nil 'height 45)
    (set-frame-parameter nil 'left-fringe 0)
    (set-frame-parameter nil 'right-fringe 0)
    (set-display-table-slot standard-display-table 'truncation ?…)
    (set-display-table-slot standard-display-table 'wrap ?—)
    (if (fboundp 'tool-bar-mode) (tool-bar-mode nil))
    (tooltip-mode 0)
    (scroll-bar-mode 0)
    (menu-bar-mode 0)
    (setq window-divider-default-right-width 24)
    (setq window-divider-default-places 'right-only)
    (window-divider-mode 1))
  
  (setq widget-image-enable nil)
  (setq x-underline-at-descent-line t)
  (setq-default indent-tabs-mode nil)
  
  (dolist (buffer (list " *Minibuf-0*" " *Echo Area 0*"
                        " *Minibuf-1*" " *Echo Area 1*"))
    (when (get-buffer buffer)
      (with-current-buffer buffer
        (face-remap-add-relative 'default 'nano-faded)))))

  
(defun nano-light ()
  "Nano theme light"
  
  (interactive)
  (message "Entering nano light mode")
  (set-foreground-color nano-light-foreground)
  (set-face-background 'internal-border nano-light-background (selected-frame))
  (custom-set-variables '(frame-background-mode 'light))
  (load-theme 'nano t)
  (set-frame-parameter nil 'background-mode 'light)
  (nano-mode)
  (frame-set-background-mode (selected-frame) t )
  (set-background-color nano-light-background))


(defun nano-dark ()
  "Nano theme dark"
  
  (interactive)
  (message "Entering nano dark mode")
  (set-foreground-color nano-dark-foreground)
  (set-face-background 'internal-border nano-dark-background (selected-frame))
  (custom-set-variables '(frame-background-mode 'dark))
  (load-theme 'nano t)
  (set-frame-parameter nil 'background-mode 'dark)
  (nano-mode)
  (frame-set-background-mode (selected-frame))
  (set-background-color nano-dark-background))


;; ---  Theme ----------------------------------------------------------
(let ((light     '((background light)))
      (dark      '((background dark)))
      (tty-light '((type tty) (background light)))
      (tty-dark  '((type tty) (background dark))))

  (custom-theme-set-faces
   'nano

   ;; --- Base ---------------------------------------------------------
   `(default ((,light (:foreground ,nano-light-foreground
		       :weight     ,(face-attribute 'nano-mono :weight)
		       :height     ,(face-attribute 'nano-mono :height)
                       :family     ,(face-attribute 'nano-mono :family)))
              (,dark  (:foreground ,nano-dark-foreground
		       :weight     ,(face-attribute 'nano-mono :weight)
		       :height     ,(face-attribute 'nano-mono :height)
                       :family     ,(face-attribute 'nano-mono :family)))))
   
   `(cursor ((,light (:foreground ,nano-light-background
                      :background ,nano-light-foreground))
             (,dark  (:foreground ,nano-dark-background
                      :background ,nano-dark-foreground))))

   `(highlight ((,light (:background ,nano-light-highlight))
                (,dark  (:background ,nano-dark-highlight))))

   `(nano-subtle ((,light (:background ,nano-light-subtle))
		  (,dark  (:background ,nano-dark-subtle))))

   `(nano-subtle-i ((,light (:foreground ,nano-light-subtle))
                    (,dark  (:foreground ,nano-dark-subtle))))
   
   `(nano-faded ((,light  (:foreground ,nano-light-faded))
		 (,dark  (:foreground ,nano-dark-faded))))

   `(nano-faded-i ((,light (:foreground ,nano-light-background
                            :background ,nano-light-faded))
                    (,dark  (:foreground ,nano-dark-background
                             :background ,nano-dark-faded))))
   
   `(nano-default ((,light  (:inherit default))
		   (,dark  (:inherit default))))

   `(nano-default-i ((,light (:foreground ,nano-light-background
                              :background ,nano-light-foreground))
                     (,dark  (:foreground ,nano-dark-background
                              :background ,nano-dark-foreground))))

   
   `(nano-salient ((,light (:foreground ,nano-light-salient))
		   (,dark  (:foreground ,nano-dark-salient))))

   `(nano-salient-i ((,light (:foreground ,nano-light-background
                              :background ,nano-light-salient))
                     (,dark  (:foreground ,nano-dark-background
                              :background ,nano-dark-salient))))

   
   `(nano-strong ((,light (:foreground ,nano-light-strong
                           :weight normal))
		  (,dark  (:foreground ,nano-dark-strong
                           :weight normal))))

   `(nano-strong-i ((,light (:foreground ,nano-light-background
                             :background ,nano-light-strong
                             :weight normal))
                    (,dark  (:foreground ,nano-dark-background
                             :background ,nano-dark-strong
                             :weight normal))))

   `(nano-popout ((,light (:foreground ,nano-light-popout))
                  (,dark  (:foreground ,nano-dark-popout))))

   `(nano-popout-i ((,light (:foreground ,nano-light-background
                             :background ,nano-light-popout))
                    (,dark  (:foreground ,nano-dark-background
                             :background ,nano-dark-popout))))
   
   `(nano-critical ((,light (:foreground ,nano-light-background
                             :background ,nano-light-critical))
                    (,dark  (:foreground ,nano-dark-background
                             :background ,nano-dark-critical))))

   `(nano-critical-i ((,light (:foreground ,nano-light-critical
                               :background ,nano-light-background))
                      (,dark  (:foreground ,nano-dark-critical
                               :background ,nano-dark-background))))
   
   ;; --- Header & mode line -----------------------------------------------
   
   `(mode-line ((,light (:foreground ,nano-light-background
                         :background ,nano-light-foreground
                         :box (:line-width 3
			       :color ,nano-light-foreground
			       :style nil)))
		(,dark  (:foreground ,nano-dark-foreground
			 :background ,nano-dark-faded
                         :box (:line-width 3
			       :color ,nano-dark-faded
			       :style nil)))))
   `(mode-line-highlight ((t (:inherit nano-popout))))
   `(mode-line-buffer-id ((t (:weight regular))))
   `(mode-line-emphasis  ((t (:weight regular))))
			   
   `(mode-line-inactive ((,light (:foreground ,nano-light-background
                                  :background ,nano-light-faded
                                  :box (:line-width 3
					:color ,nano-light-faded
					:style nil)))
			 (,dark  (:foreground ,nano-dark-faded
                                  :background ,nano-dark-subtle
                                  :box (:line-width 3
					:color ,nano-dark-subtle
					:style nil)))))

   ;; --- Structural ---------------------------------------------------------
   '(bold                        ((t (:inherit nano-strong))))
   '(italic                      ((t (:slant italic))))
   '(bold-italic                 ((t (:inherit nano-strong))))
   '(region                      ((t (:inherit nano-subtle))))
   '(fringe                      ((t (:inherit (nano-faded)))))
   '(hl-line                     ((t (:inherit highlight))))
   '(link                        ((t (:inherit nano-salient))))
   '(fixed-pitch                 ((t (:inherit default))))
   '(fixed-pitch-serif           ((t (:inherit default))))
   `(variable-pitch              ((t (:weight ,(face-attribute 'nano-sans :weight)
				      :height ,(face-attribute 'nano-sans :height)
				      :family ,(face-attribute 'nano-sans :family)))))
   
   ;; --- Semantic ---------------------------------------------------------
   '(shadow                        ((t (:inherit nano-faded))))
   '(success                       ((t (:inherit nano-salient))))
   '(warning                       ((t (:inherit nano-popout))))
   '(error                         ((t (:inherit nano-critical))))
   '(match                         ((t (:inherit nano-popout))))

   ;; --- General ---------------------------------------------------------
   '(buffer-menu-buffer            ((t (:inherit nano-strong))))
   '(minibuffer-prompt             ((t (:inherit nano-strong))))
   '(isearch                       ((t (:inherit nano-strong))))
   '(isearch-fail                  ((t (:inherit nano-faded))))
   '(show-paren-match              ((t (:inherit nano-strong))))
   '(show-paren-mismatch           ((t (:inherit nano-critical))))
   '(lazy-highlight                ((t (:inherit nano-subtle))))
   '(trailing-whitespace           ((t (:inherit nano-subtle))))
   '(secondary-selection           ((t (:inherit nano-subtle))))
   '(completions-annotations       ((t (:inherit nano-faded))))
   '(completions-common-part       ((t (:inherit nano-faded))))
   '(completions-first-difference  ((t (:inherit default))))
   '(tooltip                       ((t (:inherit nano-subtle))))
   '(read-multiple-choice-face     ((t (:inherit nano-strong))))
   '(nobreak-hyphen                ((t (:inherit nano-popout))))
   '(nobreak-space                 ((t (:inherit nano-popout))))
   '(help-argument-name            ((t (:inherit nano-faded))))
   '(tabulated-list-fake-header    ((t (:inherit nano-strong))))
   '(tool-bar                      ((t (:inherit nano-faded-i))))

   ;; --- TTY faces ----------------------------------------------------
   '(tty-menu-disabled-face        ((t (:inherit nano-faded-i))))
   '(tty-menu-enabled-face         ((t (:inherit nano-default-i))))
   '(tty-menu-selected-face        ((t (:inherit nano-salient-i))))
   
   ;; --- Windows divider ----------------------------------------------
   `(window-divider                ((,light (:foreground ,nano-light-background))
			            (,dark  (:foreground ,nano-dark-background))))
   '(window-divider-first-pixel    ((t (:inherit window-divider))))
   '(window-divider-last-pixel     ((t (:inherit window-divider))))
   `(vertical-border               ((,light (:foreground ,nano-light-background))
			            (,dark  (:foreground ,nano-dark-background))))

   ;; --- Tab bar ------------------------------------------------------
   '(tab-bar                       ((t (:inherit default))))
   '(tab-bar-tab                   ((t (:inherit default))))
   '(tab-bar-tab-inactive          ((t (:inherit nano-faded))))
   '(tab-line                      ((t (:inherit default))))
   
   ;; --- Line numbers ----------------------------------------------------
   '(line-number                  ((t (:inherit nano-faded))))
   '(line-number-current-line     ((t (:inherit default))))
   `(line-number-major-tick       ((t (:inherit nano-faded))))
   '(line-number-minor-tick       ((t (:inherit nano-faded))))
   
   ;; --- Font lock -------------------------------------------------------
   '(font-lock-comment-face        ((t (:inherit nano-faded))))
   '(font-lock-doc-face            ((t (:inherit nano-faded))))
   '(font-lock-string-face         ((t (:inherit nano-popout))))
   '(font-lock-constant-face       ((t (:inherit nano-salient))))
   '(font-lock-warning-face        ((t (:inherit nano-popout))))
   '(font-lock-function-name-face  ((t (:inherit nano-strong))))
   '(font-lock-variable-name-face  ((t (:inherit nano-strong))))
   '(font-lock-builtin-face        ((t (:inherit nano-salient))))
   '(font-lock-type-face           ((t (:inherit nano-salient))))
   '(font-lock-keyword-face        ((t (:inherit nano-salient))))

   ;; --- Custom edit -----------------------------------------------------
   '(widget-field                  ((t (:inherit nano-subtle))))
   '(widget-button                 ((t (:inherit nano-strong))))
   '(widget-single-line-field      ((t (:inherit nano-subtle))))
   '(custom-group-subtitle         ((t (:inherit nano-strong))))
   '(custom-group-tag              ((t (:inherit nano-strong))))
   '(custom-group-tag-1            ((t (:inherit nano-strong))))
   '(custom-comment                ((t (:inherit nano-faded))))
   '(custom-comment-tag            ((t (:inherit nano-faded))))
   '(custom-changed                ((t (:inherit nano-salient))))
   '(custom-modified               ((t (:inherit nano-salient))))
   '(custom-face-tag               ((t (:inherit nano-strong))))
   '(custom-variable-tag           ((t (:inherit nano-strong))))
   '(custom-invalid                ((t (:inherit nano-popout))))
   '(custom-visibility             ((t (:inherit nano-salient))))
   '(custom-state                  ((t (:inherit nano-salient))))
   '(custom-link                   ((t (:inherit nano-salient))))
   '(custom-variable-obsolete      ((t (:inherit nano-faded))))


   ;; --- Buttons -------------------------------------------------------
   `(custom-button
     ((,light (:foreground ,nano-light-faded
               :background ,nano-light-highlight
               :box nil))
      (,dark (:foreground ,nano-dark-faded
              :background ,nano-dark-highlight
              :box nil))))

   `(custom-button-mouse
     ((,light (:foreground ,nano-light-foreground
	       :background ,nano-light-subtle
               :box nil))
      (,dark (:foreground ,nano-dark-foreground
	      :background ,nano-dark-subtle
              :box nil))))

   `(custom-button-pressed
     ((,light (:foreground ,nano-light-background
	       :background ,nano-light-foreground
               :box nil))
      (,dark (:foreground ,nano-dark-background
	      :background ,nano-dark-foreground
              :box nil))))


   
   ;; --- Packages --------------------------------------------------------
   '(package-description            ((t (:inherit nano-default))))
   '(package-help-section-name      ((t (:inherit nano-default))))
   '(package-name                   ((t (:inherit nano-salient))))
   '(package-status-avail-obso      ((t (:inherit nano-faded))))
   '(package-status-available       ((t (:inherit nano-default))))
   '(package-status-built-in        ((t (:inherit nano-salient))))
   '(package-status-dependency      ((t (:inherit nano-salient))))
   '(package-status-disabled        ((t (:inherit nano-faded))))
   '(package-status-external        ((t (:inherit nano-default))))
   '(package-status-held            ((t (:inherit nano-default))))
   '(package-status-incompat        ((t (:inherit nano-faded))))
   '(package-status-installed       ((t (:inherit nano-salient))))
   '(package-status-new             ((t (:inherit nano-default))))
   '(package-status-unsigned        ((t (:inherit nano-default))))

   ;; --- Info ---------------------------------------------------------
   '(info-node                      ((t (:inherit nano-strong))))
   '(info-menu-header               ((t (:inherit nano-strong))))
   '(info-header-node               ((t (:inherit nano-default))))
   '(info-index-match               ((t (:inherit nano-salient))))
   '(Info-quoted                    ((t (:inherit nano-faded))))
   '(info-title-1                   ((t (:inherit nano-strong))))
   '(info-title-2                   ((t (:inherit nano-strong))))
   '(info-title-3                   ((t (:inherit nano-strong))))
   '(info-title-4                   ((t (:inherit nano-strong))))

   ;; --- Diff ---------------------------------------------------------
   '(diff-header                    ((t (:inherit nano-faded))))
   '(diff-file-header               ((t (:inherit nano-strong))))
   '(diff-context                   ((t (:inherit nano-default))))
   '(diff-removed                   ((t (:inherit nano-faded))))
   '(diff-changed                   ((t (:inherit nano-popout))))
   '(diff-added                     ((t (:inherit nano-salient))))
   '(diff-refine-added              ((t (:inherit (nano-face-salient
                                                   nano-face-strong)))))
   '(diff-refine-changed            ((t (:inherit nano-popout))))
   '(diff-refine-removed            ((t (:inherit nano-faded
						  :strike-through t))))
   
   ;; --- Message ---------------------------------------------------
   '(message-cited-text-1           ((t (:inherit nano-faded))))
   '(message-cited-text-2           ((t (:inherit nano-faded))))
   '(message-cited-text-3           ((t (:inherit nano-faded))))
   '(message-cited-text-4           ((t (:inherit nano-faded))))
   '(message-cited-text             ((t (:inherit nano-faded))))
   '(message-header-cc              ((t (:inherit nano-default))))
   '(message-header-name            ((t (:inherit nano-strong))))
   '(message-header-newsgroups      ((t (:inherit nano-default))))
   '(message-header-other           ((t (:inherit nano-default))))
   '(message-header-subject         ((t (:inherit nano-salient))))
   '(message-header-to              ((t (:inherit nano-salient))))
   '(message-header-xheader         ((t (:inherit nano-default))))
   '(message-mml                    ((t (:inherit nano-popout))))
   '(message-separator              ((t (:inherit nano-faded))))

   
   ;; --- Outline ---------------------------------------------------
   '(outline-1                      ((t (:inherit nano-strong))))
   '(outline-2                      ((t (:inherit nano-strong))))
   '(outline-3                      ((t (:inherit nano-strong))))
   '(outline-4                      ((t (:inherit nano-strong))))
   '(outline-5                      ((t (:inherit nano-strong))))
   '(outline-6                      ((t (:inherit nano-strong))))
   '(outline-7                      ((t (:inherit nano-strong))))
   '(outline-8                      ((t (:inherit nano-strong))))
   
   ;; --- Fly spell -------------------------------------------------
   '(flyspell-duplicate             ((t (:inherit nano-popout))))
   '(flyspell-incorrect             ((t (:inherit nano-popout))))

   ;; --- Org agenda ------------------------------------------------
   '(org-agenda-calendar-event      ((t (:inherit nano-default))))
   '(org-agenda-calendar-sexp       ((t (:inherit nano-salient))))
   '(org-agenda-clocking            ((t (:inherit nano-faded))))
   '(org-agenda-column-dateline     ((t (:inherit nano-faded))))
   '(org-agenda-current-time        ((t (:inherit nano-strong))))
   '(org-agenda-date                ((t (:inherit nano-salient))))
   '(org-agenda-date-today          ((t (:inherit (nano-face-salient
                                                   nano-face-strong)))))
   '(org-agenda-date-weekend        ((t (:inherit nano-faded))))
   '(org-agenda-diary               ((t (:inherit nano-faded))))
   '(org-agenda-dimmed-todo-face    ((t (:inherit nano-faded))))
   '(org-agenda-done                ((t (:inherit nano-faded))))
   '(org-agenda-filter-category     ((t (:inherit nano-faded))))
   '(org-agenda-filter-effort       ((t (:inherit nano-faded))))
   '(org-agenda-filter-regexp       ((t (:inherit nano-faded))))
   '(org-agenda-filter-tags         ((t (:inherit nano-faded))))
   '(org-agenda-property-face       ((t (:inherit nano-faded))))
   '(org-agenda-restriction-lock    ((t (:inherit nano-faded))))
   '(org-agenda-structure           ((t (:inherit nano-strong))))


   ;; --- Org -------------------------------------------------------
   '(org-archived                            ((t (:inherit nano-faded))))
   '(org-block                               ((t (:inherit highlight))))
   '(org-block-begin-line                    ((t (:inherit nano-faded))))
   '(org-block-end-line                      ((t (:inherit nano-faded))))
   '(org-checkbox                            ((t (:inherit nano-faded))))
   '(org-checkbox-statistics-done            ((t (:inherit nano-faded))))
   '(org-checkbox-statistics-todo            ((t (:inherit nano-faded))))
   '(org-clock-overlay                       ((t (:inherit nano-faded))))
   '(org-code                                ((t (:inherit nano-faded))))
   '(org-column                              ((t (:inherit nano-faded))))
   '(org-column-title                        ((t (:inherit nano-faded))))
   '(org-date                                ((t (:inherit nano-faded))))
   '(org-date-selected                       ((t (:inherit nano-faded))))
   '(org-default                             ((t (:inherit nano-faded))))
   '(org-document-info                       ((t (:inherit nano-faded))))
   '(org-document-info-keyword               ((t (:inherit nano-faded))))
   '(org-document-title                      ((t (:inherit nano-faded))))
   '(org-done                                ((t (:inherit nano-default))))
   '(org-drawer                              ((t (:inherit nano-faded))))
   '(org-ellipsis                            ((t (:inherit nano-faded))))
   '(org-footnote                            ((t (:inherit nano-faded))))
   '(org-formula                             ((t (:inherit nano-faded))))
   '(org-headline-done                       ((t (:inherit nano-faded))))
   '(org-hide                                ((t (:inherit nano-subtle-i))))
   '(org-indent                              ((t (:inherit nano-subtle-i))))
   '(org-latex-and-related                   ((t (:inherit nano-faded))))
   '(org-level-1                             ((t (:inherit nano-strong))))
   '(org-level-2                             ((t (:inherit nano-strong))))
   '(org-level-3                             ((t (:inherit nano-strong))))
   '(org-level-4                             ((t (:inherit nano-strong))))
   '(org-level-5                             ((t (:inherit nano-strong))))
   '(org-level-6                             ((t (:inherit nano-strong))))
   '(org-level-7                             ((t (:inherit nano-strong))))
   '(org-level-8                             ((t (:inherit nano-strong))))
   '(org-link                                ((t (:inherit nano-salient))))
   '(org-list-dt                             ((t (:inherit nano-faded))))
   '(org-macro                               ((t (:inherit nano-faded))))
   '(org-meta-line                           ((t (:inherit nano-faded))))
   '(org-mode-line-clock                     ((t (:inherit nano-faded))))
   '(org-mode-line-clock-overrun             ((t (:inherit nano-faded))))
   '(org-priority                            ((t (:inherit nano-faded))))
   '(org-property-value                      ((t (:inherit nano-faded))))
   '(org-quote                               ((t (:inherit nano-faded))))
   '(org-scheduled                           ((t (:inherit nano-faded))))
   '(org-scheduled-previously                ((t (:inherit nano-faded))))
   '(org-scheduled-today                     ((t (:inherit nano-faded))))
   '(org-sexp-date                           ((t (:inherit nano-faded))))
   '(org-special-keyword                     ((t (:inherit nano-faded))))
   '(org-table                               ((t (:inherit nano-faded))))
   '(org-tag                                 ((t (:inherit nano-popout))))
   '(org-tag-group                           ((t (:inherit nano-faded))))
   '(org-target                              ((t (:inherit nano-faded))))
   '(org-time-grid                           ((t (:inherit nano-faded))))
   '(org-todo                                ((t (:inherit nano-salient))))
   '(org-upcoming-deadline                   ((t (:inherit nano-default))))
   '(org-verbatim                            ((t (:inherit nano-popout))))
   '(org-verse                               ((t (:inherit nano-faded))))
   '(org-warning                             ((t (:inherit nano-popout))))


   ;; --- Mu4e -------------------------------------------------------
   '(mu4e-attach-number-face                ((t (:inherit nano-strong))))
   '(mu4e-cited-1-face                       ((t (:inherit nano-faded))))
   '(mu4e-cited-2-face                       ((t (:inherit nano-faded))))
   '(mu4e-cited-3-face                       ((t (:inherit nano-faded))))
   '(mu4e-cited-4-face                       ((t (:inherit nano-faded))))
   '(mu4e-cited-5-face                       ((t (:inherit nano-faded))))
   '(mu4e-cited-6-face                       ((t (:inherit nano-faded))))
   '(mu4e-cited-7-face                       ((t (:inherit nano-faded))))
   '(mu4e-compose-header-face                ((t (:inherit nano-faded))))
   '(mu4e-compose-separator-face             ((t (:inherit nano-faded))))
   '(mu4e-contact-face                     ((t (:inherit nano-salient))))
   '(mu4e-context-face                       ((t (:inherit nano-faded))))
   '(mu4e-draft-face                         ((t (:inherit nano-faded))))
   '(mu4e-flagged-face                      ((t (:inherit nano-popout))))
   '(mu4e-footer-face                        ((t (:inherit nano-faded))))
   '(mu4e-forwarded-face                   ((t (:inherit nano-default))))
   '(mu4e-header-face                      ((t (:inherit nano-default))))
   '(mu4e-header-highlight-face                ((t (:inherit highlight))))
   '(mu4e-header-key-face                   ((t (:inherit nano-strong))))
   '(mu4e-header-marks-face                  ((t (:inherit nano-faded))))
   '(mu4e-header-title-face                 ((t (:inherit nano-strong))))
   '(mu4e-header-value-face                ((t (:inherit nano-default))))
   '(mu4e-highlight-face                    ((t (:inherit nano-popout))))
   '(mu4e-link-face                        ((t (:inherit nano-salient))))
   '(mu4e-modeline-face                      ((t (:inherit nano-faded))))
   '(mu4e-moved-face                         ((t (:inherit nano-faded))))
   '(mu4e-ok-face                            ((t (:inherit nano-faded))))
   '(mu4e-region-code                        ((t (:inherit nano-faded))))
   '(mu4e-replied-face                     ((t (:inherit nano-default))))
   '(mu4e-special-header-value-face        ((t (:inherit nano-default))))
   '(mu4e-system-face                        ((t (:inherit nano-faded))))
   '(mu4e-title-face                        ((t (:inherit nano-strong))))
   '(mu4e-trashed-face                       ((t (:inherit nano-faded))))
   '(mu4e-unread-face                       ((t (:inherit nano-strong))))
   '(mu4e-url-number-face                    ((t (:inherit nano-faded))))
   '(mu4e-view-body-face                   ((t (:inherit nano-default))))
   '(mu4e-warning-face                      ((t (:inherit nano-popout))))

   ;; --- Elfeed ----------------------------------------------------
    '(elfeed-log-date-face                    ((t (:inherit nano-faded))))
    '(elfeed-log-info-level-face            ((t (:inherit nano-default))))
    '(elfeed-log-debug-level-face           ((t (:inherit nano-default))))
    '(elfeed-log-warn-level-face             ((t (:inherit nano-popout))))
    '(elfeed-log-error-level-face            ((t (:inherit nano-popout))))
    '(elfeed-search-tag-face                  ((t (:inherit nano-faded))))
    '(elfeed-search-date-face                 ((t (:inherit nano-faded))))
    '(elfeed-search-feed-face               ((t (:inherit nano-salient))))
    '(elfeed-search-filter-face               ((t (:inherit nano-faded))))
    '(elfeed-search-last-update-face        ((t (:inherit nano-salient))))
    '(elfeed-search-title-face              ((t (:inherit nano-default))))
    '(elfeed-search-tag-face                  ((t (:inherit nano-faded))))
    '(elfeed-search-unread-count-face        ((t (:inherit nano-strong))))
    '(elfeed-search-unread-title-face        ((t (:inherit nano-strong))))

    ;; --- Deft ----------------------------------------------------
    '(deft-filter-string-error-face         ((t (:inherit nano-popout))))
    '(deft-filter-string-face              ((t (:inherit nano-default))))
    '(deft-header-face                     ((t (:inherit nano-salient))))
    '(deft-separator-face                    ((t (:inherit nano-faded))))
    '(deft-summary-face                      ((t (:inherit nano-faded))))
    '(deft-time-face                       ((t (:inherit nano-salient))))
    '(deft-title-face                       ((t (:inherit nano-strong))))

    ;; --- Restructured text ------------------------------------------
    '(rst-adornment                           ((t (:inherit nano-faded))))
    '(rst-block                             ((t (:inherit nano-default))))
    '(rst-comment                             ((t (:inherit nano-faded))))
    '(rst-definition                        ((t (:inherit nano-salient))))
    '(rst-directive                         ((t (:inherit nano-salient))))
    '(rst-emphasis1                           ((t (:inherit nano-faded))))
    '(rst-emphasis2                          ((t (:inherit nano-strong))))
    '(rst-external                          ((t (:inherit nano-salient))))
    '(rst-level-1                            ((t (:inherit nano-strong))))
    '(rst-level-2                            ((t (:inherit nano-strong))))
    '(rst-level-3                            ((t (:inherit nano-strong))))
    '(rst-level-4                            ((t (:inherit nano-strong))))
    '(rst-level-5                            ((t (:inherit nano-strong))))
    '(rst-level-6                            ((t (:inherit nano-strong))))
    '(rst-literal                           ((t (:inherit nano-salient))))
    '(rst-reference                         ((t (:inherit nano-salient))))
    '(rst-transition                        ((t (:inherit nano-default))))


    ;; --- Markdown --------------------------------------------------
    '(markdown-blockquote-face              ((t (:inherit nano-default))))
    '(markdown-bold-face                     ((t (:inherit nano-strong))))
    '(markdown-code-face                    ((t (:inherit nano-default))))
    '(markdown-comment-face                   ((t (:inherit nano-faded))))
    '(markdown-footnote-marker-face         ((t (:inherit nano-default))))
    '(markdown-footnote-text-face           ((t (:inherit nano-default))))
    '(markdown-gfm-checkbox-face            ((t (:inherit nano-default))))
    '(markdown-header-delimiter-face          ((t (:inherit nano-faded))))
    '(markdown-header-face                   ((t (:inherit nano-strong))))
    '(markdown-header-face-1                 ((t (:inherit nano-strong))))
    '(markdown-header-face-2                 ((t (:inherit nano-strong))))
    '(markdown-header-face-3                 ((t (:inherit nano-strong))))
    '(markdown-header-face-4                 ((t (:inherit nano-strong))))
    '(markdown-header-face-5                 ((t (:inherit nano-strong))))
    '(markdown-header-face-6                ((t (:inherit nano-strong))))
    '(markdown-header-rule-face             ((t (:inherit nano-default))))
    '(markdown-highlight-face               ((t (:inherit nano-default))))
    '(markdown-hr-face                      ((t (:inherit nano-default))))
    '(markdown-html-attr-name-face          ((t (:inherit nano-default))))
    '(markdown-html-attr-value-face         ((t (:inherit nano-default))))
    '(markdown-html-entity-face             ((t (:inherit nano-default))))
    '(markdown-html-tag-delimiter-face      ((t (:inherit nano-default))))
    '(markdown-html-tag-name-face           ((t (:inherit nano-default))))
    '(markdown-inline-code-face              ((t (:inherit nano-popout))))
    '(markdown-italic-face                    ((t (:inherit nano-faded))))
    '(markdown-language-info-face           ((t (:inherit nano-default))))
    '(markdown-language-keyword-face        ((t (:inherit nano-default))))
    '(markdown-line-break-face              ((t (:inherit nano-default))))
    '(markdown-link-face                    ((t (:inherit nano-salient))))
    '(markdown-link-title-face              ((t (:inherit nano-default))))
    '(markdown-list-face                      ((t (:inherit nano-faded))))
    '(markdown-markup-face                    ((t (:inherit nano-faded))))
    '(markdown-math-face                    ((t (:inherit nano-default))))
    '(markdown-metadata-key-face              ((t (:inherit nano-faded))))
    '(markdown-metadata-value-face            ((t (:inherit nano-faded))))
    '(markdown-missing-link-face            ((t (:inherit nano-default))))
    '(markdown-plain-url-face               ((t (:inherit nano-default))))
    '(markdown-pre-face                     ((t (:inherit nano-default))))
    '(markdown-reference-face               ((t (:inherit nano-salient))))
    '(markdown-strike-through-face            ((t (:inherit nano-faded))))
    '(markdown-table-face                   ((t (:inherit nano-default))))
    '(markdown-url-face                     ((t (:inherit nano-salient))))


    ;; --- Terminal ------------------------------------------------------
    '(term-bold        ((t (:inherit nano-strong))))
    '(term-color-black ((t (:inherit default))))
    '(term-color-blue ((t (:foreground "#42A5F5"   ;; material color blue L400
                           :background "#BBDEFB"))))  ;; material color blue L100
    '(term-color-cyan ((t (:foreground "#26C6DA"   ;; material color cyan L400
                           :background "#B2EBF2"))))  ;; material color cyan L100
    '(term-color-green ((t (:foreground "#66BB6A"   ;; material color green L400
                            :background "#C8E6C9"))))  ;; material color green L100
    '(term-color-magenta ((t (:foreground "#AB47BC"   ;; material color purple L400
                              :background "#E1BEE7"))))  ;; material color purple L100
    '(term-color-red ((t (:foreground "#EF5350"   ;; material color red L400
                          :background "#FFCDD2"))))  ;; material color red L100
    '(term-color-yellow ((t (:foreground "#FFEE58"    ;; material color yellow L400
                             :background "#FFF9C4")))) ;; material color yellow L100
    ))

(provide-theme 'nano)

 
