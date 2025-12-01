;;; nano-theme-custom.el --- NANO theme (custom) -*- lexical-binding: t -*-

;; Copyright (C) 2021,2025 Free Software Foundation, Inc.

;; Maintainer: Nicolas P. Rougier <Nicolas.Rougier@inria.fr>
;; URL: https://github.com/rougier/nano-theme
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: theme, gray, light, gray, mono

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
;; Curtom groups for NANO theme

;;; Code:

(defgroup nano-theme nil
  "NANO theme customization.

Known packages are listed below (Subgroups). If your preferred package
is not present, you can make a pull request to add it."
  :group 'faces)

(defgroup nano-theme-emacs nil
  "Emacs faces"
  :tag "Emacs faces"
  :group 'nano-theme)

(defgroup nano-theme-org nil
  "Org faces"
  :tag "Org faces"
  :group 'nano-theme)

(defgroup nano-theme-dired nil
  "Dired faces"
  :tag "Dired faces"
  :group 'nano-theme)

(defgroup nano-theme-dired nil
  "Dired faces"
  :tag "Dired faces"
  :group 'nano-theme)

(defgroup nano-theme-vertico nil
  "Vertico faces"
  :tag "Vertico faces"
  :group 'nano-theme)

(defgroup nano-theme-marginalia nil
  "Marginalia faces"
  :tag "Marginalia faces"
  :group 'nano-theme)

(defgroup nano-theme-magit nil
  "Magit faces"
  :tag "Magit faces"
  :group 'nano-theme)

(defgroup nano-theme-corfu nil
  "Corfu faces"
  :tag "Corfu faces"
  :group 'nano-theme)

(defgroup nano-theme-mu4e nil
  "Mu4e faces"
  :tag "Mu4e faces"
  :group 'nano-theme)

(defgroup nano-theme-gnus nil
  "Gnus faces"
  :tag "Gnus faces"
  :group 'nano-theme)

(defgroup nano-theme-ledger nil
  "Ledger faces"
  :tag "Ledger faces"
  :group 'nano-theme)

(defgroup nano-theme-elfeed nil
  "Elfeed faces"
  :tag "Elfeed faces"
  :group 'nano-theme)

(defgroup nano-theme-deft nil
  "Deft faces"
  :tag "Deft faces"
  :group 'nano-theme)

(defgroup nano-theme-message nil
  "Message faces"
  :tag "Message faces"
  :group 'nano-theme)

(defgroup nano-theme-epa nil
  "EPA (enigmail)"
  :tag "EPA (enigmail)"
  :group 'nano-theme)

(defgroup nano-theme-popup nil
  "Popup menus faces"
  :tag "Popup menus faces"
  :group 'nano-theme)

(defgroup nano-theme-diff nil
  "Diff faces"
  :tag "Diff faces"
  :group 'nano-theme)

(defgroup nano-theme-icomplete nil
  "Icomplete faces"
  :tag "Icomplete faces"
  :group 'nano-theme)

(defgroup nano-theme-markdown nil
  "Markdown faces"
  :tag "Markdown faces"
  :group 'nano-theme)

(defgroup nano-theme-shr nil
  "SHR (Simple HTML Renderer) faces"
  :tag "SHR faces"
  :group 'nano-theme)

(defgroup nano-theme-elpher nil
  "Elpher (Gemini/HTML/Telnet) faces"
  :tag "Elpher faces"
  :group 'nano-theme)

(defgroup nano-theme-buffer-box nil
  "Buffer box  faces"
  :tag "Buffer-box faces"
  :group 'nano-theme)

(provide 'nano-theme-custom)

;;; nano-theme-custom.el ends here
