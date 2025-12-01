;;; nano-light-theme.el --- NANO theme -*- lexical-binding: t -*-

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
;; Light version of NANO theme

;;; Code:
(require 'nano-theme)

;;;###autoload
(deftheme nano-light "Nano light theme")
(nano-theme-build-bases 'nano-light)
(nano-theme-build-faces 'nano-light)
(nano-theme-build-ansi-term 'nano-light)

(provide-theme 'nano-light)

;;; nano-light-theme.el ends here
