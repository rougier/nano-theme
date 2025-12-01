;;; nano-dark-theme.el --- NANO dark theme -*- lexical-binding: t -*-

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
;; Dark version of NANO theme


;;; Code:

;;;###autoload
(deftheme nano-dark "Nano dark theme")
(require 'nano-theme-custom)
(require 'nano-theme-support)
(nano-theme-build-bases 'nano-dark)
(nano-theme-build-faces 'nano-dark)
(nano-theme-build-ansi-term 'nano-dark)
(provide-theme 'nano-dark)

;;; nano-dark-theme.el ends here
