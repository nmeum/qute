;; SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
;;
;; SPDX-License-Identifier: GPL-3.0-only

(use-modules (guix gexp)
             (guix packages)
             (guix profiles)
             (gnu packages base)
             (gnu packages c)
             (gnu packages haskell-apps)
             (gnu packages license)
             (gnu packages version-control)
             (qute-packages))

;; This setup here is inspired by the guile-git Guix setup.
;; See https://gitlab.com/guile-git/guile-git/-/tree/v0.10.0/.guix

(concatenate-manifests
  (list (package->development-manifest qute)
        (package->development-manifest qute-syntax)
        (package->development-manifest qute-symex)
        (package->development-manifest qute-cli)

        ;; Extra packages, useful for development purposes.
        (packages->manifest
          (list
            cabal-install
            coreutils
            ktest-tool
            hlint
            apply-refact
            reuse
            cproc
            git
            lhs2tex
            qute-cli))))
