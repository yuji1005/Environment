;; load-path
(add-to-list 'load-path "~/.emacs.d/elisp/")
(let ((default-directory  "~/.emacs.d/elisp/"))
  (normal-top-level-add-subdirs-to-load-path))
;; ---------------------------------------------------------
;; エディタ設定
;; ---------------------------------------------------------
;; 現在行に色を付ける
(global-hl-line-mode 1)
;; 色
(set-face-background 'hl-line "moccasin")
;; 履歴を次回Emacs起動時にも保存する
(savehist-mode 1)
;; ファイル内のカーソル位置を記憶する
(setq-default save-place t)
(require 'saveplace)
;; 対応する括弧を表示させる
(show-paren-mode 1)
;; シェルに合わせるため，C-hは後退に割り当てる
;; ヘルプは <f1> でも使用可能
;; (global-set-key (kbd "C-h") 'delete-backward-char)
(define-key key-translation-map (kbd "C-h") (kbd "<DEL>"))
;; モードラインに時刻を表示する
(display-time)
;; 行番号，桁番号を表示する
(line-number-mode 1)
(column-number-mode 1)
;; リージョンに色をつける
(transient-mark-mode 1)
;; GCを減らして軽くする(デフォルトの10倍)
;; 現在のマシンパワーではもっと大きくしてもよい
(setq gc-cons-threshold (* 10 gc-cons-threshold))
;; ログの記録行数を増やす
(setq message-log-max 10000)
;; ミニバッファを再帰的に呼び出せるようにする
(setq enable-recursive-minibuffers t)
;; ダイアログボックスを使わないようにする
(setq use-dialog-box nil)
(defalias 'message-box 'message)
;; 履歴をたくさん保存する
(setq history-length 1000)
;; キーストロークをエコーエリアに早く表示する
(setq echo-keystrokes 0.1)
;; 大きいファイルを開こうとしたときに警告を発生させる
;; デフォルトは10MBなので25MBに拡張する
(setq large-file-warning-threshold (* 25 1024 1024))
;; ミニバッファで入力を取り消しても履歴に残す
;; 誤って取り消して入力が失われるのを防ぐため
(defadvice abort-recursive-edit (before minibuffer-save activate)
  (when (eq (selected-window) (active-minibuffer-window))
    (add-to-history minibuffer-history-variable (minibuffer-contents))))
;; yesと入力するのは面倒なのでyで十分
(defalias 'yes-or-no-p 'y-or-n-p)
;; ツールバーとスクロールバーを消す
(tool-bar-mode -1)
(scroll-bar-mode -1)
;; 物理行 ON
(setq line-move-visual nil)
;; 現在の関数名を常に表示する
(which-func-mode 1)
(setq which-func-modes t)
(delete (assoc 'which-func-mode mode-line-format) mode-line-format)
(setq-default header-line-format '(which-func-mode ("" which-func-format)))
;; ffap
(ffap-bindings)
;; uniquify
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-ignore-buffers-re "*[^*]+*")
;; bookmark
(setq bookmark-save-flag 1)
(progn
  (setq bookmark-sort-flag nil)
  (defun bookmark-arrange-latest-top ()
    (let ((latest (bookmark-get-bookmark bookmark)))
      (setq bookmark-alias (cons latest (delq latest bookmark-alist))))
    (bookmark-save))
  (add-hook 'bookmark-after-jump-hook 'bookmark-arrange-latest-top))
;; emacsclient
(server-start)
(defun iconify-emacs-when-server-is-done ()
  (unless server-clients (iconify-frame)))
(add-hook 'server-done-hook 'iconify-emacs-when-server-is-done)
(global-set-key (kbd "C-x C-c") 'server-edit)
(defalias 'exit 'save-buffers-kill-emacs)
;; ---------------------------------------------------------
;; exec-path-from-shell設定
;; ---------------------------------------------------------
(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to match that used by the user's shell.

This is particularly useful under Mac OSX, where GUI apps are not started from a shell."
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))
(set-exec-path-from-shell-PATH)
;; ---------------------------------------------------------
;; el-get設定
;; ---------------------------------------------------------
(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

(add-to-list 'load-path (locate-user-emacs-file "el-get/el-get"))
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))
;; ---------------------------------------------------------
;; インストールパッケージ
;; ---------------------------------------------------------
(el-get-bundle company-mode)
(el-get-bundle flycheck)
(el-get-bundle purcell/exec-path-from-shell)
(el-get-bundle dash)
(el-get-bundle recentf-ext)
(el-get-bundle sr-speedbar)
(el-get-bundle function-args)
(el-get-bundle projectile)
(el-get-bundle deferred)
(el-get-bundle highlight-symbol)
(el-get-bundle auto-highlight-symbol)
(el-get-bundle anzu)
(el-get-bundle smart-compile)
(el-get-bundle yasnippet)
(el-get-bundle point-undo)
(el-get-bundle bm)
(el-get-bundle goto-chg)
(el-get-bundle redo+)
(el-get-bundle color-moccur)
(el-get-bundle moccur-edit)
(el-get-bundle igrep)
(el-get-bundle grep-a-lot)
(el-get-bundle grep-edit)
(el-get-bundle open-junk-file)
(el-get-bundle summarye)
(el-get-bundle ipa)
(el-get-bundle dash-at-point)
;; ido関連
(el-get-bundle nonsequitur/smex)
(el-get-bundle DarwinAwardWinner/ido-ubiquitous)
(el-get-bundle ido-vertical-mode)
(el-get-bundle idomenu)
(el-get-bundle flx-ido :type elpa)
(el-get-bundle flx :type elpa)
;; markdown関連
(el-get-bundle markdown-mode)
;; latex関連
(el-get-bundle yatex
  :type hg
  :pkgname "yatex"
  :url "http://www.yatex.org/hgrepos/yatex")
;; typescript関連
(el-get-bundle typescript-mode)
(el-get-bundle tide)
;; c, c++関連
(el-get-bundle tuhdo/semantic-refactor)
(el-get-bundle randomphrase/company-c-headers)
(el-get-bundle ggtags)
;; go 関連
(el-get-bundle go-mode)
(el-get-bundle company-go
  :type elpa
  ;; :url http://melpa.org/packages/go-mode-20160208.1210.tar
  )
(el-get-bundle go-eldoc)
;; rust 関連
(el-get-bundle company-racer)
(el-get-bundle racer)
(el-get-bundle emacs-racer)
(el-get-bundle flycheck-rust)
;; haskell関連
(el-get-bundle haskell-mode)
(el-get-bundle hindent :type elpa)
(el-get-bundle company-ghc)
(el-get-bundle structured-haskell-mode)
(el-get-bundle flycheck-haskell)
(el-get-bundle rainbow-delimiters)
;; yaml関連
(el-get-bundle yaml-mode)
;; ruby関連
(el-get-bundle enh-ruby-mode)
(el-get-bundle ruby-electric)
(el-get-bundle ruby-block)
(el-get-bundle ruby-refactor)
(el-get-bundle inf-ruby)
(el-get-bundle robe)
(el-get-bundle company-inf-ruby)
(el-get-bundle rubocop)
;; rails関連
(el-get-bundle rinari)
(el-get-bundle rhtml-mode)
(el-get-bundle coffee-mode)
(el-get-bundle sass-mode)
(el-get-bundle haml-mode)
(el-get-bundle projectile-rails)
;; python関連
(el-get-bundle jedi)
(el-get-bundle company-jedi)
;; js関連
(el-get-bundle web-mode)
(el-get-bundle tern)
(el-get-bundle nodejs-repl)
(el-get-bundle js2-mode)
(el-get-bundle js2-refactor)
(el-get-bundle json-mode)
(el-get-bundle swank-js)
(el-get-bundle skewer-mode)
(el-get-bundle company-tern)
(el-get-bundle company-web)
;; scala関連
(el-get-bundle scala-mode2)
(el-get-bundle ensime)

;; ---------------------------------------------------------
;; グローバル設定
;; ---------------------------------------------------------
;; recentf and recentf-ext.el
(require 'recentf)
(setq recentf-save-file "~/.emacs.d/.recentf")
(setq recentf-max-saved-items 1000)            ;; recentf に保存するファイルの数
(setq recentf-exclude '(".recentf"))           ;; .recentf自体は含まない
(setq recentf-auto-cleanup 10)                 ;; 保存する内容を整理
(run-with-idle-timer 30 t 'recentf-save-list)  ;; 30秒ごとに .recentf を保存
(require 'recentf-ext)

;;; 一行あたりの文字数を指定してfill-region
(defun fill-region-with-N (num)
  ""
  (interactive "nfill-column value? ")
  (let ((fill-column num))
    (fill-region (region-beginning) (region-end)))
  )
;; yasnippet
(require 'yasnippet)
(yas/load-directory "~/.emacs.d/snippets")
(yas-global-mode 1)
;; 既存スニペットを挿入する
(define-key yas-minor-mode-map (kbd "C-x i i") 'yas-insert-snippet)
;; 新規スニペットを作成するバッファを用意する
(define-key yas-minor-mode-map (kbd "C-x i n") 'yas-new-snippet)
;; 既存スニペットを閲覧・編集する
(define-key yas-minor-mode-map (kbd "C-x i v") 'yas-visit-snippet-file)
;; ---------------------------------------------------------
;; dash-at-point設定
;; ---------------------------------------------------------
(when 
    (autoload 'dash-at-point "dash-at-point"
      "Search the word at point with Dash." t nil)
  (global-set-key "\C-cd" 'dash-at-point)
  (global-set-key "\C-ce" 'dash-at-point-with-docset))
;; ---------------------------------------------------------
;; ipa設定
;; ---------------------------------------------------------
(require 'ipa)
;; ---------------------------------------------------------
;; summarye設定
;; ---------------------------------------------------------
(require 'summarye)
;; ---------------------------------------------------------
;; open-junk-file設定
;; ---------------------------------------------------------
(require 'open-junk-file)
(setq open-junk-file-format "~/tmp/%Y-%m-%d-%H%M%S.")
;; ---------------------------------------------------------
;; grep, igrep設定
;; ---------------------------------------------------------
(require 'grep-edit)
(require 'igrep)
(igrep-define lgrep (igrep-use-zgrep nil)(igrep-regex-option "-n -0u8"))
(igrep-find-define lgrep (igrep-use-zgrep nil)(igrep-regex-option "-n -0u8"))
(require 'grep-a-lot)
(grep-a-lot-setup-keys)
(grep-a-lot-advise igrep)
;; ---------------------------------------------------------
;; color-moccur, moccur-edit設定
;; ---------------------------------------------------------
(require 'color-moccur)
(require 'moccur-edit)
(setq moccur-split-word t)
;; ---------------------------------------------------------
;; redo+設定
;; ---------------------------------------------------------
(require 'redo+)
(global-set-key (kbd "C-M-/") 'redo)
(setq undo-no-redo t)
(setq undo-limit 600000)
(setq undo-strong-limit 900000)
;; ---------------------------------------------------------
;; goto-chg設定
;; ---------------------------------------------------------
(require 'goto-chg)
(define-key global-map (kbd "<f8>") 'goto-last-change)
(define-key global-map (kbd "S-<f8>") 'goto-last-change-reverse)
  
;; ---------------------------------------------------------
;; bm設定
;; ---------------------------------------------------------
(setq-default bm-buffer-persistence nil)
(setq bm-restore-repository-on-load t)
(require 'bm)
(add-hook 'find-file-hooks 'bm-buffer-restore)
(add-hook 'kill-buffer-hook 'bm-buffer-save)
(add-hook 'after-save-hook 'bm-buffer-restore)
(add-hook 'vc-before-checkin-hook 'bm-buffer-save)
(global-set-key (kbd "M-SPC") 'bm-toggle)
(global-set-key (kbd "M-[") 'bm-previous)
(global-set-key (kbd "M-]") 'bm-next)
;; ---------------------------------------------------------
;; point-undo設定
;; ---------------------------------------------------------
(require 'point-undo)
(define-key global-map (kbd "<f7>") 'point-undo)
(define-key global-map (kbd "S-<f7>") 'point-redo)
;; ---------------------------------------------------------
;; migemo設定
;; ---------------------------------------------------------
(when (and (executable-find "cmigemo")
           (require 'migemo nil t))
  (setq migemo-options '("-q" "--emacs"))

  (setq migemo-user-dictionary nil)
  (setq migemo-regex-dictionary nil)
  (setq migemo-coding-system 'utf-8-unix)
  (setq migemo-command "/usr/local/bin/cmigemo")
  (setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")
  (load-library "migemo")
  (migemo-init)
  )
;; IME, フォント設定
(setq default-input-method "MacOSX")
;; Monaco 12pt をデフォルトにする
(set-face-attribute 'default nil
                    :family "Monaco"
                    :height 130)
;; 日本語をヒラギノ角ゴProNにする
(set-fontset-font "fontset-default"
                  'japanese-jisx0208
                  '("Hiragino Maru Gothic ProN"))
;; 半角カナをヒラギノ角ゴProNにする
(set-fontset-font "fontset-default"
                  'katakana-jisx0201
                  '("Hiragino Maru Gothic ProN"))
;; ---------------------------------------------------------
;; highlight-symbol, auto-highlight-symbol設定
;; ---------------------------------------------------------
(require 'auto-highlight-symbol)
(require 'highlight-symbol)
(setq highlight-symbol-colors '("DarkOrange" "DodgerBlue1" "DeepPink1"))

(global-set-key (kbd "") 'highlight-symbol-at-point)
(global-set-key (kbd "") 'highlight-symbol-remove-all)
;; ---------------------------------------------------------
;; anzu設定
;; ---------------------------------------------------------
(require 'anzu)
(global-anzu-mode +1)

(set-face-attribute 'anzu-mode-line nil
                    :foreground "yellow" :weight 'bold)
(custom-set-variables
 '(anzu-mode-lighter "")
 '(anzu-deactivate-region t)
 '(anzu-search-threshold 1000)
 '(anzu-use-mimego t)
 '(anzu-replace-to-string-separator " => "))
;; ---------------------------------------------------------
;; smart-compile設定
;; ---------------------------------------------------------
(require 'smart-compile)
(setq compilation-window-height 15) ;; default window height is 15

;; ---------------------------------------------------------
;; ido設定
;; ---------------------------------------------------------
(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
(ido-ubiquitous-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)
(setq ido-save-directory-list-file "~/.emacs.d/cache/ido.last")
(ido-vertical-mode 1)
(setq ido-vertical-define-keys 'C-n-C-p-up-and-down)
(setq ido-max-window-height 0.75)
(defun recentf-interactive-complete ()
  "find a file in the recently open file using ido for completion"
  (interactive)
  (let* ((all-files recentf-list)
	 (file-assoc-list (mapcar (lambda (x) (cons (file-name-nondirectory x) x)) all-files))
	 (filename-list (remove-duplicates (mapcar 'car file-assoc-list) :test 'string=))
	 (ido-make-buffer-list-hook
	  (lambda ()
	    (setq ido-temp-list filename-list)))
	 (filename (ido-read-buffer "Find Recent File: "))
	 (result-list (delq nil (mapcar (lambda (x) (if (string= (car x) filename) (cdr x))) file-assoc-list)))
	 (result-length (length result-list)))
         (find-file 
	  (cond 
	   ((= result-length 0) filename)
	   ((= result-length 1) (car result-list))
	   ( t
	     (let ( (ido-make-buffer-list-hook
		     (lambda ()
		       (setq ido-temp-list result-list))))
	       (ido-read-buffer (format "%d matches:" result-length))))
	   ))))
(global-set-key (kbd "C-x C-r") 'recentf-interactive-complete)
(global-set-key (kbd "C-x C-f") 'ido-find-file)
(global-set-key (kbd "C-x C-d") 'ido-dired)
(global-set-key (kbd "C-x b") 'ido-switch-buffer)
(global-set-key (kbd "C-x C-b") 'ido-switch-buffer)
;; smex
(require 'smex)
(smex-initialize)
(setq smex-save-file "~/.emacs.d/cache/.smex-items")
(global-set-key (kbd "M-x") 'smex)
;; idomenu
(autoload 'idomenu "idomenu" nil t)
;; ido-imenu
 (defun ido-goto-symbol (&optional symbol-list)
      "Refresh imenu and jump to a place in the buffer using Ido."
      (interactive)
      (unless (featurep 'imenu)
        (require 'imenu nil t))
      (cond
       ((not symbol-list)
        (let ((ido-mode ido-mode)
              (ido-enable-flex-matching
               (if (boundp 'ido-enable-flex-matching)
                   ido-enable-flex-matching t))
              name-and-pos symbol-names position)
          (unless ido-mode
            (ido-mode 1)
            (setq ido-enable-flex-matching t))
          (while (progn
                   (imenu--cleanup)
                   (setq imenu--index-alist nil)
                   (ido-goto-symbol (imenu--make-index-alist))
                   (setq selected-symbol
                         (ido-completing-read "Symbol? " symbol-names))
                   (string= (car imenu--rescan-item) selected-symbol)))
          (unless (and (boundp 'mark-active) mark-active)
            (push-mark nil t nil))
          (setq position (cdr (assoc selected-symbol name-and-pos)))
          (cond
           ((overlayp position)
            (goto-char (overlay-start position)))
           (t
            (goto-char position)))))
       ((listp symbol-list)
        (dolist (symbol symbol-list)
          (let (name position)
            (cond
             ((and (listp symbol) (imenu--subalist-p symbol))
              (ido-goto-symbol symbol))
             ((listp symbol)
              (setq name (car symbol))
              (setq position (cdr symbol)))
             ((stringp symbol)
              (setq name symbol)
              (setq position
                    (get-text-property 1 'org-imenu-marker symbol))))
            (unless (or (null position) (null name)
                        (string= (car imenu--rescan-item) name))
              (add-to-list 'symbol-names name)
              (add-to-list 'name-and-pos (cons name position))))))))
(global-set-key "\C-ci" 'ido-goto-symbol) ; or any key you see fit
;; ---------------------------------------------------------
;; projectile設定
;; ---------------------------------------------------------
(require  'projectile)
(projectile-global-mode)
;; windows indexing 高速化のおまじない.
(setq projectile-indexing-method 'alien) 
;; 大きいプロジェクトだと劇的に速度が改善するらしい.
(setq projectile-enable-caching t)
(when (executable-find "gtags")
  (setq projectile-tags-file-name "GTAGS")
  (setq projectile-tags-command "gtags"))
(require 'projectile-rails)
(add-hook 'projectile-mode-hook 'projectile-rails-on)
;; ---------------------------------------------------------
;; company設定
;; ---------------------------------------------------------
(require 'company)
;;; C-n, C-pで補完候補を選べるように
(define-key company-active-map (kbd "M-n") nil)
(define-key company-active-map (kbd "M-p") nil)
(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)
;;; C-hがデフォルトでドキュメント表示にmapされているので、文字を消せるようにmapを外す
(define-key company-active-map (kbd "C-h") nil)
;;; 1つしか候補がなかったらtabで補完、複数候補があればtabで次の候補へ行くように
;; (global-set-key (kbd "TAB") #'company-indent-or-complete-common) ;
(setq company-tooltip-align-annotations t)
(define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
;;; ドキュメント表示
(define-key company-active-map (kbd "M-d") 'company-show-doc-buffer)

(setq company-minimum-prefix-length 1) ;; 1文字入力で補完されるように
 ;;; 候補の一番上でselect-previousしたら一番下に、一番下でselect-nextしたら一番上に行くように
(setq company-selection-wrap-around t)

(setq company-tooltip-limit 20)                      ; bigger popup window
(setq company-idle-delay .3)                         ; decrease delay before autocompletion popup shows
(setq company-echo-delay 0)                          ; remove annoying blinking
(setq company-begin-commands '(self-insert-command)) ; start autocompletion only after typing

;;; 色の設定。出来るだけ奇抜にならないように
(set-face-attribute 'company-tooltip nil
                    :foreground "black"
                    :background "lightgray")
(set-face-attribute 'company-preview-common nil
                    :foreground "dark gray"
                    :background "black"
                    :underline t)
(set-face-attribute 'company-tooltip-selection nil
                    :background "steelblue"
                    :foreground "white")
(set-face-attribute 'company-tooltip-common nil
                    :foreground "black"
                    :underline t)
(set-face-attribute 'company-tooltip-common-selection nil
                    :foreground "white"
                    :background "steelblue"
                    :underline t)
(set-face-attribute 'company-tooltip-annotation nil
                    :foreground "red")
;; ---------------------------------------------------------
;;  open-junk-fileの設定
;; ---------------------------------------------------------
(require 'open-junk-file)
(setq open-junk-file-format "~/Documents/memo/junk/%Y-%m%d-%H%M%S.")
(global-set-key "\C-xj" 'open-junk-file)
;; ---------------------------------------------------------
;; org-mode の設定
;; ---------------------------------------------------------
(require 'org)
(require 'org-habit)
(require 'ob-C)
(require 'ob-ruby)
(setq org-src-fontify-natively t)
(defun my-org-confirm-babel-evaluate (lang body)
  (not (or (string= lang "ditaa")
           (string= lang "emacs-lisp")
           (string= lang "ruby")
           (string= lang "C")
           (string= lang "cpp")
           )))
(setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)
(defun org-insert-upheading (arg)
  "1レベル上の見出しを入力する"
  (interactive "P")
  (org-insert-heading arg)
  (cond ((org-on-heading-p) (org-do-promote))
	((org-at-item-p) (org-indent-item -1))))
(defun org-insert-heading-dwim (arg)
  "現在と同じレベルの見出しを入力する．
C-uをつけると1レベル上，C-u C-uをつけると1レベル下の見出しを入力する．"
  (interactive "p")
  (case arg
    (4   (org-insert-subheading nil))
    (16  (org-insert-upheading nil))
    (t   (org-insert-heading nil))))
(define-key org-mode-map (kbd "<C-return>") 'org-insert-heading-dwim)
;; (save-window-excursion (shell-command (format "emacs-test -l test-minimum -l %s %s &" buffer-file-name buffer-file-name)))
 (require 'org-install)
 (setq org-startup-truncated nil)
 (setq org-return-follows-link t)
 (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
 (setq org-directory "~/memo/")
 (setq org-default-notes-file (concat org-directory "agenda.org"))
 (setq org-capture-templates
      '(("t" "Todo" entry
         (file+headline nil "Inbox")
         "** TODO %?\n   %i\n   %a\n   %t")
        ("b" "Bug" entry
         (file+headline nil "Inbox")
         "** TODO %?   :bug:\n   %i\n   %a\n   %t")
        ("i" "Idea" entry
         (file+headline nil "New Ideas")
         "** %?\n   %i\n   %a\n   %t")))

(setq org-use-fast-todo-selection t)
;; DONEの時刻を記録
(setq org-log-done 'time)
(setq org-todo-keywords
      '((sequence "TODO(t)"  "STARTED(s)"  "WAITING(w)" "|" "DONE(x)" "CANCEL(c)" "SOMEDAY(d)")
	(sequence  "APPT(a)" "|" "DONE(x)" "CANCEL(c)")))
(setq org-default-notes-file "~/memo/plan.org")
(setq org-agenda-files (list org-default-notes-file))
(setq org-agenda-custom-commands
      '(("x" "My agenda view"
	 ((agenda)
	  (todo "WAITING")
	  (tags-todo "project")))))
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
;; ---------------------------------------------------------
;; YaTeX の設定
;; ---------------------------------------------------------
;; YaTeX mode
;;
;; PATH
;;
(setenv "PATH"
        (concat (getenv "PATH") ":/Library/TeX/texbin"))
(unless (getenv "RUST_SRC_PATH")
      (setenv "RUST_SRC_PATH" (expand-file-name "~/.rust/src")))
;;
;; YaTeX
;;
(autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)
(setq auto-mode-alist
      (append '(("\\.tex$" . yatex-mode)
                ("\\.ltx$" . yatex-mode)
                ("\\.cls$" . yatex-mode)
                ("\\.sty$" . yatex-mode)
                ("\\.clo$" . yatex-mode)
                ("\\.bbl$" . yatex-mode)) auto-mode-alist))
(setq YaTeX-inhibit-prefix-letter t)
(setq YaTeX-kanji-code nil)
(setq YaTeX-latex-message-code 'utf-8)
(setq YaTeX-use-LaTeX2e t)
(setq YaTeX-use-AMS-LaTeX t)
(setq YaTeX-dvi2-command-ext-alist
      '(("TeXworks\\|texworks\\|texstudio\\|mupdf\\|SumatraPDF\\|Preview\\|Skim\\|TeXShop\\|evince\\|okular\\|zathura\\|qpdfview\\|Firefox\\|firefox\\|chrome\\|chromium\\|Adobe\\|Acrobat\\|AcroRd32\\|acroread\\|pdfopen\\|xdg-open\\|open\\|start" . ".pdf")))
(setq tex-command "/Library/TeX/texbin/ptex2pdf -u -l -ot '-synctex=1'")
;(setq tex-command "/Library/TeX/texbin/platex-ng -synctex=1")
;(setq tex-command "/Library/TeX/texbin/pdflatex -synctex=1")
;(setq tex-command "/Library/TeX/texbin/lualatex -synctex=1")
;(setq tex-command "/Library/TeX/texbin/luajitlatex -synctex=1")
;(setq tex-command "/Library/TeX/texbin/xelatex -synctex=1")
;(setq tex-command "/Library/TeX/texbin/latexmk -e '$latex=q/uplatex %O -synctex=1 %S/' -e '$bibtex=q/upbibtex %O %B/' -e '$biber=q/biber %O --bblencoding=utf8 -u -U --output_safechars %B/' -e '$makeindex=q/upmendex %O -o %D %S/' -e '$dvipdf=q/dvipdfmx %O -o %D %S/' -norc -gg -pdfdvi")
;(setq tex-command "/Library/TeX/texbin/latexmk -e '$latex=q/uplatex %O -synctex=1 %S/' -e '$bibtex=q/upbibtex %O %B/' -e '$biber=q/biber %O --bblencoding=utf8 -u -U --output_safechars %B/' -e '$makeindex=q/upmendex %O -o %D %S/' -e '$dvips=q/dvips %O -z -f %S | convbkmk -u > %D/' -e '$ps2pdf=q/ps2pdf %O %S %D/' -norc -gg -pdfps")
;(setq tex-command "/Library/TeX/texbin/latexmk -e '$pdflatex=q/platex-ng %O -synctex=1 %S/' -e '$bibtex=q/upbibtex %O %B/' -e '$biber=q/biber %O --bblencoding=utf8 -u -U --output_safechars %B/' -e '$makeindex=q/upmendex %O -o %D %S/' -norc -gg -pdf")
;(setq tex-command "/Library/TeX/texbin/latexmk -e '$pdflatex=q/pdflatex %O -synctex=1 %S/' -e '$bibtex=q/bibtex %O %B/' -e '$biber=q/biber %O --bblencoding=utf8 -u -U --output_safechars %B/' -e '$makeindex=q/makeindex %O -o %D %S/' -norc -gg -pdf")
;(setq tex-command "/Library/TeX/texbin/latexmk -e '$pdflatex=q/lualatex %O -synctex=1 %S/' -e '$bibtex=q/upbibtex %O %B/' -e '$biber=q/biber %O --bblencoding=utf8 -u -U --output_safechars %B/' -e '$makeindex=q/upmendex %O -o %D %S/' -norc -gg -pdf")
;(setq tex-command "/Library/TeX/texbin/latexmk -e '$pdflatex=q/luajitlatex %O -synctex=1 %S/' -e '$bibtex=q/upbibtex %O %B/' -e '$biber=q/biber %O --bblencoding=utf8 -u -U --output_safechars %B/' -e '$makeindex=q/upmendex %O -o %D %S/' -norc -gg -pdf")
;(setq tex-command "/Library/TeX/texbin/latexmk -e '$pdflatex=q/xelatex %O -synctex=1 %S/' -e '$bibtex=q/upbibtex %O %B/' -e '$biber=q/biber %O --bblencoding=utf8 -u -U --output_safechars %B/' -e '$makeindex=q/upmendex %O -o %D %S/' -norc -gg -pdf")
(setq bibtex-command "/Library/TeX/texbin/latexmk -e '$latex=q/uplatex %O -synctex=1 %S/' -e '$bibtex=q/upbibtex %O %B/' -e '$biber=q/biber %O --bblencoding=utf8 -u -U --output_safechars %B/' -e '$makeindex=q/upmendex %O -o %D %S/' -e '$dvipdf=q/dvipdfmx %O -o %D %S/' -norc -gg -pdfdvi")
(setq makeindex-command "/Library/TeX/texbin/latexmk -e '$latex=q/uplatex %O -synctex=1 %S/' -e '$bibtex=q/upbibtex %O %B/' -e '$biber=q/biber %O --bblencoding=utf8 -u -U --output_safechars %B/' -e '$makeindex=q/upmendex %O -o %D %S/' -e '$dvipdf=q/dvipdfmx %O -o %D %S/' -norc -gg -pdfdvi")
(setq dvi2-command "/usr/bin/open -a Skim")
;(setq dvi2-command "/usr/bin/open -a Preview")
;(setq dvi2-command "/usr/bin/open -a TeXShop")
;(setq dvi2-command "/Applications/TeXworks.app/Contents/MacOS/TeXworks")
;(setq dvi2-command "/Applications/texstudio.app/Contents/MacOS/texstudio --pdf-viewer-only")
(setq tex-pdfview-command "/usr/bin/open -a Skim")
;(setq tex-pdfview-command "/usr/bin/open -a Preview")
;(setq tex-pdfview-command "/usr/bin/open -a TeXShop")
;(setq tex-pdfview-command "/Applications/TeXworks.app/Contents/MacOS/TeXworks")
;(setq tex-pdfview-command "/Applications/texstudio.app/Contents/MacOS/texstudio --pdf-viewer-only")
(setq dviprint-command-format "/usr/bin/open -a \"Adobe Acrobat Reader DC\" `echo %s | gsed -e \"s/\\.[^.]*$/\\.pdf/\"`")
(add-hook 'yatex-mode-hook
          '(lambda ()
	     (auto-fill-mode -1)
	     (flycheck-mode t)
	     (setq flycheck-check-syntax-automatically '(save mode-enabled))
	     (eldoc-mode t)
	     (company-mode-on)))
;;
;; RefTeX with YaTeX
;;
;(add-hook 'yatex-mode-hook 'turn-on-reftex)
(add-hook 'yatex-mode-hook
          '(lambda ()
             (reftex-mode 1)
             (define-key reftex-mode-map (concat YaTeX-prefix ">") 'YaTeX-comment-region)
             (define-key reftex-mode-map (concat YaTeX-prefix "<") 'YaTeX-uncomment-region)))
;; ---------------------------------------------------------
;; emacsLisp の設定
;; ---------------------------------------------------------
(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (flycheck-mode t)
	    (setq flycheck-check-syntax-automatically '(save mode-enabled))
	    (eldoc-mode t)
	    (company-mode-on)))
;; ---------------------------------------------------------
;; lispの設定
;; ---------------------------------------------------------
;; SBCLをデフォルトのCommon Lisp処理系に設定
(setq inferior-lisp-program "sbcl")
(require 'slime)
(require 'slime-company)
(add-hook 'slime-mode-hook (lambda ()
		       (company-mode)
		       (flycheck-mode t)
		       (setq flycheck-check-syntax-automatically '(save mode-enabled))
		       (slime-setup '(slime-repl slime-fancy slime-banner))
		       ))
;; ---------------------------------------------------------
;; typescript の設定
;; ---------------------------------------------------------
;; typescript-mode
(require 'typescript-mode)
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
;; tide
(require 'tide)
(add-hook 'typescript-mode-hook
          (lambda ()
            (tide-setup)
            (flycheck-mode t)
            (setq flycheck-check-syntax-automatically '(save mode-enabled))
            (eldoc-mode t)
            (company-mode)
	    ))
;; ---------------------------------------------------------
;; c, c++ の設定
;; ---------------------------------------------------------
(require 'cc-mode)
(require 'semantic)
(require 'function-args)
;; c-mode-common-hook は C/C++ の設定
(add-hook 'c-mode-common-hook
          (lambda ()
            (setq c-default-style "k&r") ;; カーニハン・リッチースタイル
            (setq indent-tabs-mode nil)  ;; タブは利用しない
            (setq c-basic-offset 4)      ;; indent は 4 スペース
	    (flycheck-mode t)
	    (setq flycheck-check-syntax-automatically '(save mode-enabled))
	    (eldoc-mode t)
	    (company-mode-on)
	    (global-semanticdb-minor-mode 1)
	    (global-semantic-idle-scheduler-mode 1)
	    (semantic-mode 1)
	    (fa-config-default)
            ))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(defmacro flycheck-define-clike-checker (name command modes)
  `(flycheck-define-checker ,(intern (format "%s" name))
     ,(format "A %s checker using %s" name (car command))
     :command (,@command source-inplace)
     :error-patterns
     ((warning line-start (file-name) ":" line ":" column ": 警告:" (message) line-end)
      (error line-start (file-name) ":" line ":" column ": エラー:" (message) line-end))
     :modes ',modes))
(flycheck-define-clike-checker c-gcc-ja
                   ("gcc" "-fsyntax-only" "-Wall" "-Wextra")
                   c-mode)
(add-to-list 'flycheck-checkers 'c-gcc-ja)
(flycheck-define-clike-checker c++-g++-ja
                   ("g++" "-fsyntax-only" "-Wall" "-Wextra" "-I/usr/include" "-I/usr/local/include" "-I../include" "-I../common" "-I../dao" "-I../model" "-I../view" "-I../cgi" "-I../tools")
                   c++-mode)
(add-to-list 'flycheck-checkers 'c++-g++-ja)
;; semantic-refactoring
(require 'srefactor)
(define-key c-mode-map (kbd "M-RET") 'srefactor-refactor-at-point)
(define-key c++-mode-map (kbd "M-RET") 'srefactor-refactor-at-point)
;; company-c-headers
(require 'company-c-headers)
;; ggtags
(require 'ggtags)
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
              (ggtags-mode 1)
	      (setq-local imenu-create-index-function #'ggtags-build-imenu-index))))

(define-key ggtags-mode-map (kbd "C-c g s") 'ggtags-find-other-symbol)
(define-key ggtags-mode-map (kbd "C-c g h") 'ggtags-view-tag-history)
(define-key ggtags-mode-map (kbd "C-c g r") 'ggtags-find-reference)
(define-key ggtags-mode-map (kbd "C-c g f") 'ggtags-find-file)
(define-key ggtags-mode-map (kbd "C-c g c") 'ggtags-create-tags)
(define-key ggtags-mode-map (kbd "C-c g u") 'ggtags-update-tags)

(define-key ggtags-mode-map (kbd "M-,") 'pop-tag-mark)
;; ---------------------------------------------------------
;; Go の設定
;; ---------------------------------------------------------
(require 'company-go)
(add-hook 'go-mode-hook (lambda ()
			  (setq gofmt-command "goimports")
                          (add-hook 'before-save-hook 'gofmt-before-save)
                          (set (make-local-variable 'compile-command)
                               "go build -v && go test -v && go vet")
                          (local-set-key (kbd "M-.") 'godef-jump)
			  (go-eldoc-setup)
			  ;; gofmtをgoimportsに上書き
			  (setq gofmt-command "goimports")
			  ;; セーブした時にgofmtを実行する
			  (add-hook 'before-save-hook 'gofmt-before-save)
			  (set (make-local-variable 'company-backends) '(company-go))
			  (company-mode)
			  (flycheck-mode t)
			  (setq flycheck-check-syntax-automatically '(save mode-enabled))))
;; ---------------------------------------------------------
;; Rust の設定
;; ---------------------------------------------------------
(require 'company-racer)
;; (with-eval-after-load 'company
;;   (add-to-list 'company-backends 'company-racer))
;; Load rust-mode when you open `.rs` files
;; Set path to racer binary
(setq racer-cmd "~/.cargo/bin/racer")
;; Set path-separator to rust src directory
(setq racer-rust-src-path "~/.rust/src/")
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)
(add-hook 'rust-mode-hook (lambda ()
			    (flycheck-mode t)
			    (setq flycheck-check-syntax-automatically '(save mode-enabled))
			    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
			    ))
;; ---------------------------------------------------------
;; haskell の設定
;; ---------------------------------------------------------
(add-hook 'haskell-mode-hook #'hindent-mode)
(eval-after-load 'haskell-mode
  '(define-key haskell-mode-map [f8] 'haskell-navigate-imports))
(let ((my-cabal-path (expand-file-name "~/.cabal/bin")))
  (setenv "PATH" (concat my-cabal-path path-separator (getenv "PATH")))
  (add-to-list 'exec-path my-cabal-path))
(custom-set-variables '(haskell-tags-on-save t))
(custom-set-variables
  '(haskell-process-suggest-remove-import-lines t)
  '(haskell-process-auto-import-loaded-modules t)
  '(haskell-process-log t))
(eval-after-load 'haskell-mode '(progn
  (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
  (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
  (define-key haskell-mode-map (kbd "C-c C-n C-t") 'haskell-process-do-type)
  (define-key haskell-mode-map (kbd "C-c C-n C-i") 'haskell-process-do-info)
  (define-key haskell-mode-map (kbd "C-c C-n C-c") 'haskell-process-cabal-build)
  (define-key haskell-mode-map (kbd "C-c C-n c") 'haskell-process-cabal)
  (define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)))
(eval-after-load 'haskell-cabal '(progn
  (define-key haskell-cabal-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
  (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
  (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
  (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)))
(custom-set-variables '(haskell-process-type 'cabal-repl))
(custom-set-variables '(haskell-process-type 'stack-ghci))
(eval-after-load 'haskell-mode
  '(define-key haskell-mode-map (kbd "C-c C-o") 'haskell-compile))
(eval-after-load 'haskell-cabal
  '(define-key haskell-cabal-mode-map (kbd "C-c C-o") 'haskell-compile))
(let ((my-cabal-path (expand-file-name "~/.cabal/bin")))
  (setenv "PATH" (concat my-cabal-path ":" (getenv "PATH")))
  (add-to-list 'exec-path my-cabal-path))
(add-to-list 'load-path "~/.cabal/share/x86_64-osx-ghc-7.10.3/ghc-mod-5.5.0.0")
(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)
(add-hook 'haskell-mode-hook (lambda () (ghc-init)))
(require 'company-ghc)
(add-hook 'haskell-mode-hook #'eldoc-mode)
(add-hook 'haskell-mode-hook #'company-mode)
(add-hook 'haskell-mode-hook (lambda () (ghc-init)))
(add-hook 'rust-mode-hook (lambda ()
			    (add-to-list 'company-backends 'company-ghc)
			    (custom-set-variables '(company-ghc-show-info t))
			    (flycheck-mode t)
			    (setq flycheck-check-syntax-automatically '(save mode-enabled))
			    (add-hook 'flycheck-mode-hook #'flycheck-haskell-setup)
			    ))
(add-hook 'haskell-mode-hook 'structured-haskell-mode)
(add-hook 'haskell-mode-hook 'subword-mode)
;; ---------------------------------------------------------
;; yamlの設定
;; ---------------------------------------------------------
(add-hook 'yaml-mode-hook
	  (lambda ()
	    (define-key yaml-mode-map "\C-m" 'newline-and-indent)))
;; ---------------------------------------------------------
;; rubyの設定
;; ---------------------------------------------------------
(autoload 'ruby-mode "ruby-mode"
  "Mode for editing ruby source files" t)
(add-to-list 'auto-mode-alist '("\\.rb$latex " . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . enh-ruby-mode))
(require 'ruby-electric)
(add-hook 'ruby-mode-hook '(lambda () (ruby-electric-mode t)))
(setq ruby-electric-expand-delimiters-list nil)
(require 'rubocop)
(add-hook 'ruby-mode-hook 'rubocop-mode)
;; ruby-block.el --- highlight matching block
(require 'ruby-block)
(ruby-block-mode t)
(setq ruby-block-highlight-toggle t)
;; rcodetools
(require 'rcodetools)
(setq rct-find-tag-if-available nil)
(defun ruby-mode-hook-rcodetools ()
  (define-key ruby-mode-map "\M-\C-i" 'rct-complete-symbol)
  (define-key ruby-mode-map "\C-c\C-t" 'ruby-toggle-buffer)
  (define-key ruby-mode-map "\C-c\C-f" 'rct-ri))
(add-hook 'ruby-mode-hook 'ruby-mode-hook-rcodetools)
;; rdefs
(defun ruby-defs ()
  (interactive)
  (let ((obuf (current-buffer)))
    (setq buf (get-buffer-create "*rdefs*"))
    (set-buffer buf)
    (erase-buffer)
    (set-buffer obuf)
    (call-process-region
     (point-min)
     (point-max) "~/.rbenv/versions/2.3.0/lib/ruby/gems/2.3.0/gems/rdefs-0.0.2/bin/rdefs" nil buf)
    (switch-to-buffer-other-window buf)
    (ruby-mode)))
;; ruby-refactor
(require 'ruby-refactor)
(add-hook 'ruby-mode-hook 'ruby-refactor-mode-launch)
;; inf-ruby
(autoload 'inf-ruby "inf-ruby" "Run an inferior Ruby process" t)
(add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)
(add-hook 'ruby-mode-hook
	  (lambda ()
	    (company-mode)
	    (robe-mode)
	    (eval-after-load 'company
	      '(add-to-list 'company-backends 'company-inf-ruby))
	    (eval-after-load 'company
	      '(add-to-list 'company-backends 'company-yasnippet))
	    (eval-after-load 'company
	      '(push 'company-robe company-backends))
	    (defadvice inf-ruby-console-auto (before activate-rvm-for-robe activate)
	      (rvm-activate-corresponding-ruby))
	    (flycheck-mode t)
	    (setq flycheck-check-syntax-automatically '(save mode-enabled))
	    (flycheck-define-checker ruby-rubocop
	      "A Ruby syntax and style checker using the RuboCop tool."
	      :command ("rubocop" "--format" "emacs" "--silent"
			(config-file "--config" flycheck-rubocoprc)
			source)
	      :error-patterns
	      ((warning line-start
			(file-name) ":" line ":" column ": " (or "C" "W") ": " (message)
			line-end)
	       (error line-start
		      (file-name) ":" line ":" column ": " (or "E" "F") ": " (message)
		      line-end))
	      :modes (enh-ruby-mode motion-mode))	  
	    ;; definition for flycheck
	    (flycheck-define-checker ruby-rubylint
	      "A Ruby syntax and style checker using the rubylint tool."
	      :command ("ruby-lint" source)
	      :error-patterns
	      ((warning line-start
			(file-name) ":" line ":" column ": " (or "C" "W") ": " (message)
			line-end)
	       (error line-start
		      (file-name) ":" line ":" column ": " (or "E" "F") ": " (message)
		      line-end))
	      :modes (enh-ruby-mode ruby-mode))
	    ))
;; ---------------------------------------------------------
;; railsの設定
;; ---------------------------------------------------------
(require 'rinari)
(require 'rhtml-mode)
(require 'coffee-mode)
(require 'sass-mode)
(require 'haml-mode)
(add-hook 'rhtml-mode-hook
(lambda ()
  (rinari-launch)
  (abbrev-mode nil) 
  ))
(add-to-list 'auto-mode-alist
	     '("\\.coffee$" . rinari-minor-mode)
	     '("\\.coffee$" . coffee-mode)
	     )
(autoload 'sass-mode "sass-mode")
(add-to-list 'auto-mode-alist
	     '("\\.sass\\'" . sass-mode)
	     '("\\.sass\\'" . rinari-minor-mode)
	     )
(autoload 'haml-mode "haml-mode")
(add-to-list 'auto-mode-alist
	     '("\\.haml\\'" . haml-mode)
	     '("\\.haml\\'" . rinari-minor-mode)
	     )
;; ---------------------------------------------------------
;; pythonの設定
;; ---------------------------------------------------------
(add-hook 'python-mode-hook
          (lambda ()
            (define-key python-mode-map (kbd "\C-m") 'newline-and-indent)
            (define-key python-mode-map (kbd "RET") 'newline-and-indent)))
(add-hook 'python-mode-hook 'py-autopep8-enable-on-save)
(defun my/python-mode-hook ()
  (add-to-list 'company-backends 'company-jedi))
(add-hook 'python-mode-hook 'my/python-mode-hook)
(defun flycheck-python-setup ()
  (flycheck-mode))
(add-hook 'python-mode-hook #'flycheck-python-setup)
;; ---------------------------------------------------------
;; jsの設定
;; ---------------------------------------------------------
;; web-mode
(require 'web-mode)
;; 拡張子の設定
(add-to-list 'auto-mode-alist '("\\.phtml$"     . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp$"       . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x$"   . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb$"       . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?$"     . web-mode))
;; インデント関係
(defun web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-html-offset   4)
  (setq web-mode-css-offset    4)
  (setq web-mode-script-offset 4)
  (setq web-mode-php-offset    4)
  (setq web-mode-java-offset   4)
  (setq web-mode-asp-offset    4)
  (setq indent-tabs-mode t)
  (setq tab-width 4))
(add-hook 'web-mode-hook 'web-mode-hook)
(require 'nodejs-repl)
(add-to-list
 'auto-mode-alist
 '("\\.js\\'" . js2-mode))
(setq-default
 js-indent-level 2
 js2-basic-offset 2
 ;; Supress js2 mode errors
 js2-mode-show-parse-errors nil
 js2-mode-show-strict-warnings)
(eval-after-load
    'flycheck
  (lambda ()
    (flycheck-add-mode 'javascript-eslint 'js2-mode)
    ;; Disable jshint
    (setq-default
     flycheck-disabled-checkers
     (append flycheck-disabled-checkers
	     '(javascript-jshint)))
    ))
(defun my-javascript-mode-hook ()
  (js2-refactor-mode 1)
  (tern-mode 1)
  (company-mode)
  (add-to-list 'company-backends 'company-tern)
  )
(add-hook
 'js2-mode-hook
 'my-javascript-mode-hook)
;; skewer-mode
(add-hook 'js2-mode-hook 'skewer-mode)
(add-hook 'css-mode-hook 'skewer-css-mode)
(add-hook 'html-mode-hook 'skewer-html-mode)
;; ---------------------------------------------------------
;; html, cssの設定
;; ---------------------------------------------------------
(add-hook 'web-mode-hook (lambda ()
			   (set (make-local-variable 'company-backends) '(company-web-html))
			   (company-mode t)))
;; ---------------------------------------------------------
;; sageの設定
;; ---------------------------------------------------------
(when (and (executable-find "sage"))
  (add-to-list 'load-path "~/.emacs.d/elisp/sage-mode/emacs/")
  (require 'sage "sage")
  (setq sage-command "/usr/local/bin/sage"))
;; ---------------------------------------------------------
;; scalaの設定
;; ---------------------------------------------------------
(require 'scala-mode2)
(require 'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
(defun scala/enable-eldoc ()
  "Show error message at point by Eldoc."
  (setq-local eldoc-documentation-function
              #'(lambda ()
                  (when (ensime-connected-p)
                    (let ((err (ensime-print-errors-at-point)))
                      (and err (not (string= err "")) err)))))
  (eldoc-mode +1)
  (company-mode)
  )
(defun scala/completing-dot-company ()
  (cond (company-backend
         (company-complete-selection)
         (scala/completing-dot))
        (t
         (insert ".")
         (company-complete))))

(defun scala/completing-dot-ac ()
  (insert ".")
  (ac-trigger-key-command t))

;; Interactive commands

(defun scala/completing-dot ()
  "Insert a period and show company completions."
  (interactive "*")
  (eval-and-compile (require 'ensime))
  (eval-and-compile (require 's))
  (when (s-matches? (rx (+ (not space)))
                    (buffer-substring (line-beginning-position) (point)))
    (delete-horizontal-space t))
  (cond ((not (and (ensime-connected-p) ensime-completion-style))
         (insert "."))
        ((eq ensime-completion-style 'company)
         (scala/completing-dot-company))
        ((eq ensime-completion-style 'auto-complete)
         (scala/completing-dot-ac))))

(add-hook 'ensime-mode-hook #'scala/enable-eldoc)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
(add-hook 'scala-mode-hook 'flycheck-mode)
(add-hook 'scala-mode-hook
          '(lambda ()
             (progn
               (local-set-key (kbd "C-x C-j") 'open-by-intellij))))
;; ---------------------------------------------------------
;; markdownの設定
;; ---------------------------------------------------------
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(require 'markdown-mode)
(defun eww-open-file-other-window (file)
  (if (one-window-p) (split-window))
  (other-window 1)
  (eww-open-file file))
(defun markdown-preview-eww ()
  (interactive)
  (message (buffer-file-name))
  (call-process "marked" nil nil nil
        "--gfm" "-o" "/tmp/marked.html" (buffer-file-name))
  (eww-open-file-other-window "/tmp/marked.html"))
(provide 'init)
