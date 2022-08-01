;;; kubectl.el --- Kubectl plugin for Emacs          -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Qiqi Jin

;; Author: Qiqi Jin <ginqi7@gmail.com>
;; Keywords: tools, lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Commands:
;;
;; Below are complete command list:
;;
;;  `k8s-get-ns'
;;    Kubectl get ns.
;;  `k8s-get-resource'
;;    Kubectl get resource.
;;  `k8s-get-pods'
;;    Kubectl get pods by NAMESPACE.
;;  `k8s-get-services'
;;    Kubectl get service by NAMESPACE.
;;  `k8s-get-deployments'
;;    Kubectl get deployment by NAMESPACE.
;;  `k8s-get-configMaps'
;;    Kubectl get configMaps by NAMESPACE.
;;  `k8s-get-ingresses'
;;    Kubectl get ingresses by NAMESPACE.
;;  `k8s-get-pods-current-point-ns'
;;    Kubectl get pods, namespacs is current point symbol.
;;  `k8s-get-pod-config'
;;    Get current pod config using yaml buffer.
;;  `k8s-apply-current-yaml-buffer'
;;    Using kubectl apply current yaml buffer.
;;  `k8s-get-services-current-iterm'
;;    Get current iterm's services.
;;  `k8s-get-pods-current-iterm'
;;    Get current iterm's pods.
;;  `k8s-get-deployments-current-iterm'
;;    Get current iterm's deployments.
;;  `k8s-get-ingresses-current-iterm'
;;    Get current iterm's ingresses.
;;  `k8s-get-configMaps-current-iterm'
;;    Get current iterm's configMaps.
;;  `k8s-list-pod-files-current-iterm'
;;    List current iterm's pod files.
;;  `k8s-get-interactive'
;;    Interactively select k8s resource and show it's config.
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;

;;; Code:

(require 'ctable)

(defvar k8s-namespace-buffer-keymap
  (ctbl:define-keymap
   '(("gs" . k8s-get-services-current-iterm)
     ("gp" . k8s-get-pods-current-iterm)
     ("gd" . k8s-get-deployments-current-iterm)
     ("gc" . k8s-get-configMaps-current-iterm)
     ("gi" . k8s-get-ingresses-current-iterm)
     ("i" . k8s-list-pod-files-current-iterm))))


(defun k8s--buffer-whole-string (buffer)
  "Get String without properties from other buffer.
BUFFER is the buffer name"
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun k8s--output-to-local-buffer-variable (proc string)
  "Get Asyn process's output to .
PROC is current running process.
STRING is current running process' output."
  (when (buffer-live-p (process-buffer proc))
    (process-put proc 'output
                 (concat
                  (process-get proc 'output)
                  (replace-regexp-in-string "\015" ""
                                            (ansi-color-apply string))))))

(defun k8s--split-string-wtith-blank (str)
  "Split STR with space, and remove all empty element."
  (split-string str "\s" t " +"))

(defun k8s--create-buffer-keymap (buffer)
  "Create keymap by BUFFER."
  k8s-namespace-buffer-keymap)

(defun k8s--list-all-async (process signal)
  "Run kubectl Async and list resource.
PROCESS is current running process.
SIGNAL is current running process' signal."
  (when (memq (process-status process) '(exit))
    (let* ((output
            (remove nil
                    (mapcar 'k8s--split-string-wtith-blank
                            (split-string
                             (process-get process 'output)
                             "\n"))))
           (titles (car output))
           (data (cdr output))
           (the-buffer (process-buffer process)))
      (k8s--show-ctable titles data the-buffer
                        (process-command process)
                        (k8s--create-buffer-keymap the-buffer)))))

(defun k8s--list-all-in-minibuffer (process signal)
  "Run kubectl Async and list resource in minibuffer.
PROCESS is current running process.
SIGNAL is current running process' signal."
  (when (memq (process-status process) '(exit))
    (let* ((output
            (remove nil
                    (mapcar 'k8s--split-string-wtith-blank
                            (split-string
                             (process-get process 'output)
                             "\n"))))
           (data (cdr output))
           (candidates (mapcar 'car data))
           (selected-item
            (completing-read "Please Select: " candidates))
           (resource-type (process-get process 'resource-type))
           (ns (process-get process 'namespace)))
      (if ns
          (k8s-get-resource ns resource-type selected-item t)
        (progn
          (let ((new-process
                 (k8s--list-all
                  (format "*k8s-list-%s*" resource-type)
                  (list "kubectl" "get" resource-type "-n" selected-item)
                  #'k8s--list-all-in-minibuffer)))
            (process-put new-process 'resource-type resource-type)
            (process-put new-process 'namespace selected-item)))))))


(defun k8s--list-all (buffer-name shell-command sentinel)
  "Run kubectl and list resource.
BUFFER-NAME is the shell ouput buffer name.
SHELL-COMMAND is running shell cmd.
SENTINEL is sentinel to handle shell output."
  (make-process :name buffer-name
                :buffer buffer-name
                :command shell-command
                :filter #'k8s--output-to-local-buffer-variable
                :sentinel sentinel))

(defun k8s--list-files-async (process signal)
  "Run kubectl Async and list resource.
PROCESS is current running process.
SIGNAL is current running process' signal."

  (when (memq (process-status process) '(exit))
    (let* ((output
            (remove nil
                    (mapcar 'k8s--split-string-wtith-blank
                            (split-string
                             (process-get process 'output)
                             "\n"))))
           (titles
            (list "auth" "type" "user" "user" "size" "time" "time" "time" "name"))
           (data (cdr output))
           (the-buffer (process-buffer process)))
      (k8s--show-ctable titles data the-buffer
                        (process-command process)
                        (k8s--create-buffer-keymap the-buffer)))))

(defun k8s--list-files (command)
  "Run kubectl COMMAND to list files in pod."
  (make-process :name
                (format "%s" command)
                :buffer (format "%s" command)
                :command command
                :filter #'k8s--output-to-local-buffer-variable
                :sentinel #'k8s--list-files-async))

(defun k8s--run-shell-on-pod(ns pod pod-cmd)
  "Run shell on a pod.
NS is k8s namespace.
POD is k8s pod name.
POD-CMD is shell run on pod."
  (k8s--list-files
   (list "kubectl" "-n" ns "exec" "-it" pod "--" "bash" "-i" "-c" pod-cmd)))

(defun k8s--create-cmodel (title)
  "Create cmodel by TITLE string."
  (make-ctbl:cmodel :title  title :align 'left))


(defun k8s--open-pod-inner-file-cmd(file-name cmd)
  "Run open a file on pod.
FILE-NAME is file name on pod.
CMD is a shell cmd."
  (message (format "%s" cmd))
  (make-process :name file-name
                :buffer file-name
                :command cmd
                :filter #'k8s--output-to-local-buffer-variable
                :sentinel #'k8s--switch-to-pod-inner-file-buffer))

(defun k8s--open-pod-inner-file (ns pod path)
  "Run open a file on pod.
NS is k8s namespace.
POD is k8s pod name.
PATH is a file path in pod."
  (let ((cmd
         (list "kubectl" "-n" ns "exec" "-it" pod "--" "bash" "-i" "-c"
               (concat "cat " path))))
    (k8s--open-pod-inner-file-cmd path cmd)))


(defun k8s--add-pod-file-click-hook (command data)
  "Add click function in a file on pod.
COMMAND is a shell command.
DATA is selected ctable data."
  (let* ((ns-idx (+ 1 (cl-position "-n" command :test #'equal)))
         (ns (nth ns-idx command))
         (pod-idx (+ 1 (cl-position "-it" command :test #'equal)))
         (pod (nth pod-idx command))
         (pod-cmd (car (last command)))
         (file-name (car (last data)))
         (file-path
          (concat (car (last (split-string pod-cmd " +" t))) file-name))
         (type (car data)))
    (if (string-prefix-p "d" type)
        (k8s--list-files
         (append
          (butlast command)
          (list (concat pod-cmd file-name "/"))))
      (k8s--open-pod-inner-file ns pod file-path))))

(defun k8s--add-diffent-click-hook (command data)
  "Add different click function on different ctable.
COMMAND is shell commad.
DATA is selected ctable data."
  (if (string-match-p "ls " (car (last command)))
      (k8s--add-pod-file-click-hook command data)
    (k8s--get-yaml
     (car data)
     (append command (list (car data) "-o" "yaml")))))
  

(defun k8s--add-click-hook (component command)
  "Add click-hook for ctable.
COMPONENT is ctable componet.
COMMAND is shell command."
  (lexical-let*
      ((cp component)
       (cmd command))
    (ctbl:cp-add-click-hook
     cp
     (lambda ()
       (k8s--add-diffent-click-hook cmd
                                    (ctbl:cp-get-selected-data-row cp))))))


(defun k8s--show-ctable (titles data buffer command keymap)
  "Create and show data in ctable.
TITLES is ctable titles.
DATA is output data.
BUFFER is output buffer.
COMMAND is running cmd.
KEYMAP is bind keymap."

  (with-current-buffer buffer
    (setq buffer-read-only nil)
    (erase-buffer))
  (let* ((model ; data model
          (make-ctbl:model
           :column-model (mapcar 'k8s--create-cmodel titles)
           :data data))
         (component ; ctable component
          (ctbl:create-table-component-buffer
           :custom-map keymap
           :buffer buffer
           :model model)))
    (pop-to-buffer (ctbl:cp-get-buffer component))
    (k8s--add-click-hook component command)
    (k8s--add-help-info
     (k8s--keymaps-to-data (ctbl:table-mode-map keymap))))
  (setq buffer-read-only t))

(defun k8s--butify-key (key)
  "Butify KEY value from byte to string."
  (if (fixnump key) (byte-to-string key) key))

(defun k8s--combine-key (prefix keymaps)
  "Combine key.
PREFIX is key prefix.
KEYMAPS is keymaps current binded."
  (mapcar
   (lambda (keymap)
     (list (concat prefix (car keymap)) (cdr keymap)))
   keymaps))

(defun k8s--keymap-to-item (key-map)
  "Convert keymap to item list.
KEY-MAP is key map current binded."
  (if (not (eq key-map 'keymap))
      (if (listp (cdr key-map))
          (k8s--combine-key
           (k8s--butify-key (car key-map))
           (remove nil (mapcar #'k8s--keymap-to-item (cdr key-map))))
        (list (k8s--butify-key (car key-map)) (cdr key-map)))))

(defun k8s--flat-list (the-list)
  "Make nest THE-LIST to flat list."
  (let ((mutil
         (car
          (seq-filter
           (lambda(item) (listp (car item)))
           the-list)))
        (single
         (seq-filter
          (lambda(item)
            (not (listp (car item))))
          the-list)))
    (append single mutil)))


(defun  k8s--keymaps-to-data
    (key-maps)
  "Make KEY-MAPS to ctable data."
  (k8s--flat-list
   (remove nil (mapcar #'k8s--keymap-to-item key-maps))))


(defun k8s--insert-list-right (str-list)
  "Insert Help Info STR-LIST on ctable right."
  (save-excursion
    (goto-line 0)
    (end-of-line)
    (let ((max-column (current-column)))
      (while str-list
        (end-of-line)
        (while (< (current-column) max-column) (insert " "))
        (insert "   ")
        (insert (car str-list))
        (setq str-list (cdr str-list))
        (if (eq (point) (point-max))
            (insert "\n")
          (next-line))))))

(defun k8s--add-help-info (data)
  "Insert Help Info DATA."
  (setq buffer-read-only nil)
  (let* ((model ; data model
          (make-ctbl:model
           :column-model (mapcar 'k8s--create-cmodel (list "key" "function"))
           :data data))
         (component ; ctable component
          (ctbl:get-table-text :model model)))
    (k8s--insert-list-right (split-string component "\n")))
  (setq buffer-read-only t))


(defun k8s--switch-to-yaml-buffer(process signal)
  "Convert shell output to yaml buffer.
PROCESS is current running process.
SIGNAL is current running process' signal."
  (when (memq (process-status process) '(exit))
    (switch-to-buffer (process-buffer process))
    (erase-buffer)
    (insert (process-get process 'output))
    (yaml-mode)))

(defun k8s--switch-to-pod-inner-file-buffer(process signal)
  "Open file on pod in a buffer.
PROCESS is current running process.
SIGNAL is current running process' signal."
  (when (memq (process-status process) '(exit))
    (switch-to-buffer (process-buffer process))
    (insert (process-get process 'output))))

(defun k8s--get-yaml (buffer-name shell-command)
  "Kubectl get resource in yaml buffer.
BUFFER-NAME is output buffer name.
SHELL-COMMAND is kubectl shell."
  (make-process :name buffer-name
                :buffer buffer-name
                :command shell-command
                :filter #'k8s--output-to-local-buffer-variable
                :sentinel #'k8s--switch-to-yaml-buffer))

(defun k8s--run-function-in-current-iterm (fun)
  "Call FUN in current selected item."
  (let ((cp (ctbl:cp-get-component)))
    (funcall fun (car (ctbl:cp-get-selected-data-row cp)))))


(defun k8s-get-ns()
  "Kubectl get ns."
  (interactive)
  (let ((command '("kubectl" "get" "ns")))
    (k8s--list-all
     (mapconcat 'identity command ":")
     command #'k8s--list-all-async)))

(defun k8s-get-resource(namespace resource &optional name needYaml)
  "Kubectl get resource.
NAMESPACE is k8s namespace.
RESOURCE is k8s resource like pod, service, etc.
NAME resource name.
NEEDYAML if need to generate yaml buffer."
  (interactive "sNamespace: \nsResource: \nsName: \nsNeedYaml: ")
  (let ((the-buffer-name
         (mapconcat #'identity
                    (remove nil (list namespace resource name))
                    ":"))
        (kubectl-command
         (remove nil
                 (list "kubectl" "get" resource name "-n" namespace))))
    (if needYaml
        (k8s--get-yaml the-buffer-name
                       (remove nil
                               (list "kubectl" "get" resource name "-n" namespace "-o" "yaml")))
      (k8s--list-all the-buffer-name kubectl-command #'k8s--list-all-async))))

(defun k8s-get-pods(namespace)
  "Kubectl get pods by NAMESPACE."
  (interactive "sNamespace: ")
  (k8s-get-resource namespace "pods"))

(defun k8s-get-services(namespace)
  "Kubectl get service by NAMESPACE."
  (interactive "sNamespace: ")
  (k8s-get-resource namespace "service"))

(defun k8s-get-deployments(namespace)
  "Kubectl get deployment by NAMESPACE."
  (interactive "sNamespace: ")
  (k8s-get-resource namespace "deployment"))

(defun k8s-get-configMaps(namespace)
  "Kubectl get configMaps by NAMESPACE."
  (interactive "sNamespace: ")
  (k8s-get-resource namespace "configMaps"))

(defun k8s-get-ingresses(namespace)
  "Kubectl get ingresses by NAMESPACE."
  (interactive "sNamespace: ")
  (k8s-get-resource namespace "ingresses"))

(defun k8s-get-pods-current-point-ns()
  "Kubectl get pods, namespacs is current point symbol."
  (interactive)
  (k8s-get-pods (thing-at-point 'symbol)))

(defun k8s-get-pod-config (ns pod)
  "Get current pod config using yaml buffer.
NS is k8s namespace.
POD is k8s pod name."
  (interactive)
  (k8s-get-resource ns "pods" pod t))

(defun k8s-list-pod-files (ns pod path)
  "List all files in POD on the PATH.
NS is k8s namespace."
  (k8s--run-shell-on-pod ns pod (concat "ls -lah " path)))

(defun k8s-apply-current-yaml-buffer ()
  "Using kubectl apply current yaml buffer."
  (interactive)
  (shell-command
   (concat "cat <<EOF | kubectl apply -f - \n"
           (k8s--buffer-whole-string (current-buffer))
           "\nEOF")))


(defun k8s-get-services-current-iterm ()
  "Get current iterm's services."
  (interactive)
  (k8s--run-function-in-current-iterm #'k8s-get-services))

(defun k8s-get-pods-current-iterm ()
  "Get current iterm's pods."
  (interactive)
  (k8s--run-function-in-current-iterm #'k8s-get-pods))

(defun k8s-get-deployments-current-iterm ()
  "Get current iterm's deployments."
  (interactive)
  (k8s--run-function-in-current-iterm #'k8s-get-deployments))

(defun k8s-get-ingresses-current-iterm ()
  "Get current iterm's ingresses."
  (interactive)
  (k8s--run-function-in-current-iterm #'k8s-get-ingresses))

(defun k8s-get-configMaps-current-iterm ()
  "Get current iterm's configMaps."
  (interactive)
  (k8s--run-function-in-current-iterm #'k8s-get-configMaps))

(defun k8s-list-pod-files-current-iterm ()
  "List current iterm's pod files."
  (interactive)
  (let* ((cp (ctbl:cp-get-component))
         (buff-name (buffer-name (ctbl:cp-get-buffer cp)))
         (ns (car (split-string buff-name ":")))
         (pod (car (ctbl:cp-get-selected-data-row cp))))
    (k8s-list-pod-files ns pod "/home/admin/")))

(defun k8s-get-interactive ()
  "Interactively select k8s resource and show it's config."
  (interactive)
  (let* ((resource-type
          (completing-read "Select Resource: "
                           '("pods" "deployments" "configmaps" "service" "ingress")))
         (ns-process
          (k8s--list-all "*k8s-list-ns*"
                         '("kubectl" "get" "ns")
                         #'k8s--list-all-in-minibuffer)))
    (process-put ns-process 'resource-type resource-type)))

(provide 'kubectl)
;;; kubectl.el ends here




