;;; kubectl.el --- a plugin for kubectl
;;; Commentary:
;;; code:
(require 'ctable)

(setq k8s--asyn-process-output "")
(make-local-variable 'k8s--asyn-process-output)

(setq k8s--namespaces-buffer-name "*k8s-namepace*")
(make-local-variable 'k8s--namespaces-buffer-name)

(setq k8s--pods-buffer-name "*k8s-pods*")
(make-local-variable 'k8s--pods-buffer-name)

(defvar k8s-namespace-buffer-keymap 
  (define-keymap "g" 
    (define-keymap 
      "s" #'k8s-get-services-current-iterm
      "p" #'k8s-get-pods-current-iterm
      "d" #'k8s-get-deployments-current-iterm
      "c" #'k8s-get-configMaps-current-iterm
      "i" #'k8s-get-ingresses-current-iterm
      )))

(defun k8s--buffer-whole-string (buffer)
  "Get String without properties from other buffer"
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun k8s--output-to-local-buffer-variable (proc string)
  "Get Asyn process's output to k8s--asyn-process-output variable"
  (when (buffer-live-p (process-buffer proc))
    (setq k8s--asyn-process-output (concat k8s--asyn-process-output string))))

(defun k8s--split-string-wtith-blank (str)
  "Split String with space, and remove all empty element"
  (remove "" (split-string str "\s"))
  )

(defun k8s--create-buffer-keymap (buffer)
  (if (string= k8s--namespaces-buffer-name (buffer-name buffer))
      k8s-namespace-buffer-keymap))

(defun k8s--list-all-async (process signal)
  "Run kubectl Async and list resource"
  (when (memq (process-status process) '(exit))
    (let* ((output (remove nil (mapcar 'k8s--split-string-wtith-blank (split-string k8s--asyn-process-output "\n"))))
          (titles (car output))
          (data (cdr output))
          (the-buffer (process-buffer process)))
      (k8s--show-ctable titles data the-buffer (k8s--create-buffer-keymap the-buffer))
      (setq k8s--asyn-process-output nil))))

(defun k8s--list-all (buffer-name shell-command)
  "Run kubectl and list resource"
  (make-process :name buffer-name
                :buffer buffer-name
                :command shell-command
                :filter #'k8s--output-to-local-buffer-variable
                :sentinel 'k8s--list-all-async))

(defun k8s--create-cmodel (title)
  "Create cmodel by title string"
  (make-ctbl:cmodel :title  title
                    :align 'left))

(defun k8s--add-click-hook (componet buffer)
  "Add click-hook for ctable"
  (setq cp component
        table-buffer buffer)
  (if (string= k8s--namespaces-buffer-name (buffer-name buffer))
      (ctbl:cp-add-click-hook cp (lambda () (k8s-get-pods (car (ctbl:cp-get-selected-data-row cp)))))
      (ctbl:cp-add-click-hook cp (lambda () (k8s-get-pod-config 
                                         (car (split-string (buffer-name table-buffer) ":"))
                                         (car (ctbl:cp-get-selected-data-row cp)))))))


(defun k8s--show-ctable (titles data buffer keymap)
  "Create and show data in ctable"
  (let* ((model ; data model
          (make-ctbl:model
           :column-model  (mapcar 'k8s--create-cmodel titles)
           :data data))
        (component ; ctable component
         (ctbl:create-table-component-buffer
          :custom-map keymap
          :buffer buffer
          :model model)))

    (k8s--add-click-hook component buffer)
    (pop-to-buffer (ctbl:cp-get-buffer component))
    (setq k8s--asyn-process-output nil)
    (k8s--add-help-info (k8s--keymaps-to-data (ctbl:table-mode-map k8s-namespace-buffer-keymap)))))

(defun k8s--butify-key (key)
  "Butify key value from byte to string"
  (if (fixnump key)
    (byte-to-string key)
    key))

(defun k8s--combine-key (prefix keymaps)
  (mapcar (lambda (keymap) (list (concat prefix (car keymap)) (cdr keymap))) keymaps))

(defun k8s--keymap-to-item (key-map)
  (if (not (eq key-map 'keymap)) 
      (if (listp (cdr key-map))
          (k8s--combine-key (k8s--butify-key (car key-map)) (remove nil (mapcar #'k8s--keymap-to-item (cdr key-map))))
        (list (k8s--butify-key (car key-map)) (cdr key-map))
        )))

(defun k8s--flat-list (the-list)
  (let ((mutil (car (seq-filter (lambda(item) (listp (car item))) the-list)))
        (single (seq-filter (lambda(item) (not (listp (car item)))) the-list)))
    (append single mutil)))


(defun  k8s--keymaps-to-data (key-maps)
  (k8s--flat-list (remove nil (mapcar #'k8s--keymap-to-item key-maps))))


(defun k8s--insert-list-right (str-list)
  "Insert Help Info on ctable right"
  (save-excursion
    (goto-line 0)
    (end-of-line)
    (let ((max-column (current-column)))
      (while str-list
        (end-of-line)
        (while (< (current-column)  max-column)
          (insert " "))
        (insert "   ")
        (insert (car str-list))
        (setq str-list (cdr str-list))
        (next-line)
        ))))

(defun k8s--add-help-info (data)
  "Insert Help Info"
  (setq buffer-read-only nil)
  (let* ((model ; data model
          (make-ctbl:model
           :column-model  (mapcar 'k8s--create-cmodel (list "key" "function"))
           :data data))
         (component ; ctable component
          (ctbl:get-table-text
           :model model)))
    (k8s--insert-list-right (split-string component "\n")))
  (setq buffer-read-only t))


(defun k8s--switch-to-yaml-buffer(process signal)
  (when (memq (process-status process) '(exit))
    (switch-to-buffer (process-buffer process))
    (insert k8s--asyn-process-output)
    (setq k8s--asyn-process-output nil) (yaml-mode)))

(defun k8s--get-yaml (buffer-name shell-command)
  (make-process :name buffer-name
                :buffer buffer-name
                :command shell-command
                :filter #'k8s--output-to-local-buffer-variable
                :sentinel #'k8s--switch-to-yaml-buffer))

(defun k8s--run-function-in-current-iterm (fun)
  (let ((cp (ctbl:cp-get-component)))
    (funcall fun (car (ctbl:cp-get-selected-data-row cp)))))


(defun k8s-get-ns()
  "kubectl get ns"
  (interactive)
  (k8s--list-all k8s--namespaces-buffer-name '("kubectl" "get" "ns")))

(defun k8s-get-resource(namespace resource &optional name needYaml)
  "kubectl get resource"
  (interactive "sNamespace: \nsResource: \nsName: \nsNeedYaml: ")
  (let ((the-buffer-name (mapconcat #'identity (remove nil (list namespace resource name)) ":"))
        (kubectl-command (remove nil (list "kubectl" "get" resource name "-n" namespace))))
    (if needYaml
      (k8s--get-yaml the-buffer-name (remove nil (list "kubectl" "get" resource name "-n" namespace "-o" "yaml")))
      (k8s--list-all the-buffer-name kubectl-command))))

(defun k8s-get-pods(namespace)
  "kubectl get pods"
  (interactive "sNamespace: ")
  (k8s-get-resource namespace "pods"))

(defun k8s-get-services(namespace)
  "kubectl get service"
  (interactive "sNamespace: ")
  (k8s-get-resource namespace "service"))

(defun k8s-get-deployments(namespace)
  "kubectl get deployment"
  (interactive "sNamespace: ")
  (k8s-get-resource namespace "deployment"))

(defun k8s-get-configMaps(namespace)
  "kubectl get configMaps"
  (interactive "sNamespace: ")
  (k8s-get-resource namespace "configMaps"))

(defun k8s-get-ingresses(namespace)
  "kubectl get ingresses "
  (interactive "sNamespace: ")
  (k8s-get-resource namespace "ingresses"))

(defun k8s-get-pods-current-point-ns()
  "kubectl get pods, namespacs is current point symbol"
  (interactive)
  (k8s-get-pods (thing-at-point 'symbol)))

(defun k8s-get-pod-config (ns pod)
  "Get current pod config using yaml buffer"
  (interactive)
  (k8s-get-resource ns "pods" pod t))

(defun k8s-apply-current-yaml-buffer ()
  "Using kubectl apply current yaml buffer"
  (interactive)
  (shell-command (concat "cat <<EOF | kubectl apply -f - \n" (k8s--buffer-whole-string (current-buffer)) "\nEOF")))


(defun k8s-get-services-current-iterm ()
  (interactive)
  (k8s--run-function-in-current-iterm #'k8s-get-services))

(defun k8s-get-pods-current-iterm ()
  (interactive)
  (k8s--run-function-in-current-iterm #'k8s-get-pods))

(defun k8s-get-deployments-current-iterm ()
  (interactive)
  (k8s--run-function-in-current-iterm #'k8s-get-deployments))

(defun k8s-get-ingresses-current-iterm ()
  (interactive)
  (k8s--run-function-in-current-iterm #'k8s-get-ingresses)
  )

(provide 'kubectl)
;;; kubectl.el ends here
