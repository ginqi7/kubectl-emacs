;;; kubectl.el --- a plugin for kubectl
;;; Commentary:
;;; code:
(require 'ctable)
(require 'names)

(define-namespace kubectl-

(setq asyn-process-output "")
(make-local-variable 'asyn-process-output)

(setq k8s-namespaces-buffer-name "*k8s-namepace*")
(make-local-variable 'k8s-namespaces-buffer-name)

(setq k8s-pods-buffer-name "*k8s-pods*")
(make-local-variable 'k8s-pods-buffer-name)

(defun buffer-whole-string (buffer)
  "Get String without properties from other buffer"
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun output-to-local-buffer-variable (proc string)
  "Get Asyn process's output to asyn-process-output variable"
  (when (buffer-live-p (process-buffer proc))
    (setq asyn-process-output (concat asyn-process-output string))))

(defun split-string-wtith-blank (str)
  "Split String with space, and remove all empty element"
  (remove "" (split-string str "\s"))
  )

(defun list-all-async (process signal)
  "Run kubectl Async and list resource"
  (when (memq (process-status process) '(exit))
    (let* (
          (output (remove nil (mapcar 'kubectl-split-string-wtith-blank (split-string asyn-process-output "\n"))))
          (titles (car output))
          (data (cdr output)))
      (show-ctable titles data (process-buffer process))
      (setq asyn-process-output nil))))

(defun list-all (buffer-name shell-command)
  "Run kubectl and list resource"
  (make-process :name buffer-name
                :buffer buffer-name
                :command shell-command
                :filter #'output-to-local-buffer-variable
                :sentinel 'kubectl-list-all-async))

(defun create-cmodel (title)
  "Create cmodel by title string"
  (make-ctbl:cmodel :title  title
                    :align 'left))

(defun add-click-hook (componet buffer)
  "Add click-hook for ctable"
  (setq cp component
        table-buffer buffer)
  (if (string= k8s-namespaces-buffer-name (buffer-name buffer))
      (ctbl:cp-add-click-hook cp (lambda () (get-pods (car (ctbl:cp-get-selected-data-row cp)))))
      (ctbl:cp-add-click-hook cp (lambda () (get-pod-config 
                                         (car (split-string (buffer-name table-buffer) ":"))
                                         (car (ctbl:cp-get-selected-data-row cp)))))))

(defun show-ctable (titles data buffer)
  "Create and show data in ctable"
  (let* ((model ; data model
          (make-ctbl:model
           :column-model  (mapcar 'kubectl-create-cmodel titles)
           :data data))
        (component ; ctable component
         (ctbl:create-table-component-buffer
          :buffer buffer
          :model model)))
    (add-click-hook component buffer)
    (pop-to-buffer (ctbl:cp-get-buffer component))
    (setq asyn-process-output nil)
    (add-help-info (keymaps-to-data (ctbl:table-mode-map)))))

(defun butify-key (key)
  "Butify key value from byte to string"
  (if (fixnump key)
    (byte-to-string key)
    key))

(defun keymap-to-item (key-map)
  (list (butify-key (car key-map)) (cdr key-map)))

(defun  keymaps-to-data (key-maps)
  (mapcar #'keymap-to-item (cdr key-maps)))


(defun insert-list-right (str-list)
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

(defun add-help-info (data)
  "Insert Help Info"
  (setq buffer-read-only nil)
  (let* (
         (model ; data model
          (make-ctbl:model
           :column-model  (mapcar 'kubectl-create-cmodel (list "key" "function"))
           :data data))
         (component ; ctable component
          (ctbl:get-table-text
           :model model)))
    (message component)
    (insert-list-right (split-string component "\n"))
    )
  (setq buffer-read-only t)
  )

(defun get-ns()
  "kubectl get ns"
  (interactive)
  (list-all k8s-namespaces-buffer-name '("kubectl" "get" "ns")))

(defun get-resource(namespace resource &optional name needYaml)
  "kubectl get resource"
  (interactive "sNamespace: \nsResource: \nsName: \nsNeedYaml: ")
  (let ((the-buffer-name (mapconcat #'identity (remove nil (list namespace resource name)) ":"))
        (kubectl-command (remove nil (list "kubectl" "get" resource name "-n" namespace))))
    (if needYaml
      (get-yaml the-buffer-name (remove nil (list "kubectl" "get" resource name "-n" namespace "-o" "yaml")))
      (list-all the-buffer-name kubectl-command))))

(defun get-pods(namespace)
  "kubectl get pods"
  (interactive "sNamespace: ")
  (get-resource namespace "pods"))

(defun get-service(namespace)
  "kubectl get service"
  (interactive "sNamespace: ")
  (get-resource namespace "service"))

(defun get-deployment(namespace)
  "kubectl get deployment"
  (interactive "sNamespace: ")
  (get-resource namespace "deployment"))

(defun get-configMaps(namespace)
  "kubectl get configMaps"
  (interactive "sNamespace: ")
  (get-resource namespace "configMaps"))


(defun get-pods-current-point-ns()
  "kubectl get pods, namespacs is current point symbol"
  (interactive)
  (get-pods (thing-at-point 'symbol)))


(defun switch-to-yaml-buffer(process signal)
  (when (memq (process-status process) '(exit))
    (switch-to-buffer (process-buffer process))
    (insert asyn-process-output)
    (setq asyn-process-output nil) (yaml-mode)))

(defun get-yaml (buffer-name shell-command)
  (make-process :name buffer-name
                :buffer buffer-name
                :command shell-command
                :filter #'output-to-local-buffer-variable
                :sentinel #'switch-to-yaml-buffer))

(defun get-pod-config (ns pod)
  "Get current pod config using yaml buffer"
  (interactive)
  (get-resource ns "pods" pod t))

(defun apply-current-yaml-buffer ()
  "Using kubectl apply current yaml buffer"
  (interactive)
  (message (concat "cat <<EOF | kubectl apply -f - \n" (buffer-whole-string (current-buffer)) "\nEOF"))
  (shell-command (concat "cat <<EOF | kubectl apply -f - \n" (buffer-whole-string (current-buffer)) "\nEOF")))

)

(provide 'kubectl)
;;; leetcode.el ends here
