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
  (make-ctbl:cmodel :title  title
                    :align 'left)
  )

(defun add-click-hook (componet buffer)
  (setq cp component
        table-buffer buffer
        )
  (if (string= k8s-namespaces-buffer-name (buffer-name buffer))
      (ctbl:cp-add-click-hook cp (lambda () (get-pods (car (ctbl:cp-get-selected-data-row cp)))))
      (ctbl:cp-add-click-hook cp (lambda () (get-pod-config 
                                         (car (split-string (buffer-name table-buffer) ":"))
                                         (car (ctbl:cp-get-selected-data-row cp)))))
    ))

(defun show-ctable (titles data buffer)
  (let* (
        (model ; data model
          (make-ctbl:model
           :column-model  (mapcar 'kubectl-create-cmodel titles)
           :data data))
        (component ; ctable component
         (ctbl:create-table-component-buffer
          :buffer buffer
          :model model)))
    (add-click-hook component buffer)
    (pop-to-buffer (ctbl:cp-get-buffer component))))

(defun get-ns()
  "kubectl get ns"
  (interactive)
  (list-all k8s-namespaces-buffer-name '("kubectl" "get" "ns"))
  )

(defun get-pods(namespace)
  "kubectl get pods"
  (interactive "sNamespace: ")
  (list-all (concat namespace ":" k8s-pods-buffer-name) (list "kubectl" "get" "pods" "-n" namespace))
  )

(defun get-pods-current-point-ns()
  "kubectl get pods, namespacs is current point symbol"
  (interactive)
  (get-pods (thing-at-point 'symbol)))


(defun switch-to-yaml-buffer(process signal)
  (when (memq (process-status process) '(exit))
    (switch-to-buffer (process-buffer process))
    (insert asyn-process-output)
    (setq asyn-process-output nil)
    (yaml-mode)
    )
  )

(defun get-yaml (buffer-name shell-command)
  (make-process :name buffer-name
                :buffer buffer-name
                :command shell-command
                :filter #'output-to-local-buffer-variable
                :sentinel #'switch-to-yaml-buffer
                ))

(defun get-pod-config (ns pod)
  (let ((yaml-buffer-name (concat ns ":" pod)))
    (when (get-buffer yaml-buffer-name)
      (kill-buffer yaml-buffer-name))
    (get-yaml yaml-buffer-name (list "kubectl" "get" "pods" pod "-o" "yaml" "-n" ns))))

)


(provide 'kubectl)
;;; leetcode.el ends here
