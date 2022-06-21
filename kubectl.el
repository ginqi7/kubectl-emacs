;;; kubectl.el --- a plugin for kubectl
;;; Commentary:
;;; code:
(require 'ctable)

(setq k8s--asyn-process-output "")
(make-local-variable 'k8s--asyn-process-output)

(defvar k8s-namespace-buffer-keymap 
  (define-keymap "g" 
    (define-keymap 
      "s" #'k8s-get-services-current-iterm
      "p" #'k8s-get-pods-current-iterm
      "d" #'k8s-get-deployments-current-iterm
      "c" #'k8s-get-configMaps-current-iterm
      "i" #'k8s-get-ingresses-current-iterm)
    "i" #'k8s-list-pod-files-current-iterm))

(defun k8s--buffer-whole-string (buffer)
  "Get String without properties from other buffer"
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun k8s--output-to-local-buffer-variable (proc string)
  "Get Asyn process's output to k8s--asyn-process-output variable"
  (when (buffer-live-p (process-buffer proc))
    (setq k8s--asyn-process-output (concat k8s--asyn-process-output (replace-regexp-in-string "\015" "" (ansi-color-apply string))))))

(defun k8s--split-string-wtith-blank (str)
  "Split String with space, and remove all empty element"
  (remove "" (split-string str "\s")))

(defun k8s--create-buffer-keymap (buffer)
  k8s-namespace-buffer-keymap)

(defun k8s--list-all-async (process signal)
  "Run kubectl Async and list resource"
  (when (memq (process-status process) '(exit))
    (let* ((output (remove nil (mapcar 'k8s--split-string-wtith-blank (split-string k8s--asyn-process-output "\n"))))
          (titles (car output))
          (data (cdr output))
          (the-buffer (process-buffer process)))
      (k8s--show-ctable titles data the-buffer (process-command process) (k8s--create-buffer-keymap the-buffer)))))

(defun k8s--list-all (buffer-name shell-command)
  "Run kubectl and list resource"
  (setq k8s--asyn-process-output nil)
  (make-process :name buffer-name
                :buffer buffer-name
                :command shell-command
                :filter #'k8s--output-to-local-buffer-variable
                :sentinel 'k8s--list-all-async))

(defun k8s--list-files-async (process signal)
  "Run kubectl Async and list resource"
  (when (memq (process-status process) '(exit))
    (let* ((output (remove nil (mapcar 'k8s--split-string-wtith-blank (split-string k8s--asyn-process-output "\n"))))
          (titles (list "auth" "type" "user" "user" "size" "time" "time" "time" "name"))
          (data (cdr output))
          (the-buffer (process-buffer process)))
      (k8s--show-ctable titles data the-buffer (process-command process) (k8s--create-buffer-keymap the-buffer)))))

(defun k8s--list-files (command)
  (make-process :name (format "%s" command)
                :buffer (format "%s" command)
                :command command
                :filter #'k8s--output-to-local-buffer-variable
                :sentinel #'k8s--list-files-async))

(defun k8s--run-shell-on-pod(ns pod pod-cmd)
  (k8s--list-files (list "kubectl" "-n" ns "exec" "-it" pod "--" "bash" "-i" "-c" pod-cmd)))

(defun k8s--create-cmodel (title)
  "Create cmodel by title string"
  (make-ctbl:cmodel :title  title
                    :align 'left))


(defun k8s--open-pod-inner-file-cmd(file-name cmd)
  (setq k8s--asyn-process-output nil)
  (message (format "%s" cmd))
  (make-process :name file-name
                :buffer file-name
                :command cmd
                :filter #'k8s--output-to-local-buffer-variable
                :sentinel #'k8s--switch-to-pod-inner-file-buffer))

(defun k8s--open-pod-inner-file (ns pod path)
  (let ((cmd (list "kubectl" "-n" ns "exec" "-it" pod "--" "bash" "-i" "-c" (concat "cat " path))))
    (k8s--open-pod-inner-file-cmd path cmd)))


(defun k8s--add-pod-file-click-hook (command data)
  (let* ((ns-idx (+ 1 (cl-position "-n" command :test #'equal)))
        (ns (nth ns-idx command))
        (pod-idx (+ 1 (cl-position "-it" command :test #'equal)))
        (pod (nth pod-idx command))
        (pod-cmd (car (last command)))
        (file-name (car (last data)))
        (file-path (concat (car (last (split-string pod-cmd " +" t))) file-name))
        (type (car data)))
    (if (string-prefix-p "d" type)
        (k8s--list-files (append (butlast command) (list (concat pod-cmd file-name "/"))))
      (k8s--open-pod-inner-file ns pod file-path))))

(defun k8s--add-diffent-click-hook (command data)
  (if (string-match-p "ls " (car (last command)))
      (k8s--add-pod-file-click-hook command data)
    (k8s--get-yaml (car data) (append command (list (car data) "-o" "yaml")))))
  

(defun k8s--add-click-hook (componet command)
  "Add click-hook for ctable"
  (lexical-let* ((cp componet)
                (cmd command))
    (ctbl:cp-add-click-hook 
     componet 
     (lambda () (k8s--add-diffent-click-hook cmd (ctbl:cp-get-selected-data-row cp))))))


(defun k8s--show-ctable (titles data buffer command keymap)
  "Create and show data in ctable"
  (with-current-buffer buffer (erase-buffer))
  (let* ((model ; data model
          (make-ctbl:model
           :column-model  (mapcar 'k8s--create-cmodel titles)
           :data data))
        (component ; ctable component
         (ctbl:create-table-component-buffer
          :custom-map keymap
          :buffer buffer
          :model model)))
    (setq k8s--asyn-process-output nil)
    (pop-to-buffer (ctbl:cp-get-buffer component))
    (k8s--add-click-hook component command)
    (k8s--add-help-info (k8s--keymaps-to-data (ctbl:table-mode-map keymap)))))

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
        (list (k8s--butify-key (car key-map)) (cdr key-map)))))

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
        (if (eq (point) (point-max))
            (insert "\n")
            (next-line))))))

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
    (yaml-mode)))

(defun k8s--switch-to-pod-inner-file-buffer(process signal)
  (when (memq (process-status process) '(exit))
    (switch-to-buffer (process-buffer process))
    (insert k8s--asyn-process-output)))

(defun k8s--get-yaml (buffer-name shell-command)
  (setq k8s--asyn-process-output nil)
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
  (let ((command '("kubectl" "get" "ns")))
    (k8s--list-all (mapconcat 'identity command ":") command)))

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

(defun k8s-list-pod-files (ns pod path)
  (k8s--run-shell-on-pod ns pod (concat "ls -lah " path)))

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
  (k8s--run-function-in-current-iterm #'k8s-get-ingresses))

(defun k8s-get-configMaps-current-iterm ()
  (interactive)
  (k8s--run-function-in-current-iterm #'k8s-get-configMaps))

(defun k8s-list-pod-files-current-iterm ()
  (interactive)
  (let* ((cp (ctbl:cp-get-component))
         (buff-name (buffer-name (ctbl:cp-get-buffer cp)))
         (ns (car (split-string buff-name ":")))
         (pod (car (ctbl:cp-get-selected-data-row cp)))
         )
    (k8s-list-pod-files ns pod "/home/admin/")))

(provide 'kubectl)
;;; kubectl.el ends here

