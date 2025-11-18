;;; wasabi.el --- A WhatsApp Emacs client  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Alvaro Ramirez

;; Author: Alvaro Ramirez https://xenodium.com
;; URL: https://github.com/xenodium/wasabi
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (acp "0.7.1"))

;; This file is not part of GNU Emacs.

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; wasabi provides a native Emacs interface for WhatsApp messaging.
;; It communicates with a wuzapi process over standard I/O using json-rpc.

;;; Code:

(require 'acp) ;; for json-rpc.
(eval-when-compile
  (require 'cl-lib))
(require 'map)
(require 'seq)
(require 'wasabi-chat)

(defcustom wasabi-user-token (concat
                              (or (user-login-name)
                                  (error "wasabi: No login name available"))
                              "-token")
  "User token for identifying wuzapi user.

Defaults to (user-login-name) + \"-token\".

This token identifies the single user added to the wuzapi database and
must remain constant across Emacs sessions. Changing this will cause
you to lose your WhatsApp session and require re-scanning the QR code.

`wasabi' communicates with a wuzapi process over standard I/O, thus
this token is not used for network security, but for rather user
identification in the wuzapi database.

Each wuzapi user has their own WhatsApp session associated with their
token.

If you need to start fresh delete the wuzapi database files."
  :type 'string
  :group 'wasabi)

(defcustom wasabi-data-dir (expand-file-name "wasabi" user-emacs-directory)
  "Directory for wasabi data storage.

This directory will be used to store the wuzapi database and session files.
If the directory does not exist, it will be created automatically."
  :type 'directory
  :group 'wasabi)

(defun wasabi-data-dir ()
  "Return the data directory, ensuring it exists.
Creates the directory if it doesn't exist.
Signals an error if the directory cannot be created."
  (let ((dir (expand-file-name wasabi-data-dir)))
    (unless (file-directory-p dir)
      (condition-case err
          (make-directory dir t)
        (error
         (error "Cannot create wasabi data directory %s: %s" dir (error-message-string err)))))
    dir))

(defcustom wasabi-wuzapi-command
  `("wuzapi" "-mode=stdio" ,(format "-datadir=%s" (wasabi-data-dir)))
  "Command and parameters for the wuzapi binary.

The first element is the command name, and the rest are command parameters."
  :type '(repeat string)
  :group 'wasabi)

(defvar wasabi--admin-token "emacs-admin-token"
  "Wuzapi admin token for administrative operations.

`wasabi' communicates with a wuzapi process over standard I/O.
While this token is not technically required for network authentication,
it is required internally by the process.")

(defconst wasabi--event-subscriptions
  '("Message"
    "Receipt"
    "Connected"
    "Disconnected"
    "ConnectFailure"
    "LoggedOut"
    "QR"
    "PairSuccess"
    "PairError"
    "StreamError"
    "StreamReplaced"
    "ClientOutdated"
    "TemporaryBan"
    "KeepAliveRestored"
    "KeepAliveTimeout"
    "UndecryptableMessage"
    "GroupInfo"
    "JoinedGroup"
    "IdentityChange"
    "HistorySync"
    "OfflineSyncCompleted"
    "AppStateSyncComplete")
  "WhatsApp event types to subscribe to during connection.")

(defvar-local wasabi--state nil)

(cl-defun wasabi--initialize (&key home-buffer)
  (unless wasabi--state
    (setq wasabi--state (wasabi--make-state :home-buffer home-buffer)))
  (cond ((not (map-elt (wasabi--state) :client))
         ;; Step 1: Create client
         (wasabi--set-status :type 'loading :message "Loading...")
         ;; wasabi-wuzapi-command: '("/path/to/wuzapi" "-mode=stdio" ...)
         (map-put! (wasabi--state)
                   :client (acp-make-client :command (car wasabi-wuzapi-command)
                                            :command-params (cdr wasabi-wuzapi-command)
                                            :environment-variables (list (concat "WUZAPI_ADMIN_TOKEN=" wasabi--admin-token)
                                                                         (concat "TZ=" (wasabi--timezone)))
                                            :context-buffer home-buffer))
         (wasabi--initialize :home-buffer home-buffer))
        ((not (map-nested-elt (wasabi--state) '(:client :notification-handlers)))
         ;; Step 2: Initialize subscriptions
         (wasabi--log "Initializing subscriptions")
         (wasabi--initialize-subscriptions)
         (wasabi--log "Subscriptions initialized, continuing")
         (wasabi--initialize :home-buffer home-buffer))
        ((not (map-elt (wasabi--state) :user-exists-checked))
         ;; Step 3: Check if we need to create a user
         (wasabi--log "Checking if user exists")
         (wasabi--send-admin-users-list-request
          :on-users-fetched (lambda (users)
                              (wasabi--log "Found %d users" (length users))
                              (if (seq-empty-p users)
                                  (wasabi--send-admin-add-user-request
                                   :on-user-added (lambda ()
                                                    (wasabi--log "User added successfully")
                                                    (map-put! (wasabi--state) :user-exists-checked t)
                                                    (wasabi--initialize :home-buffer home-buffer)))
                                (wasabi--log "User already exists")
                                (map-put! (wasabi--state) :user-exists-checked t)
                                (wasabi--initialize :home-buffer home-buffer)))))
        ((not (map-elt (wasabi--state) :status-checked))
         ;; Step 4: Check session status
         (wasabi--log "Sending status check request")
         (wasabi--set-status :type 'checking-session-status :message "Loading...")
         (acp-send-request :client (map-elt (wasabi--state) :client)
                           :request (wasabi--make-session-status-request
                                     :token wasabi-user-token)
                           :on-success (lambda (response)
                                         (wasabi--log "Status check response received: connected=%s logged-in=%s"
                                                      (map-elt response 'connected)
                                                      (map-elt response 'loggedIn))
                                         (map-put! (wasabi--state) :status-checked t)
                                         (cond
                                          ;; Fully connected and logged in - fetch chat index
                                          ((and (map-elt response 'connected)
                                                (map-elt response 'loggedIn))
                                           (map-put! (wasabi--state) :connected t)
                                           (wasabi--log "Already connected and logged in")
                                           (wasabi--fetch-chat-index))
                                          ;; Connected but not logged in - wait for Connected notification
                                          ((map-elt response 'connected)
                                           (map-put! (wasabi--state) :connected t)
                                           (wasabi--log "Connected but not logged in yet, waiting for Connected notification")
                                           (wasabi--set-status
                                            :type 'already-connected
                                            :message "Loading..."))
                                          ;; Not connected at all - proceed to connect
                                          (t
                                           (wasabi--log "Not connected, continuing to connect")
                                           (wasabi--initialize :home-buffer home-buffer))))
                           :on-failure (lambda (error)
                                         (map-put! (wasabi--state) :status-checked t)
                                         (wasabi--log "Status check failed: %s" (or (map-elt error 'message) "unknown"))
                                         (wasabi--initialize :home-buffer home-buffer))))
        ((not (map-elt (wasabi--state) :connected))
         ;; Step 5: Connect to WhatsApp (only if not already connected)
         (wasabi--log "Not connected, initiating connection")
         (wasabi--send-connect-request))))

(defun wasabi--send-connect-request ()
  (unless (derived-mode-p 'wasabi-mode)
    (error "Not in a chats buffer"))
  (wasabi--set-status :type 'connecting :message "Loading...")
  (acp-send-request :client (map-elt (wasabi--state) :client)
                    :request (wasabi--make-session-connect-request
                              :token wasabi-user-token
                              :immediate t
                              :subscribe wasabi--event-subscriptions)
                    :on-success (lambda (_response)
                                  (wasabi--log "Initial connection successful (waiting for notification)")
                                  (wasabi--set-status :type 'awaiting-connection :message "Loading..."))
                    :on-failure (lambda (error)
                                  ;; Already connected, don't display as
                                  ;; error since it can be ignored.
                                  (if (and (map-elt error 'message)
                                           (string-match-p "already connected" (map-elt error 'message)))
                                      (wasabi--log "Connect request failed: already connected (ignored)")
                                    (wasabi--log "Connect request failed: %s"
                                                 (map-elt error 'message))
                                    ;; Display real error
                                    (wasabi--set-status
                                     :type 'error
                                     :message (wasabi--refresh-error :message "Failed to connect"))))))

(cl-defun wasabi--send-disconnect-request (&key on-disconnected)
  (unless (derived-mode-p 'wasabi-mode)
    (error "Not in a chats buffer"))
  (wasabi--log "Requesting to disconnected")
  (acp-send-request :client (map-elt (wasabi--state) :client)
                    :request (wasabi--make-session-disconnect-request
                              :token wasabi-user-token)
                    :on-success (lambda (_response)
                                  (wasabi--log "Disconnect: success")
                                  (when on-disconnected
                                    (funcall on-disconnected))
                                  (wasabi--set-status :type nil :message nil))
                    :on-failure (lambda (error)
                                  (wasabi--log "Disconnect: failure %s" (map-elt error 'message))
                                  (wasabi--set-status
                                   :type 'error
                                   :message (wasabi--refresh-error :message "Something is not right")))))

(cl-defun wasabi--send-admin-users-list-request (&key on-users-fetched)
  (unless (derived-mode-p 'wasabi-mode)
    (error "Not in a chats buffer"))
  (unless on-users-fetched
    (error ":on-users-fetched is required"))
  (wasabi--set-status :type 'getting-details :message "Loading...")
  (acp-send-request :client (map-elt (wasabi--state) :client)
                    :request (wasabi--make-admin-users-list-request
                              :admin-token wasabi--admin-token)
                    :on-success (lambda (response)
                                  (funcall on-users-fetched response))
                    :on-failure (lambda (error)
                                  (wasabi--log "Couldn't load user: %s"
                                               (or (map-elt error 'message)
                                                   "Don't know why"))
                                  (wasabi--set-status
                                   :type 'error
                                   :message (wasabi--refresh-error :message "Couldn't load user")))))

(cl-defun wasabi--send-admin-add-user-request (&key on-user-added)
  (unless (derived-mode-p 'wasabi-mode)
    (error "Not in a chats buffer"))
  (unless on-user-added
    (error ":on-user-added is required"))
  (wasabi--set-status :type 'adding-user :message "Initializing client...")
  (acp-send-request :client (map-elt (wasabi--state) :client)
                    :request (wasabi--make-admin-add-user-request
                              :admin-token wasabi--admin-token
                              :name (user-login-name)
                              :token wasabi-user-token
                              :events '("Message"
                                        "Receipt"
                                        "Connected"
                                        "Disconnected"
                                        "ConnectFailure"
                                        "LoggedOut"
                                        "QR"
                                        "PairSuccess"
                                        "PairError"
                                        "StreamError"
                                        "StreamReplaced"
                                        "ClientOutdated"
                                        "TemporaryBan"
                                        "KeepAliveRestored"
                                        "KeepAliveTimeout"
                                        "UndecryptableMessage"
                                        "GroupInfo"
                                        "JoinedGroup"
                                        "IdentityChange"
                                        "HistorySync")
                              :history 100)
                    :on-success (lambda (_response)
                                  (funcall on-user-added))
                    :on-failure (lambda (error)
                                  (wasabi--log "Couldn't add local user: %s" (map-elt error 'message))
                                  (wasabi--set-status
                                   :type 'error
                                   :message (wasabi--refresh-error :message "Couldn't initialize user")))))

;; TODO: Reconsider naming and splitting into two separate requests "index" vs "jid".
(cl-defun wasabi--send-chat-history-request (&key chat-jid contact-name on-finished)
  "Fetch chat history for CHAT-JID and store in state.
CONTACT-NAME is the display name to use for the chat buffer.

When CHAT-JID is \"index\", stores normalized chat index as alist in :chats-index:
  ((\"chat-jid-1\" . chat-metadata-1)
   (\"chat-jid-2\" . chat-metadata-2) ...)

For a specific chat JID, stores message array in :chats and opens chat buffer."
  (unless (derived-mode-p 'wasabi-mode)
    (error "Not in a chats buffer"))
  (unless chat-jid
    (error ":chat-jid is required"))
  (acp-send-request :client (map-elt (wasabi--state) :client)
                    :request (wasabi--make-chat-history-request
                              :token wasabi-user-token
                              :chat-jid chat-jid)
                    :on-success (lambda (response)
                                  (cond
                                   ;; Handle "index" response: ((user-id . [chat-array]))
                                   ;; response: ((user-id . [((chat_jid . "...") (last_updated . "...")) ...]))
                                   ((and (listp response)
                                         (= (length response) 1)
                                         (consp (car response))
                                         (vectorp (cdar response)))
                                    (let ((chats-index (wasabi--parse-chat-index
                                                        (mapcar (lambda (p-chat)
                                                                  (cons (map-elt p-chat 'chat_jid) p-chat))
                                                                (append (cdar response) nil))
                                                        (map-elt (wasabi--state) :contacts)
                                                        (map-elt (wasabi--state) :groups))))
                                      (map-put! (wasabi--state) :chats-index chats-index)
                                      (wasabi--log "Loaded chat index: %d chats" (length chats-index))
                                      (wasabi--refresh)))
                                   ;; Handle specific chat response: [message-array]
                                   ;; Store as alist entry: (chat-jid . p-messages)
                                   (response
                                    (map-put! (wasabi--state) :chats
                                              (map-insert (or (map-elt (wasabi--state) :chats) '())
                                                          chat-jid response))
                                    ;; Parse protocol messages to internal format
                                    (let ((messages (wasabi-chat--parse-messages response
                                                                                 :chat-jid chat-jid
                                                                                 :contact-name contact-name
                                                                                 :contacts (map-elt (wasabi--state) :contacts)))
                                          ;; TODO: Consolidate buffer creation logic.
                                          (chat-buffer (get-buffer (format "*Wasabi: %s*" (or contact-name chat-jid)))))
                                      (if chat-buffer
                                          ;; Buffer exists, just refresh it
                                          (progn
                                            (with-current-buffer chat-buffer
                                              (wasabi-chat--refresh messages))
                                            (switch-to-buffer chat-buffer))
                                        ;; Buffer doesn't exist, start new chat
                                        (wasabi-chat--start :chat-jid chat-jid
                                                            :messages messages
                                                            :contact-name contact-name))))
                                   ((and chat-jid (not (equal chat-jid "index")))
                                    ;; Not an "index" request.
                                    ;; Start new chat (no history).
                                    (wasabi-chat--start :chat-jid chat-jid
                                                        :messages nil ;; no history.
                                                        :contact-name contact-name)))
                                  (when on-finished
                                    (funcall on-finished)))
                    :on-failure (lambda (error)
                                  (message "Failed to fetch chat history: %s" (or (map-elt error 'message) "unknown")))))

(cl-defun wasabi--send-chat-send-text-request (&key phone body on-success on-failure)
  "Send a text message to PHONE with BODY.
Calls ON-SUCCESS when message is sent successfully.
Calls ON-FAILURE with error if sending fails."
  (unless (derived-mode-p 'wasabi-mode 'wasabi-chat-mode)
    (error "Not in a chats buffer"))
  (unless phone
    (error ":phone is required"))
  (unless body
    (error ":body is required"))
  (acp-send-request :client (map-elt (wasabi--state) :client)
                    :request (wasabi--make-chat-send-text-request
                              :token wasabi-user-token
                              :phone phone
                              :body body)
                    :on-success (or on-success
                                    (lambda (_response)
                                      (message "Message sent")))
                    :on-failure (or on-failure
                                    (lambda (error)
                                      (message "Failed to send message: %s" (or (map-elt error 'message) "unknown"))))))

(cl-defun wasabi--send-download-image-request (&key url direct-path media-key mimetype
                                                    file-enc-sha256 file-sha256 file-length
                                                    on-success on-failure)
  "Download and decrypt an image from WhatsApp servers.
Calls ON-SUCCESS with response containing decrypted image data.
Calls ON-FAILURE with error if download fails."
  (unless (derived-mode-p 'wasabi-mode 'wasabi-chat-mode)
    (error "Not in a chats buffer"))
  (unless url
    (error ":url is required"))
  (acp-send-request :client (map-elt (wasabi--state) :client)
                    :request (wasabi--make-download-image-request
                              :token wasabi-user-token
                              :url url
                              :direct-path direct-path
                              :media-key media-key
                              :mimetype mimetype
                              :file-enc-sha256 file-enc-sha256
                              :file-sha256 file-sha256
                              :file-length file-length)
                    :on-success (or on-success
                                    (lambda (_response)
                                      (message "Image downloaded")))
                    :on-failure (or on-failure
                                    (lambda (error)
                                      (message "Failed to download image: %s" (or (map-elt error 'message) "unknown"))))))

(cl-defun wasabi--send-download-video-request (&key url direct-path media-key mimetype
                                                    file-enc-sha256 file-sha256 file-length
                                                    on-success on-failure)
  "Download and decrypt a video from WhatsApp servers.
Calls ON-SUCCESS with response containing decrypted video data.
Calls ON-FAILURE with error if download fails."
  (unless (derived-mode-p 'wasabi-mode 'wasabi-chat-mode)
    (error "Not in a chats buffer"))
  (unless url
    (error ":url is required"))
  (acp-send-request :client (map-elt (wasabi--state) :client)
                    :request (wasabi--make-download-video-request
                              :token wasabi-user-token
                              :url url
                              :direct-path direct-path
                              :media-key media-key
                              :mimetype mimetype
                              :file-enc-sha256 file-enc-sha256
                              :file-sha256 file-sha256
                              :file-length file-length)
                    :on-success (or on-success
                                    (lambda (_response)
                                      (message "Video downloaded")))
                    :on-failure (or on-failure
                                    (lambda (error)
                                      (message "Failed to download video: %s" (or (map-elt error 'message) "unknown"))))))

(cl-defun wasabi--make-session-disconnect-request (&key token)
  "Instantiate a \"session.disconnect\" request.

  Required parameters:
    TOKEN - User authentication token

  Disconnects from WhatsApp without logging out. Session can be
  reconnected later without re-pairing. Clears event subscriptions
  in the database.

  See: stdio.go:189, handlers.go (disconnectHandler)"
  (unless token
    (error ":token is required"))
  `((:method . "session.disconnect")
    (:params . ((token . ,token)))))

(defun wasabi--fetch-chat-index ()
  "Fetch contacts, groups, then chat index from WhatsApp."
  (wasabi--log "Fetching contacts first for chat index...")
  (wasabi--set-status :type 'fetching-contacts :message "Loading contacts...")
  (wasabi--fetch-contacts
   :on-success (lambda ()
                 (wasabi--log "Contacts fetched, now fetching groups...")
                 (wasabi--set-status :type 'fetching-groups :message "Loading groups...")
                 (wasabi--fetch-groups
                  :on-success (lambda ()
                                (wasabi--log "Groups fetched, now fetching chat index...")
                                (wasabi--set-status :type 'fetching-chats :message "Loading chats...")
                                (wasabi--send-chat-history-request
                                 :chat-jid "index"
                                 :on-finished (lambda ()
                                                (wasabi--set-status :type nil :message nil))))))))

(cl-defun wasabi--fetch-groups (&key on-success)
  "Fetch group list from WhatsApp.
Calls ON-SUCCESS after groups are fetched and parsed."
  (wasabi--log "Fetching groups...")
  (acp-send-request :client (map-elt (wasabi--state) :client)
                    :request (wasabi--make-group-list-request
                              :token wasabi-user-token)
                    :on-success (lambda (response)
                                  ;; response: ((Groups . [group-array]))
                                  (let* ((p-groups (if (and (listp response)
                                                            (= (length response) 1)
                                                            (consp (car response)))
                                                       (cdar response)
                                                     response))
                                         (groups (wasabi--parse-groups p-groups)))
                                    (map-put! (wasabi--state) :groups groups)
                                    (wasabi--log "Fetched %d groups" (length groups))
                                    (when on-success
                                      (funcall on-success))))
                    :on-failure (lambda (error)
                                  (wasabi--log "Failed to fetch groups: %s" (map-elt error 'message)))))

(cl-defun wasabi--fetch-contacts (&key on-success)
  "Fetch contacts from WhatsApp if not already successfully fetched.
Only marks as fetched if we get at least 1 contact, allowing retries
on fresh pairings where contacts may not be synced yet.
Calls ON-SUCCESS after contacts are fetched and parsed."
  (if (map-elt wasabi--state :contacts-fetched)
      (progn
        (wasabi--log "Contacts already fetched, skipping")
        (when on-success
          (funcall on-success)))
    (wasabi--log "Fetching contacts...")
    (wasabi--set-status :type 'fetching-contacts :message "Loading...")
    (acp-send-request :client (map-elt wasabi--state :client)
                      :request (wasabi--make-user-contacts-request
                                :token wasabi-user-token)
                      :on-success (lambda (response)
                                    (let* ((contacts (wasabi--parse-contacts response))
                                           (count (length contacts)))
                                      (map-put! (wasabi--state) :contacts contacts)
                                      (wasabi--log "Fetched %d contacts" count)
                                      (if (> count 0)
                                          (progn
                                            (map-put! (wasabi--state) :contacts-fetched t)
                                            (wasabi--set-status :type nil :message nil)
                                            (when on-success
                                              (funcall on-success)))
                                        (wasabi--log "No contacts synced yet, waiting for sync..."))))
                      :on-failure (lambda (error)
                                    (wasabi--log "Failed to fetch contacts: %s" (map-elt error 'message))
                                    (wasabi--set-status :type 'error
                                                        :message (wasabi--refresh-error :message "Failed to fetch contacts"))))))

(cl-defun wasabi--initialize-subscriptions ()
  "Initialize json-rpc subscriptions with SHELL.."
  (unless (map-elt wasabi--state :client)
    (error "Missing client"))
  (acp-subscribe-to-errors
   :client (map-elt wasabi--state :client)
   :on-error (lambda (error)
               (wasabi--log "Something is wrong %s" error)
               (wasabi--set-status
                :type 'error
                :message (wasabi--refresh-error :message "Something is not right"))))
  (acp-subscribe-to-notifications
   :client (map-elt wasabi--state :client)
   :on-notification (lambda (notification)
                      (wasabi--log "Notification: %s" (map-elt notification 'method))
                      (cond ((equal (map-elt notification 'method) "QR")
                             (when (map-nested-elt notification '(params qrCodeBase64))
                               (wasabi--display-qr-code (map-nested-elt notification '(params qrCodeBase64)))))
                            ((equal (map-elt notification 'method) "PairSuccess")
                             (wasabi--log "Pairing successful"))
                            ((equal (map-elt notification 'method) "Connected")
                             (map-put! (wasabi--state) :connected t)
                             (wasabi--log "Connected to WhatsApp"))
                            ((equal (map-elt notification 'method) "OfflineSyncCompleted")
                             (wasabi--log "Offline sync completed")
                             (when (and (not (map-elt (wasabi--state) :chats-index))
                                        (map-elt (wasabi--state) :connected))
                               (wasabi--fetch-chat-index)))
                            ((equal (map-elt notification 'method) "AppStateSyncComplete")
                             (wasabi--log "App state sync complete"))
                            ((equal (map-elt notification 'method) "ConnectFailure")
                             (map-put! (wasabi--state) :connected nil)
                             (wasabi--log "Couldn't connect: %s"
                                          (format "%s" (or (map-nested-elt notification '(params reason))
                                                           (map-nested-elt notification '(params error))
                                                           "???")))
                             (wasabi--set-status
                              :type 'error
                              :message (wasabi--refresh-error :message "Couldn't connect")))
                            ((equal (map-elt notification 'method) "Disconnected")
                             (map-put! (wasabi--state) :connected nil)
                             (wasabi--set-status
                              :type 'disconnected
                              :message (wasabi--refresh-error :message "Disconnected")))
                            ((equal (map-elt notification 'method) "Message")
                             (let* ((p-message (map-nested-elt notification '(params event Message)))
                                    (p-info (map-nested-elt notification '(params event Info)))
                                    (chat-jid (if (symbolp (map-elt p-info 'Chat))
                                                  (symbol-name (map-elt p-info 'Chat))
                                                (map-elt p-info 'Chat)))
                                    ;; Must capture contacts before with-current-buffer.
                                    (contacts (map-elt (wasabi--state) :contacts)))
                               (dolist (buffer (buffer-list))
                                 (with-current-buffer buffer
                                   (when-let ((message (and (derived-mode-p 'wasabi-chat-mode)
                                                            (equal (map-elt wasabi-chat--chat :chat-jid) chat-jid)
                                                            (wasabi-chat--parse-notification
                                                             :p-message p-message
                                                             :p-info p-info
                                                             :contact-name (map-elt wasabi-chat--chat :contact-name)
                                                             :chat-jid chat-jid
                                                             :contacts contacts))))
                                     (if (map-elt message :is-reaction)
                                         (wasabi-chat--add-reaction
                                          :target-id (map-elt message :target-id)
                                          :emoji (map-elt message :emoji)
                                          :sender (map-elt message :sender))
                                       (wasabi-chat--append-message message)))))))
                            ((equal (map-elt notification 'method) "HistorySync")
                             (wasabi--log "HistorySync received")
                             (wasabi--fetch-chat-index))))))

(defun wasabi--log (format-string &rest args)
  "Log a debug message to *Wasabi-Log* buffer."
  (with-current-buffer (get-buffer-create "*Wasabi-Log*")
    (goto-char (point-max))
    (insert (format-time-string "[%Y-%m-%d %H:%M:%S] "))
    (insert (apply #'format format-string args))
    (insert "\n")))

(cl-defun wasabi--refresh-error (&key message)
  "Return error MESSAGE with refresh instructions.
If refresh keybinding exists, appends \"\\n\\n<keybinding> to retry\".
Otherwise returns just the message.

MESSAGE should be a string like \"Failed to connect\"."
  (if-let ((key (car (where-is-internal 'wasabi-reload))))
      (concat message "\n\n"
              (propertize (key-description key) 'face 'help-key-binding)
              " to retry")
    message))

(defun wasabi--display-qr-code (base64-data)
  "Display a QR code from BASE64-DATA in the home buffer."
  (unless (derived-mode-p 'wasabi-mode)
    (error "Not in a chats buffer"))
  (let ((image (create-image (base64-decode-string
                              (replace-regexp-in-string
                               "^data:image/png;base64," "" base64-data))
                             'png t)))
    (wasabi--set-status :type 'qr
                        :message (concat
                                  (propertize
                                   ;; Pad image string with * so it can be centered in screen.
                                   (concat (make-string (round (car (image-size image))) ?*) "\n")
                                   'display image)
                                  "\nScan from WhatsApp mobile to enable client"))))

(cl-defun wasabi--message (&key text)
  "Display centered TEXT centered in current buffer.

This function displayes TEXT only, wiping everything else."
  (interactive)
  (unless (derived-mode-p 'wasabi-mode)
    (error "Not in a chats buffer"))
  (let* ((lines (split-string (or text "") "\n"))
         (n-lines (length lines))
         (win-height (window-body-height))
         (win-width  (window-body-width))
         (top-pad (max 0 (/ (- win-height n-lines) 2)))
         centered-lines)
    ;; Compute centered each line horizontally
    (setq centered-lines
          (mapcar
           (lambda (line)
             (let* ((col-pad (max 0 (/ (- win-width (string-width line)) 2)))
                    (spaces (make-string col-pad ?\s)))
               (concat spaces line)))
           lines))
    (let ((inhibit-read-only t))
      (erase-buffer)
      ;; Pad with newlines to center vertically
      (insert (make-string top-pad ?\n))
      ;; Insert centered lines
      (dolist (cl centered-lines)
        (insert cl "\n")))))

(cl-defun wasabi--make-state (&key home-buffer)
  "Construct chat client state with home HOME-BUFFER."
  (list (cons :client nil)
        (cons :home-buffer home-buffer)
        (cons :status nil)
        (cons :user-exists-checked nil)
        (cons :status-checked nil)
        (cons :connected nil)
        (cons :contacts-fetched nil)
        ;; Sample contacts structure:
        ;;
        ;; ((555123456789@lid (BusinessName . "") (FirstName . "John") (Found . t)
        ;;                    (FullName . "John Smith") (PushName . "Johnny") (RedactedPhone . ""))
        ;;  (555987654321@lid (BusinessName . "Acme Corp") (FirstName . "Jane") (Found . t)
        ;;                    (FullName . "Jane Doe") (PushName . "Jane") (RedactedPhone . "")))
        (cons :contacts nil)
        ;; Sample chats index structure:
        ;;
        ;; (("1234567890@s.whatsapp.net" . ((chat_jid . "1234567890@s.whatsapp.net")
        ;;                                  (last_updated . "2025-11-11 12:00:00.000000 +0000 GMT")))
        ;;  ("987654321@g.us" . ((chat_jid . "987654321@g.us")
        ;;                       (last_updated . "2025-11-10 18:30:00.000000 +0000 GMT")))
        ;;  ...)
        (cons :chats-index nil)
        ;; Sample chats structure:
        ;;
        ;; (("1234567890@s.whatsapp.net" . [((chat_jid . "1234567890@s.whatsapp.net")
        ;;                                   (message_id . "ABC123DEF456")
        ;;                                   (message_type . "text")
        ;;                                   (sender_jid . "9876543210@s.whatsapp.net")
        ;;                                   (text_content . "Hello world")
        ;;                                   (timestamp . "2025-11-11T12:00:00Z")
        ;;                                   (data_json . "{}")
        ;;                                   (media_link . "")
        ;;                                   (id . 1)
        ;;                                   (user_id . "user123"))
        ;;                                  ...])
        ;;  ("987654321@g.us" . [...])
        ;;  ...)
        (cons :chats nil)
        (cons :groups nil)))

(cl-defun wasabi--make-status (&key type message)
  "Create a status object with TYPE and optional MESSAGE.
TYPE can be: loading, connecting, connected, disconnected, error, etc.
MESSAGE is displayed if present."
  `((:type . ,type)
    (:message . ,message)))

(cl-defun wasabi--set-status (&key type message)
  "Set the current status and refresh the display."
  (wasabi--log "Status change: %s \"%s\"" type message)
  (unless (derived-mode-p 'wasabi-mode)
    (error "Not in a chats buffer"))
  (map-put! (wasabi--state) :status (wasabi--make-status :type type :message message))
  ;; Header line should be present after loading.
  (if type
      (setq header-line-format nil)
    (wasabi--update-header-line))
  (wasabi--refresh))

(defun wasabi--timezone ()
  "Return the current time zone."
  (or
   ;; macOS: Try /etc/localtime symlink
   (let ((target (ignore-errors (file-truename "/etc/localtime"))))
     (when (and target
                (string-match "\\(\\([A-Za-z_]+\\)/\\([A-Za-z_+-]+\\)\\)$" target))
       (match-string 1 target)))
   ;; macOS: systemsetup command
   (let ((tz (string-trim
              (ignore-errors
                (shell-command-to-string
                 "systemsetup -gettimezone 2>/dev/null | awk -F': ' '{print $2}'")))))
     (when (and tz (string-match "/" tz))
       tz))
   ;; Linux: /etc/timezone
   (let ((tz (and (file-exists-p "/etc/timezone")
                  (with-temp-buffer
                    (insert-file-contents "/etc/timezone")
                    (string-trim (buffer-string))))))
     (when (and tz (string-match "/" tz))
       tz))
   ;; Fallback: TZ env variable
   (let ((tz (getenv "TZ")))
     (when (and tz (string-match "/" tz))
       tz))
   (error "Couldn't determine timezone")))

(defun wasabi--make-action-keymap (action)
  "Create keymap with ACTION."
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] action)
    (define-key map (kbd "RET") action)
    (define-key map [remap self-insert-command] 'ignore)
    map))

(defun wasabi--add-action-to-text (text action &optional on-entered face)
  "Add ACTION lambda to propertized TEXT and return modified text.
ON-ENTERED is a function to call when the cursor enters the text.
FACE when non-nil applies the specified face to the text."
  (add-text-properties 0 (length text)
                       `(keymap ,(wasabi--make-action-keymap action)
                                mouse-face highlight
                                pointer hand)
                       text)
  (when on-entered
    (add-text-properties 0 (length text)
                         (list 'cursor-sensor-functions
                               (list (lambda (_window _old-pos sensor-action)
                                       (when (eq sensor-action 'entered)
                                         (funcall on-entered)))))
                         text))
  (when face
    (add-text-properties 0 (length text)
                         `(font-lock-face ,face)
                         text)
    (add-text-properties 0 (length text)
                         `(face ,face)
                         text))
  text)

(defvar wasabi-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'next-line)
    (define-key map (kbd "p") #'previous-line)
    (define-key map (kbd "C") #'wasabi-new-chat)
    (define-key map (kbd "c") #'wasabi-new-chat)
    (define-key map (kbd "+") #'wasabi-new-chat-new-number)
    (define-key map (kbd "q") #'wasabi-quit)
    (define-key map (kbd "g") #'wasabi-reload)
    map)
  "Keymap for `wasabi-mode'.")

(defun wasabi--update-header-line ()
  "Update the header line for the main chats app buffer."
  (let ((bindings `((:command wasabi-new-chat :description "new chat")
                    (:command wasabi-new-chat-new-number :description "new number")
                    (:command wasabi-reload :description "refresh")
                    (:command wasabi-quit :description "quit"))))
    (setq header-line-format
          (concat
           " "
           (propertize "Recent Chats and Groups" 'face 'font-lock-doc-face)
           " "
           (mapconcat
            #'identity
            (seq-filter
             #'identity
             (mapcar
              (lambda (binding)
                (when-let* ((command (map-elt binding :command))
                            (description (map-elt binding :description))
                            (keys (where-is-internal command wasabi-mode-map))
                            (key (key-description (car keys))))
                  (concat
                   (propertize key 'face 'help-key-binding)
                   " "
                   description)))
              bindings))
            " ")))))

(define-derived-mode wasabi-mode fundamental-mode "Wasabi"
  "Major mode for chat interfaces.

\\{wasabi-mode-map}"
  (setq buffer-read-only t)
  (wasabi--update-header-line)
  (goto-char (point-max)))

(defun wasabi-quit ()
  (interactive)
  (unless (derived-mode-p 'wasabi-mode)
    (error "Not in a chats buffer"))
  (quit-restore-window (get-buffer-window (current-buffer)) 'kill))

(defun wasabi ()
  "Create or switch to the `*Wasabi*` buffer in `wasabi-mode`."
  (interactive)
  (let ((home-buffer (get-buffer-create "*Wasabi*")))
    (with-current-buffer home-buffer
      (unless (derived-mode-p 'wasabi-mode)
        (wasabi-mode))
      (add-hook 'kill-buffer-hook #'wasabi--clean-up nil t)
      (add-hook 'window-size-change-functions
                (lambda (_frame)
                  (with-current-buffer home-buffer
                    (when (map-nested-elt (wasabi--state) '(:status :message))
                      (wasabi--refresh)))) nil t)
      (add-hook 'window-configuration-change-hook
                (lambda ()
                  (with-current-buffer home-buffer
                    (when (map-nested-elt (wasabi--state) '(:status :message))
                      (wasabi--refresh)))) nil t)
      (wasabi--initialize :home-buffer home-buffer))
    (switch-to-buffer home-buffer)
    (wasabi--refresh)))

(defun wasabi--buffer ()
  "Get the `wasabi'.

Error if not found."
  (or (get-buffer "*Wasabi*")
      (user-error "Wasabi buffer not found (start with M-x wasabi)")))

(defun wasabi-reload ()
  (interactive)
  (unless (derived-mode-p 'wasabi-mode)
    (user-error "Not in a chats buffer"))
  (wasabi--log "Refresh requested, resetting state and restarting...")
  (let ((home-buffer (current-buffer)))
    (when (map-elt (wasabi--state) :client)
      (acp-shutdown :client (map-elt (wasabi--state) :client)))
    (setq wasabi--state nil)
    (wasabi--initialize :home-buffer home-buffer)))

(defun wasabi-new-chat-new-number ()
  "Start a new chat with a new phone number."
  (interactive)
  (let ((current-prefix-arg t))
    (call-interactively #'wasabi-new-chat)))

(defun wasabi-new-chat (new-number)
  "Select a contact or group using completing-read and open the chat.
With prefix argument, prompt for a phone number to chat with directly."
  (interactive
   (list (when current-prefix-arg
           (read-string "Phone number (with country code, e.g., 447123456789): "))))
  (with-current-buffer (wasabi--buffer)
    (if new-number
        ;; Direct phone number chat
        (wasabi--send-chat-history-request
         :chat-jid (concat new-number "@s.whatsapp.net")
         :contact-name (string-trim new-number))
      ;; Normal contact/group selection
      (unless (or (map-elt (wasabi--state) :contacts)
                  (map-elt (wasabi--state) :groups))
        (user-error "No contacts or groups available"))
      (let* ((contact-entries
              (mapcar (lambda (contact-entry)
                        (let* ((jid (symbol-name (car contact-entry)))
                               (full-name (map-elt (cdr contact-entry) :full-name))
                               (push-name (map-elt (cdr contact-entry) :push-name))
                               (display-name (or (and full-name (not (string-empty-p full-name)) full-name)
                                                 (and push-name (not (string-empty-p push-name)) push-name)
                                                 jid)))
                          `((:display-name . ,display-name)
                            (:jid . ,jid)
                            (:is-group . nil))))
                      (map-elt (wasabi--state) :contacts)))
             (group-entries
              (mapcar (lambda (group-entry)
                        (let* ((jid (symbol-name (car group-entry)))
                               (group-info (cdr group-entry))
                               (group-name (map-elt group-info :name))
                               (display-name (or (and group-name (not (string-empty-p group-name)) group-name)
                                                 jid)))
                          `((:display-name . ,display-name)
                            (:jid . ,jid)
                            (:is-group . t))))
                      (map-elt (wasabi--state) :groups)))
             (all-entries
              (sort
               (seq-filter
                (lambda (entry)
                  ;; Filter out unknown groups or numbers
                  (and (not (string-suffix-p "@lid" (map-elt entry :display-name)))
                       (not (string-suffix-p "@g.us" (map-elt entry :display-name)))))
                (append contact-entries group-entries))
               (lambda (a b) (string< (map-elt a :display-name) (map-elt b :display-name)))))
             (max-width (apply #'max (mapcar (lambda (entry) (string-width (map-elt entry :display-name))) all-entries)))
             (candidates
              (mapcar (lambda (entry)
                        ;; return list of (label . jid)
                        (cons (if (map-elt entry :is-group)
                                  (concat (map-elt entry :display-name)
                                          ;; Pad using longest contact name.
                                          (make-string (- max-width (string-width (map-elt entry :display-name))) ?\s)
                                          " (group)")
                                (map-elt entry :display-name))
                              (map-elt entry :jid)))
                      all-entries)))
        (if-let* ((selected-label (completing-read "Chat with: " candidates nil t))
                  (selected-jid (map-elt candidates selected-label))
                  (selected-entry (seq-find (lambda (entry)
                                              (string= (map-elt entry :jid) selected-jid))
                                            all-entries)))
            (wasabi--send-chat-history-request
             :chat-jid selected-jid
             :contact-name (concat (map-elt selected-entry :display-name)
                                   (when (map-elt selected-entry :is-group)
                                     " (group)")))
          (user-error "No contact or group found"))))))

(defun wasabi-open-data-directory ()
  "Open data directory (database, media, etc)."
  (interactive)
  (find-file (wasabi-data-dir)))

(defalias 'wasabi-new-message #'wasabi-new-chat)

(defun wasabi--refresh ()
  "Refresh the display based on current status."
  (save-excursion
    (let* ((status (map-elt (wasabi--state) :status))
           (message (when status (map-elt status :message)))
           (chats-index (map-elt (wasabi--state) :chats-index)))
      (cond
       ;; If we have chats and no blocking status message, display chat list
       ((and chats-index (not message))
        (let* ((max-name-width (apply #'max
                                      (mapcar (lambda (chat) (string-width (map-elt chat :display-name)))
                                              chats-index)))
               ;; Format with aligned columns and add actions
               (chat-lines
                (mapcar
                 (lambda (chat)
                   (wasabi--add-action-to-text
                    ;; Recent contact line
                    (concat (map-elt chat :display-name)
                            ;; padding
                            (make-string (- max-name-width (string-width (map-elt chat :display-name))) ?\s)
                            (when (map-elt chat :is-group)
                              (propertize " (group)" 'face 'font-lock-comment-face)))
                    (lambda ()
                      (interactive)
                      (wasabi--send-chat-history-request
                       :chat-jid (map-elt chat :chat-jid)
                       :contact-name (map-elt chat :display-name)))))
                 chats-index)))
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert "\n")
            (insert (mapconcat #'identity chat-lines "\n")))))
       ;; Otherwise show status message
       (message
        (wasabi--message :text message))))))

(defun wasabi--state ()
  "Get shell state or fail in an incompatible buffer."
  (unless (derived-mode-p 'wasabi-mode)
    (error "No access outside wasabi-mode: %s" major-mode))
  (unless wasabi--state
    (error "No wasabi-mode state available"))
  wasabi--state)

(defun wasabi--clean-up ()
  "Clean up resources.

For example, shut down wuzapi process."
  (unless (derived-mode-p 'wasabi-mode)
    (error "Not in a chats buffer"))
  (when (map-elt (wasabi--state) :client)
    (acp-shutdown :client (map-elt (wasabi--state) :client))))

;; Protocol parsing functions

(defun wasabi--parse-contact (p-contact)
  "Parse protocol contact to internal format.
P-CONTACT is an alist from user.contacts response.
Returns alist with :full-name and :push-name."
  (let ((full-name (map-elt p-contact 'FullName))
        (push-name (map-elt p-contact 'PushName)))
    `((:full-name . ,full-name)
      (:push-name . ,push-name))))

(defun wasabi--parse-contacts (p-contacts)
  "Parse contacts response to alist of (jid . contact-info).
P-CONTACTS is the response from user.contacts.
Returns alist: ((jid1 . contact1) (jid2 . contact2) ...)"
  (mapcar (lambda (p-contact-entry)
            (let ((jid (car p-contact-entry))
                  (p-contact-data (cdr p-contact-entry)))
              (cons jid (wasabi--parse-contact p-contact-data))))
          p-contacts))

(defun wasabi--parse-group (p-group)
  "Parse protocol group to internal format.
P-GROUP is an alist from group.list response.
Returns alist with :name."
  `((:name . ,(map-elt p-group 'Name))))

(defun wasabi--parse-groups (p-groups)
  "Parse groups response to alist of (jid . group-info).
P-GROUPS is the response from group.list (vector or list of groups).
Returns alist: ((jid1 . group1) (jid2 . group2) ...)"
  (mapcar (lambda (p-group)
            ;; TODO: Do we need symbols here? Why not keep as string?
            (let ((jid (intern (map-elt p-group 'JID))))
              (cons jid (wasabi--parse-group p-group))))
          (append p-groups nil)))

(defun wasabi--parse-chat-index-entry (p-chat-entry contacts groups)
  "Parse protocol chat index entry with name enrichment.
P-CHAT-ENTRY is protocol chat entry:
 (chat-jid . ((chat_jid . ...) (last_updated . ...))).
CONTACTS is internal contacts alist (already parsed).
GROUPS is internal groups alist (already parsed).
Returns alist with :chat-jid, :display-name, :last-updated, :is-group."
  (let* ((chat-jid (car p-chat-entry))
         (p-metadata (cdr p-chat-entry))
         (last-updated (map-elt p-metadata 'last_updated))
         (is-group (string-match "@g\\.us$" chat-jid))
         (name (if is-group
                   ;; TODO: Do we need symbols here? Why not keep as string?
                   (map-nested-elt groups (list (intern chat-jid) :name))
                 ;; For contacts, look up in contacts list
                 (when-let ((contact (map-elt contacts (intern chat-jid))))
                   (or (map-elt contact :full-name)
                       (map-elt contact :push-name)))))
         (identifier (when (string-match "\\([^@]+\\)@" chat-jid)
                       (match-string 1 chat-jid)))
         (display-name (or name identifier chat-jid)))
    `((:chat-jid . ,chat-jid)
      (:display-name . ,display-name)
      (:is-group . ,is-group)
      (:last-updated . ,last-updated))))

(defun wasabi--parse-chat-index (p-chat-index contacts groups)
  "Parse chat index response to list of internal chat entries.
P-CHAT-INDEX is the raw response from chat.history with chat_jid='index'.
CONTACTS is internal contacts alist (already parsed).
GROUPS is internal groups alist (already parsed).
Returns list of internal chat entry alists, filtered and sorted."
  (let* ((parsed (delq nil
                       (mapcar (lambda (p-entry)
                                 ;; Filter out status broadcasts
                                 (unless (string= (car p-entry) "status@broadcast")
                                   (wasabi--parse-chat-index-entry p-entry contacts groups)))
                               p-chat-index)))
         (sorted (sort parsed
                       (lambda (a b)
                         (string> (or (map-elt a :last-updated) "")
                                  (or (map-elt b :last-updated) ""))))))
    sorted))

;; Protocol request builders

(cl-defun wasabi--make-session-connect-request (&key token subscribe immediate)
  "Instantiate a \"session.connect\" request.

  Required parameters:
    TOKEN - User authentication token

  Optional parameters:
    SUBSCRIBE - List of event types to subscribe to during connection
    IMMEDIATE - Whether to immediately trigger connection process

  Initiates a WhatsApp connection for the user, preparing for QR code
  scanning or device pairing."
  (unless token
    (error ":token is required"))
  (let ((params `((token . ,token))))
    (when subscribe
      (push `(subscribe . ,(vconcat subscribe)) params))
    (when immediate
      (push `(immediate . ,immediate) params))
    `((:method . "session.connect")
      (:params . ,params))))

(cl-defun wasabi--make-user-contacts-request (&key token)
  "Instantiate a \"user.contacts\" request to get all contacts."
  (unless token (error ":token is required"))
  `((:method . "user.contacts")
    (:params . ((token . ,token)))))

(cl-defun wasabi--make-group-list-request (&key token)
  "Instantiate a \"group.list\" request to get all groups."
  (unless token (error ":token is required"))
  `((:method . "group.list")
    (:params . ((token . ,token)))))

(cl-defun wasabi--make-chat-history-request (&key token chat-jid)
  "Instantiate a \"chat.history\" request.

  Required parameters:
    TOKEN - User authentication token
    CHAT-JID - Chat JID identifying the conversation:
               - For contacts: \"1234567890@s.whatsapp.net\"
               - For groups: \"groupid@g.us\"
               - Special value: \"index\" returns mapping of all chats

  Retrieves message history for a specific chat. History must be
  enabled when creating the user account.

  See: stdio.go:199, handlers.go (getMessagesHandler)"
  (unless token
    (error ":token is required"))
  (unless chat-jid
    (error ":chat-jid is required"))
  `((:method . "chat.history")
    (:params . ((token . ,token)
                (chat_jid . ,chat-jid)))))

(cl-defun wasabi--make-chat-clear-request (&key token chat-jid)
  "Instantiate a \"chat.clear\" request.

  Required parameters:
    TOKEN - User authentication token
    CHAT-JID - Chat JID identifying the conversation to clear

  Clears the local message history for a specific chat in the database.
  This only affects the local wuzapi database, not the actual WhatsApp chat."
  (unless token
    (error ":token is required"))
  (unless chat-jid
    (error ":chat-jid is required"))
  `((:method . "chat.clear")
    (:params . ((token . ,token)
                (chat_jid . ,chat-jid)))))

(cl-defun wasabi--make-admin-users-list-request (&key admin-token)
  "Instantiate an \"admin.users.list\" request.

  Required parameters:
    ADMIN-TOKEN - Admin authentication token

  Returns array of all user accounts with their details including
  connection status, JID, webhook configuration, and event subscriptions.

  See: stdio.go:159, handlers.go (listUsersHandler)"
  (unless admin-token
    (error ":admin-token is required"))
  `((:method . "admin.users.list")
    (:params . ((adminToken . ,admin-token)))))

(cl-defun wasabi--make-admin-add-user-request (&key admin-token
                                                    name
                                                    token
                                                    events
                                                    hmac-key
                                                    expiration
                                                    history
                                                    proxy-config
                                                    s3-config)
  "Instantiate an \"admin.users.add\" request."
  (unless admin-token (error ":admin-token is required"))
  (unless name (error ":name is required"))
  (unless token (error ":token is required"))
  `((:method . "admin.users.add")
    (:params . ,(append `((adminToken . ,admin-token)
                          (name . ,name)
                          (token . ,token)
                          (events . ,(and events
                                          (if (listp events)
                                              (mapconcat #'identity events ",")
                                            events)))
                          (history . ,history)
                          (proxyConfig . ,proxy-config)
                          (s3Config . ,s3-config))
                        (when expiration
                          `((expiration . ,expiration)))
                        (when hmac-key
                          `((hmacKey . ,hmac-key)))))))

(cl-defun wasabi--make-session-status-request (&key token)
  "Instantiate a \"session.status\" request.

  Required parameters:
    TOKEN - User authentication token

  Returns connection and login status including connected state,
  logged in state, JID, name, and configuration status.

  See: stdio.go:186, handlers.go (statusHandler)"
  (unless token
    (error ":token is required"))
  `((:method . "session.status")
    (:params . ((token . ,token)))))

(cl-defun wasabi--make-chat-send-text-request (&key token
                                                    phone
                                                    body
                                                    link-preview
                                                    id
                                                    context-info
                                                    quoted-text)
  "Instantiate a \"chat.send.text\" request.

  Required parameters:
    TOKEN - User authentication token
    PHONE - Phone number with country code or group JID
    BODY - Message text content

  Optional parameters:
    LINK-PREVIEW - Enable link preview for URLs (boolean)
    ID - Custom message ID (string, auto-generated if not provided)
    CONTEXT-INFO - Context object with :stanza-id and :participant
    QUOTED-TEXT - Text to quote from another message

  Sends a text message to a WhatsApp contact or group. Requires
  an active WhatsApp session (connected and logged in).

  See: stdio.go:196, handlers.go (sendHandler)"
  (unless token
    (error ":token is required"))
  (unless phone
    (error ":phone is required"))
  (unless body
    (error ":body is required"))
  (let ((params `((token . ,token)
                  (Phone . ,phone)
                  (Body . ,body))))
    (when link-preview
      (push `(LinkPreview . ,link-preview) params))
    (when id
      (push `(Id . ,id) params))
    (when context-info
      (push `(ContextInfo . ,context-info) params))
    (when quoted-text
      (push `(QuotedText . ,quoted-text) params))
    `((:method . "chat.send.text")
      (:params . ,params))))

(cl-defun wasabi--make-download-image-request (&key token
                                                    url
                                                    direct-path
                                                    media-key
                                                    mimetype
                                                    file-enc-sha256
                                                    file-sha256
                                                    file-length)
  "Instantiate a \"chat.download.image\" request.

  Required parameters:
    TOKEN - User authentication token
    URL - WhatsApp media URL
    DIRECT-PATH - WhatsApp direct path to encrypted file
    MEDIA-KEY - Base64 encryption key for decrypting the image
    MIMETYPE - Image MIME type (e.g., \"image/jpeg\")
    FILE-ENC-SHA256 - SHA256 hash of encrypted file
    FILE-SHA256 - SHA256 hash of decrypted file
    FILE-LENGTH - File size in bytes

  Downloads and decrypts an image from WhatsApp servers. Returns
  a data URL with the decrypted image data.

  See: stdio.go:248, handlers.go (DownloadImage)"
  (unless token
    (error ":token is required"))
  (unless url
    (error ":url is required"))
  `((:method . "chat.download.image")
    (:params . ((token . ,token)
                (Url . ,url)
                (DirectPath . ,direct-path)
                (MediaKey . ,media-key)
                (Mimetype . ,mimetype)
                (FileEncSHA256 . ,file-enc-sha256)
                (FileSHA256 . ,file-sha256)
                (FileLength . ,file-length)))))

(cl-defun wasabi--make-download-video-request (&key token
                                                    url
                                                    direct-path
                                                    media-key
                                                    mimetype
                                                    file-enc-sha256
                                                    file-sha256
                                                    file-length)
  "Instantiate a \"chat.download.video\" request.

  Required parameters:
    TOKEN - User authentication token
    URL - WhatsApp media URL
    DIRECT-PATH - WhatsApp direct path to encrypted file
    MEDIA-KEY - Base64 encryption key for decrypting the video
    MIMETYPE - Video MIME type (e.g., \"video/mp4\")
    FILE-ENC-SHA256 - SHA256 hash of encrypted file
    FILE-SHA256 - SHA256 hash of decrypted file
    FILE-LENGTH - File size in bytes

  Downloads and decrypts a video from WhatsApp servers. Returns
  a data URL with the decrypted video data.

  See: stdio.go:251, handlers.go (DownloadVideo)"
  (unless token
    (error ":token is required"))
  (unless url
    (error ":url is required"))
  `((:method . "chat.download.video")
    (:params . ((token . ,token)
                (Url . ,url)
                (DirectPath . ,direct-path)
                (MediaKey . ,media-key)
                (Mimetype . ,mimetype)
                (FileEncSHA256 . ,file-enc-sha256)
                (FileSHA256 . ,file-sha256)
                (FileLength . ,file-length)))))

(provide 'wasabi)
;;; wasabi.el ends here
