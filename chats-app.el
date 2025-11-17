;;; chats-app.el --- A WhatsApp Emacs client  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Alvaro Ramirez

;; Author: Alvaro Ramirez https://xenodium.com
;; URL: https://github.com/xenodium/chats-app
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

;; chats-app provides a native Emacs interface for WhatsApp messaging.
;; It communicates with a wuzapi process over standard I/O using json-rpc.

;;; Code:

(require 'acp) ;; for json-rpc.
(eval-when-compile
  (require 'cl-lib))
(require 'map)
(require 'seq)
(require 'chats-app-chat)

(defcustom chats-app-user-token (concat
                                 (or (user-login-name)
                                     (error "chats-app: No login name available"))
                                 "-token")
  "User token for identifying wuzapi user.

Defaults to (user-login-name) + \"-token\".

This token identifies the single user added to the wuzapi database and
must remain constant across Emacs sessions. Changing this will cause
you to lose your WhatsApp session and require re-scanning the QR code.

`chats-app' communicates with a wuzapi process over standard I/O, thus
this token is not used for network security, but for rather user
identification in the wuzapi database.

Each wuzapi user has their own WhatsApp session associated with their
token.

If you need to start fresh delete the wuzapi database files."
  :type 'string
  :group 'chats-app)

(defcustom chats-app-wuzapi-command
  '("wuzapi" "-mode=stdio")
  "Command and parameters for the Anthropic Claude client.

The first element is the command name, and the rest are command parameters."
  :type '(repeat string)
  :group 'chats-app)

(defvar chats-app--admin-token "emacs-admin-token"
  "Wuzapi admin token for administrative operations.

`chats-app' communicates with a wuzapi process over standard I/O.
While this token is not technically required for network authentication,
it is required internally by the process.")

(defconst chats-app--event-subscriptions
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

(defvar-local chats-app--state nil)

(cl-defun chats-app--initialize (&key home-buffer)
  (unless chats-app--state
    (setq chats-app--state (chats-app--make-state :home-buffer home-buffer)))
  (cond ((not (map-elt (chats-app--state) :client))
         ;; Step 1: Create client
         (chats-app--set-status :type 'loading :message "Loading...")
         ;; chats-app-wuzapi-command: '("/path/to/wuzapi" "-mode=stdio" ...)
         (map-put! (chats-app--state)
                   :client (acp-make-client :command (car chats-app-wuzapi-command)
                                            :command-params (cdr chats-app-wuzapi-command)
                                            :environment-variables (list (concat "WUZAPI_ADMIN_TOKEN=" chats-app--admin-token)
                                                                         (concat "TZ=" (chats-app--timezone)))
                                            :context-buffer home-buffer))
         (chats-app--initialize :home-buffer home-buffer))
        ((not (map-nested-elt (chats-app--state) '(:client :notification-handlers)))
         ;; Step 2: Initialize subscriptions
         (chats-app--log "Initializing subscriptions")
         (chats-app--initialize-subscriptions)
         (chats-app--log "Subscriptions initialized, continuing")
         (chats-app--initialize :home-buffer home-buffer))
        ((not (map-elt (chats-app--state) :user-exists-checked))
         ;; Step 3: Check if we need to create a user
         (chats-app--log "Checking if user exists")
         (chats-app--send-admin-users-list-request
          :on-users-fetched (lambda (users)
                              (chats-app--log "Found %d users" (length users))
                              (if (seq-empty-p users)
                                  (chats-app--send-admin-add-user-request
                                   :on-user-added (lambda ()
                                                    (chats-app--log "User added successfully")
                                                    (map-put! (chats-app--state) :user-exists-checked t)
                                                    (chats-app--initialize :home-buffer home-buffer)))
                                (chats-app--log "User already exists")
                                (map-put! (chats-app--state) :user-exists-checked t)
                                (chats-app--initialize :home-buffer home-buffer)))))
        ((not (map-elt (chats-app--state) :status-checked))
         ;; Step 4: Check session status
         (chats-app--log "Sending status check request")
         (chats-app--set-status :type 'checking-session-status :message "Loading...")
         (acp-send-request :client (map-elt (chats-app--state) :client)
                           :request (chats-app--make-session-status-request
                                     :token chats-app-user-token)
                           :on-success (lambda (response)
                                         (chats-app--log "Status check response received: connected=%s logged-in=%s"
                                                         (map-elt response 'connected)
                                                         (map-elt response 'loggedIn))
                                         (map-put! (chats-app--state) :status-checked t)
                                         (cond
                                          ;; Fully connected and logged in - fetch chat index
                                          ((and (map-elt response 'connected)
                                                (map-elt response 'loggedIn))
                                           (map-put! (chats-app--state) :connected t)
                                           (chats-app--log "Already connected and logged in")
                                           (chats-app--fetch-chat-index))
                                          ;; Connected but not logged in - wait for Connected notification
                                          ((map-elt response 'connected)
                                           (map-put! (chats-app--state) :connected t)
                                           (chats-app--log "Connected but not logged in yet, waiting for Connected notification")
                                           (chats-app--set-status
                                            :type 'already-connected
                                            :message "Loading..."))
                                          ;; Not connected at all - proceed to connect
                                          (t
                                           (chats-app--log "Not connected, continuing to connect")
                                           (chats-app--initialize :home-buffer home-buffer))))
                           :on-failure (lambda (error)
                                         (map-put! (chats-app--state) :status-checked t)
                                         (chats-app--log "Status check failed: %s" (or (map-elt error 'message) "unknown"))
                                         (chats-app--initialize :home-buffer home-buffer))))
        ((not (map-elt (chats-app--state) :connected))
         ;; Step 5: Connect to WhatsApp (only if not already connected)
         (chats-app--log "Not connected, initiating connection")
         (chats-app--send-connect-request))))

(defun chats-app--send-connect-request ()
  (unless (derived-mode-p 'chats-app-mode)
    (error "Not in a chats buffer"))
  (chats-app--set-status :type 'connecting :message "Loading...")
  (acp-send-request :client (map-elt (chats-app--state) :client)
                    :request (chats-app--make-session-connect-request
                              :token chats-app-user-token
                              :immediate t
                              :subscribe chats-app--event-subscriptions)
                    :on-success (lambda (_response)
                                  (chats-app--log "Initial connection successful (waiting for notification)")
                                  (chats-app--set-status :type 'awaiting-connection :message "Loading..."))
                    :on-failure (lambda (error)
                                  ;; Already connected, don't display as
                                  ;; error since it can be ignored.
                                  (if (and (map-elt error 'message)
                                           (string-match-p "already connected" (map-elt error 'message)))
                                      (chats-app--log "Connect request failed: already connected (ignored)")
                                    (chats-app--log "Connect request failed: %s"
                                                    (map-elt error 'message))
                                    ;; Display real error
                                    (chats-app--set-status
                                     :type 'error
                                     :message (chats-app--refresh-error :message "Failed to connect"))))))

(cl-defun chats-app--send-disconnect-request (&key on-disconnected)
  (unless (derived-mode-p 'chats-app-mode)
    (error "Not in a chats buffer"))
  (chats-app--log "Requesting to disconnected")
  (acp-send-request :client (map-elt (chats-app--state) :client)
                    :request (chats-app--make-session-disconnect-request
                              :token chats-app-user-token)
                    :on-success (lambda (_response)
                                  (chats-app--log "Disconnect: success")
                                  (when on-disconnected
                                    (funcall on-disconnected))
                                  (chats-app--set-status :type nil :message nil))
                    :on-failure (lambda (error)
                                  (chats-app--log "Disconnect: failure %s" (map-elt error 'message))
                                  (chats-app--set-status
                                   :type 'error
                                   :message (chats-app--refresh-error :message "Something is not right")))))

(cl-defun chats-app--send-admin-users-list-request (&key on-users-fetched)
  (unless (derived-mode-p 'chats-app-mode)
    (error "Not in a chats buffer"))
  (unless on-users-fetched
    (error ":on-users-fetched is required"))
  (chats-app--set-status :type 'getting-details :message "Loading...")
  (acp-send-request :client (map-elt (chats-app--state) :client)
                    :request (chats-app--make-admin-users-list-request
                              :admin-token chats-app--admin-token)
                    :on-success (lambda (response)
                                  (funcall on-users-fetched response))
                    :on-failure (lambda (error)
                                  (chats-app--log "Couldn't load user: %s"
                                                  (or (map-elt error 'message)
                                                      "Don't know why"))
                                  (chats-app--set-status
                                   :type 'error
                                   :message (chats-app--refresh-error :message "Couldn't load user")))))

(cl-defun chats-app--send-admin-add-user-request (&key on-user-added)
  (unless (derived-mode-p 'chats-app-mode)
    (error "Not in a chats buffer"))
  (unless on-user-added
    (error ":on-user-added is required"))
  (chats-app--set-status :type 'adding-user :message "Initializing client...")
  (acp-send-request :client (map-elt (chats-app--state) :client)
                    :request (chats-app--make-admin-add-user-request
                              :admin-token chats-app--admin-token
                              :name (user-login-name)
                              :token chats-app-user-token
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
                                  (chats-app--log "Couldn't add local user: %s" (map-elt error 'message))
                                  (chats-app--set-status
                                   :type 'error
                                   :message (chats-app--refresh-error :message "Couldn't initialize user")))))

;; TODO: Reconsider naming and splitting into two separate requests "index" vs "jid".
(cl-defun chats-app--send-chat-history-request (&key chat-jid contact-name on-finished)
  "Fetch chat history for CHAT-JID and store in state.
CONTACT-NAME is the display name to use for the chat buffer.

When CHAT-JID is \"index\", stores normalized chat index as alist in :chats-index:
  ((\"chat-jid-1\" . chat-metadata-1)
   (\"chat-jid-2\" . chat-metadata-2) ...)

For a specific chat JID, stores message array in :chats and opens chat buffer."
  (unless (derived-mode-p 'chats-app-mode)
    (error "Not in a chats buffer"))
  (unless chat-jid
    (error ":chat-jid is required"))
  (acp-send-request :client (map-elt (chats-app--state) :client)
                    :request (chats-app--make-chat-history-request
                              :token chats-app-user-token
                              :chat-jid chat-jid)
                    :on-success (lambda (response)
                                  (cond
                                   ;; Handle "index" response: ((user-id . [chat-array]))
                                   ;; response: ((user-id . [((chat_jid . "...") (last_updated . "...")) ...]))
                                   ((and (listp response)
                                         (= (length response) 1)
                                         (consp (car response))
                                         (vectorp (cdar response)))
                                    (let ((chats-index (chats-app--parse-chat-index
                                                        (mapcar (lambda (p-chat)
                                                                  (cons (map-elt p-chat 'chat_jid) p-chat))
                                                                (append (cdar response) nil))
                                                        (map-elt (chats-app--state) :contacts)
                                                        (map-elt (chats-app--state) :groups))))
                                      (map-put! (chats-app--state) :chats-index chats-index)
                                      (chats-app--log "Loaded chat index: %d chats" (length chats-index))
                                      (chats-app--refresh)))
                                   ;; Handle specific chat response: [message-array]
                                   ;; Store as alist entry: (chat-jid . p-messages)
                                   (response
                                    (map-put! (chats-app--state) :chats
                                              (map-insert (or (map-elt (chats-app--state) :chats) '())
                                                          chat-jid response))
                                    ;; Parse protocol messages to internal format
                                    (let ((messages (chats-app-chat--parse-messages response
                                                                                    :chat-jid chat-jid
                                                                                    :contact-name contact-name
                                                                                    :contacts (map-elt (chats-app--state) :contacts)))
                                          ;; TODO: Consolidate buffer creation logic.
                                          (chat-buffer (get-buffer (format "*ChatsApp: %s*" (or contact-name chat-jid)))))
                                      (if chat-buffer
                                          ;; Buffer exists, just refresh it
                                          (progn
                                            (with-current-buffer chat-buffer
                                              (chats-app-chat--refresh messages))
                                            (switch-to-buffer chat-buffer))
                                        ;; Buffer doesn't exist, start new chat
                                        (chats-app-chat--start :chat-jid chat-jid
                                                               :messages messages
                                                               :contact-name contact-name))))
                                   ((and chat-jid (not (equal chat-jid "index")))
                                    ;; Not an "index" request.
                                    ;; Start new chat (no history).
                                    (chats-app-chat--start :chat-jid chat-jid
                                                           :messages nil ;; no history.
                                                           :contact-name contact-name)))
                                  (when on-finished
                                    (funcall on-finished)))
                    :on-failure (lambda (error)
                                  (message "Failed to fetch chat history: %s" (or (map-elt error 'message) "unknown")))))

(cl-defun chats-app--send-chat-send-text-request (&key phone body on-success on-failure)
  "Send a text message to PHONE with BODY.
Calls ON-SUCCESS when message is sent successfully.
Calls ON-FAILURE with error if sending fails."
  (unless (derived-mode-p 'chats-app-mode 'chats-app-chat-mode)
    (error "Not in a chats buffer"))
  (unless phone
    (error ":phone is required"))
  (unless body
    (error ":body is required"))
  (acp-send-request :client (map-elt (chats-app--state) :client)
                    :request (chats-app--make-chat-send-text-request
                              :token chats-app-user-token
                              :phone phone
                              :body body)
                    :on-success (or on-success
                                    (lambda (_response)
                                      (message "Message sent")))
                    :on-failure (or on-failure
                                    (lambda (error)
                                      (message "Failed to send message: %s" (or (map-elt error 'message) "unknown"))))))

(cl-defun chats-app--send-download-image-request (&key url direct-path media-key mimetype
                                                       file-enc-sha256 file-sha256 file-length
                                                       on-success on-failure)
  "Download and decrypt an image from WhatsApp servers.
Calls ON-SUCCESS with response containing decrypted image data.
Calls ON-FAILURE with error if download fails."
  (unless (derived-mode-p 'chats-app-mode 'chats-app-chat-mode)
    (error "Not in a chats buffer"))
  (unless url
    (error ":url is required"))
  (acp-send-request :client (map-elt (chats-app--state) :client)
                    :request (chats-app--make-download-image-request
                              :token chats-app-user-token
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

(cl-defun chats-app--send-download-video-request (&key url direct-path media-key mimetype
                                                       file-enc-sha256 file-sha256 file-length
                                                       on-success on-failure)
  "Download and decrypt a video from WhatsApp servers.
Calls ON-SUCCESS with response containing decrypted video data.
Calls ON-FAILURE with error if download fails."
  (unless (derived-mode-p 'chats-app-mode 'chats-app-chat-mode)
    (error "Not in a chats buffer"))
  (unless url
    (error ":url is required"))
  (acp-send-request :client (map-elt (chats-app--state) :client)
                    :request (chats-app--make-download-video-request
                              :token chats-app-user-token
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

(cl-defun chats-app--make-session-disconnect-request (&key token)
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

(defun chats-app--fetch-chat-index ()
  "Fetch contacts, groups, then chat index from WhatsApp."
  (chats-app--log "Fetching contacts first for chat index...")
  (chats-app--set-status :type 'fetching-contacts :message "Loading contacts...")
  (chats-app--fetch-contacts
   :on-success (lambda ()
                 (chats-app--log "Contacts fetched, now fetching groups...")
                 (chats-app--set-status :type 'fetching-groups :message "Loading groups...")
                 (chats-app--fetch-groups
                  :on-success (lambda ()
                                (chats-app--log "Groups fetched, now fetching chat index...")
                                (chats-app--set-status :type 'fetching-chats :message "Loading chats...")
                                (chats-app--send-chat-history-request
                                 :chat-jid "index"
                                 :on-finished (lambda ()
                                                (chats-app--set-status :type nil :message nil))))))))

(cl-defun chats-app--fetch-groups (&key on-success)
  "Fetch group list from WhatsApp.
Calls ON-SUCCESS after groups are fetched and parsed."
  (chats-app--log "Fetching groups...")
  (acp-send-request :client (map-elt (chats-app--state) :client)
                    :request (chats-app--make-group-list-request
                              :token chats-app-user-token)
                    :on-success (lambda (response)
                                  ;; response: ((Groups . [group-array]))
                                  (let* ((p-groups (if (and (listp response)
                                                            (= (length response) 1)
                                                            (consp (car response)))
                                                       (cdar response)
                                                     response))
                                         (groups (chats-app--parse-groups p-groups)))
                                    (map-put! (chats-app--state) :groups groups)
                                    (chats-app--log "Fetched %d groups" (length groups))
                                    (when on-success
                                      (funcall on-success))))
                    :on-failure (lambda (error)
                                  (chats-app--log "Failed to fetch groups: %s" (map-elt error 'message)))))

(cl-defun chats-app--fetch-contacts (&key on-success)
  "Fetch contacts from WhatsApp if not already successfully fetched.
Only marks as fetched if we get at least 1 contact, allowing retries
on fresh pairings where contacts may not be synced yet.
Calls ON-SUCCESS after contacts are fetched and parsed."
  (if (map-elt chats-app--state :contacts-fetched)
      (progn
        (chats-app--log "Contacts already fetched, skipping")
        (when on-success
          (funcall on-success)))
    (chats-app--log "Fetching contacts...")
    (chats-app--set-status :type 'fetching-contacts :message "Loading...")
    (acp-send-request :client (map-elt chats-app--state :client)
                      :request (chats-app--make-user-contacts-request
                                :token chats-app-user-token)
                      :on-success (lambda (response)
                                    (let* ((contacts (chats-app--parse-contacts response))
                                           (count (length contacts)))
                                      (map-put! (chats-app--state) :contacts contacts)
                                      (chats-app--log "Fetched %d contacts" count)
                                      (if (> count 0)
                                          (progn
                                            (map-put! (chats-app--state) :contacts-fetched t)
                                            (chats-app--set-status :type nil :message nil)
                                            (when on-success
                                              (funcall on-success)))
                                        (chats-app--log "No contacts synced yet, waiting for sync..."))))
                      :on-failure (lambda (error)
                                    (chats-app--log "Failed to fetch contacts: %s" (map-elt error 'message))
                                    (chats-app--set-status :type 'error
                                                           :message (chats-app--refresh-error :message "Failed to fetch contacts"))))))

(cl-defun chats-app--initialize-subscriptions ()
  "Initialize json-rpc subscriptions with SHELL.."
  (unless (map-elt chats-app--state :client)
    (error "Missing client"))
  (acp-subscribe-to-errors
   :client (map-elt chats-app--state :client)
   :on-error (lambda (error)
               (chats-app--log "Something is wrong %s" error)
               (chats-app--set-status
                :type 'error
                :message (chats-app--refresh-error :message "Something is not right"))))
  (acp-subscribe-to-notifications
   :client (map-elt chats-app--state :client)
   :on-notification (lambda (notification)
                      (chats-app--log "Notification: %s" (map-elt notification 'method))
                      (cond ((equal (map-elt notification 'method) "QR")
                             (when (map-nested-elt notification '(params qrCodeBase64))
                               (chats-app--display-qr-code (map-nested-elt notification '(params qrCodeBase64)))))
                            ((equal (map-elt notification 'method) "PairSuccess")
                             (chats-app--log "Pairing successful"))
                            ((equal (map-elt notification 'method) "Connected")
                             (map-put! (chats-app--state) :connected t)
                             (chats-app--log "Connected to WhatsApp"))
                            ((equal (map-elt notification 'method) "OfflineSyncCompleted")
                             (chats-app--log "Offline sync completed")
                             (when (and (not (map-elt (chats-app--state) :chats-index))
                                        (map-elt (chats-app--state) :connected))
                               (chats-app--fetch-chat-index)))
                            ((equal (map-elt notification 'method) "AppStateSyncComplete")
                             (chats-app--log "App state sync complete"))
                            ((equal (map-elt notification 'method) "ConnectFailure")
                             (map-put! (chats-app--state) :connected nil)
                             (chats-app--log "Couldn't connect: %s"
                                             (format "%s" (or (map-nested-elt notification '(params reason))
                                                              (map-nested-elt notification '(params error))
                                                              "???")))
                             (chats-app--set-status
                              :type 'error
                              :message (chats-app--refresh-error :message "Couldn't connect")))
                            ((equal (map-elt notification 'method) "Disconnected")
                             (map-put! (chats-app--state) :connected nil)
                             (chats-app--set-status
                              :type 'disconnected
                              :message (chats-app--refresh-error :message "Disconnected")))
                            ((equal (map-elt notification 'method) "Message")
                             (let* ((p-message (map-nested-elt notification '(params event Message)))
                                    (p-info (map-nested-elt notification '(params event Info)))
                                    (chat-jid (if (symbolp (map-elt p-info 'Chat))
                                                  (symbol-name (map-elt p-info 'Chat))
                                                (map-elt p-info 'Chat)))
                                    ;; Must capture contacts before with-current-buffer.
                                    (contacts (map-elt (chats-app--state) :contacts)))
                               (dolist (buffer (buffer-list))
                                 (with-current-buffer buffer
                                   (when-let ((message (and (derived-mode-p 'chats-app-chat-mode)
                                                            (equal (map-elt chats-app-chat--chat :chat-jid) chat-jid)
                                                            (chats-app-chat--parse-notification
                                                             :p-message p-message
                                                             :p-info p-info
                                                             :contact-name (map-elt chats-app-chat--chat :contact-name)
                                                             :chat-jid chat-jid
                                                             :contacts contacts))))
                                     (if (map-elt message :is-reaction)
                                         (chats-app-chat--add-reaction
                                          :target-id (map-elt message :target-id)
                                          :emoji (map-elt message :emoji)
                                          :sender (map-elt message :sender))
                                       (chats-app-chat--append-message message)))))))
                            ((equal (map-elt notification 'method) "HistorySync")
                             (chats-app--log "HistorySync received")
                             (chats-app--fetch-chat-index))))))

(defun chats-app--log (format-string &rest args)
  "Log a debug message to *ChatsApp-Log* buffer."
  (with-current-buffer (get-buffer-create "*ChatsApp-Log*")
    (goto-char (point-max))
    (insert (format-time-string "[%Y-%m-%d %H:%M:%S] "))
    (insert (apply #'format format-string args))
    (insert "\n")))

(cl-defun chats-app--refresh-error (&key message)
  "Return error MESSAGE with refresh instructions.
If refresh keybinding exists, appends \"\\n\\n<keybinding> to retry\".
Otherwise returns just the message.

MESSAGE should be a string like \"Failed to connect\"."
  (if-let ((key (car (where-is-internal 'chats-app-reload))))
      (concat message "\n\n"
              (propertize (key-description key) 'face 'help-key-binding)
              " to retry")
    message))

(defun chats-app--display-qr-code (base64-data)
  "Display a QR code from BASE64-DATA in the home buffer."
  (unless (derived-mode-p 'chats-app-mode)
    (error "Not in a chats buffer"))
  (let ((image (create-image (base64-decode-string
                              (replace-regexp-in-string
                               "^data:image/png;base64," "" base64-data))
                             'png t)))
    (chats-app--set-status :type 'qr
                           :message (concat
                                     (propertize
                                      ;; Pad image string with * so it can be centered in screen.
                                      (concat (make-string (round (car (image-size image))) ?*) "\n")
                                      'display image)
                                     "\nScan to enable this WhatsApp client"))))

(cl-defun chats-app--message (&key text)
  "Display centered TEXT centered in current buffer.

This function displayes TEXT only, wiping everything else."
  (interactive)
  (unless (derived-mode-p 'chats-app-mode)
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

(cl-defun chats-app--make-state (&key home-buffer)
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

(cl-defun chats-app--make-status (&key type message)
  "Create a status object with TYPE and optional MESSAGE.
TYPE can be: loading, connecting, connected, disconnected, error, etc.
MESSAGE is displayed if present."
  `((:type . ,type)
    (:message . ,message)))

(cl-defun chats-app--set-status (&key type message)
  "Set the current status and refresh the display."
  (chats-app--log "Status change: %s \"%s\"" type message)
  (unless (derived-mode-p 'chats-app-mode)
    (error "Not in a chats buffer"))
  (map-put! (chats-app--state) :status (chats-app--make-status :type type :message message))
  (chats-app--refresh))

(defun chats-app--timezone ()
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

(defun chats-app--make-action-keymap (action)
  "Create keymap with ACTION."
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] action)
    (define-key map (kbd "RET") action)
    (define-key map [remap self-insert-command] 'ignore)
    map))

(defun chats-app--add-action-to-text (text action &optional on-entered face)
  "Add ACTION lambda to propertized TEXT and return modified text.
ON-ENTERED is a function to call when the cursor enters the text.
FACE when non-nil applies the specified face to the text."
  (add-text-properties 0 (length text)
                       `(keymap ,(chats-app--make-action-keymap action))
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

(defvar chats-app-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'next-line)
    (define-key map (kbd "p") #'previous-line)
    (define-key map (kbd "q") #'chats-app-quit)
    (define-key map (kbd "g") #'chats-app-reload)
    map)
  "Keymap for `chats-app-mode'.")

(define-derived-mode chats-app-mode fundamental-mode "ChatsApp"
  "Major mode for chat interfaces.

\\{chats-app-mode-map}"
  (setq buffer-read-only t)
  (goto-char (point-max)))

(defun chats-app-quit ()
  (interactive)
  (unless (derived-mode-p 'chats-app-mode)
    (error "Not in a chats buffer"))
  (quit-restore-window (get-buffer-window (current-buffer)) 'kill))

(defun chats-app ()
  "Create or switch to the `*ChatsApp*` buffer in `chats-app-mode`."
  (interactive)
  (let ((home-buffer (get-buffer-create "*ChatsApp*")))
    (with-current-buffer home-buffer
      (unless (derived-mode-p 'chats-app-mode)
        (chats-app-mode))
      (add-hook 'kill-buffer-hook #'chats-app--clean-up nil t)
      (add-hook 'window-size-change-functions
                (lambda (_frame)
                  (with-current-buffer home-buffer
                    (when (map-nested-elt (chats-app--state) '(:status :message))
                      (chats-app--refresh)))) nil t)
      (add-hook 'window-configuration-change-hook
                (lambda ()
                  (with-current-buffer home-buffer
                    (when (map-nested-elt (chats-app--state) '(:status :message))
                      (chats-app--refresh)))) nil t)
      (chats-app--initialize :home-buffer home-buffer))
    (switch-to-buffer home-buffer)
    (chats-app--refresh)))

(defun chats-app--buffer ()
  "Get the `chats-app'.

Error if not found."
  (or (get-buffer "*ChatsApp*")
      (user-error "ChatsApp buffer not found (start with M-x chats-app)")))

(defun chats-app-reload ()
  (interactive)
  (unless (derived-mode-p 'chats-app-mode)
    (user-error "Not in a chats buffer"))
  (chats-app--log "Refresh requested, resetting state and restarting...")
  (let ((home-buffer (current-buffer)))
    (when (map-elt (chats-app--state) :client)
      (acp-shutdown :client (map-elt (chats-app--state) :client)))
    (setq chats-app--state nil)
    (chats-app--initialize :home-buffer home-buffer)))

(defun chats-app-new-chat ()
  "Select a contact or group using completing-read and open the chat."
  (interactive)
  (with-current-buffer (chats-app--buffer)
    (let ((contacts (map-elt (chats-app--state) :contacts))
          (groups (map-elt (chats-app--state) :groups)))
      (unless (or contacts groups)
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
                      contacts))
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
                      groups))
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
                  (selected-jid (cdr (assoc selected-label candidates)))
                  (selected-entry (seq-find (lambda (entry)
                                              (string= (map-elt entry :jid) selected-jid))
                                            all-entries)))
            (chats-app--send-chat-history-request
             :chat-jid selected-jid
             :contact-name (concat (map-elt selected-entry :display-name)
                                   (when (map-elt selected-entry :is-group)
                                     " (group)")))
          (user-error "No contact or group found"))))))

(defalias 'chats-app-new-message #'chats-app-new-chat)

(defun chats-app--refresh ()
  "Refresh the display based on current status."
  (save-excursion
    (let* ((status (map-elt (chats-app--state) :status))
           (message (when status (map-elt status :message)))
           (chats-index (map-elt (chats-app--state) :chats-index)))
      (cond
       ;; If we have chats and no blocking status message, display chat list
       ((and chats-index (not message))
        (let* (;; chats-index is already parsed and sorted, just need to format for display
               (max-name-width (apply #'max
                                      (mapcar (lambda (chat) (string-width (map-elt chat :display-name)))
                                              chats-index)))
               ;; Format with aligned columns and add actions
               (chat-lines
                (mapcar
                 (lambda (chat)
                   (let* ((name-width (string-width (map-elt chat :display-name)))
                          (padding (make-string (- max-name-width name-width) ?\s))
                          (line (format "%s%s"
                                        (map-elt chat :display-name)
                                        padding)))
                     (chats-app--add-action-to-text
                      line
                      (lambda ()
                        (interactive)
                        (chats-app--send-chat-history-request
                         :chat-jid (map-elt chat :chat-jid)
                         :contact-name (map-elt chat :display-name))))))
                 chats-index)))
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert (mapconcat #'identity chat-lines "\n")))))
       ;; Otherwise show status message
       (message
        (chats-app--message :text message))))))

(defun chats-app--state ()
  "Get shell state or fail in an incompatible buffer."
  (unless (derived-mode-p 'chats-app-mode)
    (error "No access outside chats-app-mode: %s" major-mode))
  (unless chats-app--state
    (error "No chats-app-mode state available"))
  chats-app--state)

(defun chats-app--clean-up ()
  "Clean up resources.

For example, shut down wuzapi process."
  (unless (derived-mode-p 'chats-app-mode)
    (error "Not in a chats buffer"))
  (when (map-elt (chats-app--state) :client)
    (acp-shutdown :client (map-elt (chats-app--state) :client))))

;; Protocol parsing functions

(defun chats-app--parse-contact (p-contact)
  "Parse protocol contact to internal format.
P-CONTACT is an alist from user.contacts response.
Returns alist with :full-name and :push-name."
  (let ((full-name (map-elt p-contact 'FullName))
        (push-name (map-elt p-contact 'PushName)))
    `((:full-name . ,full-name)
      (:push-name . ,push-name))))

(defun chats-app--parse-contacts (p-contacts)
  "Parse contacts response to alist of (jid . contact-info).
P-CONTACTS is the response from user.contacts.
Returns alist: ((jid1 . contact1) (jid2 . contact2) ...)"
  (mapcar (lambda (p-contact-entry)
            (let ((jid (car p-contact-entry))
                  (p-contact-data (cdr p-contact-entry)))
              (cons jid (chats-app--parse-contact p-contact-data))))
          p-contacts))

(defun chats-app--parse-group (p-group)
  "Parse protocol group to internal format.
P-GROUP is an alist from group.list response.
Returns alist with :name."
  `((:name . ,(map-elt p-group 'Name))))

(defun chats-app--parse-groups (p-groups)
  "Parse groups response to alist of (jid . group-info).
P-GROUPS is the response from group.list (vector or list of groups).
Returns alist: ((jid1 . group1) (jid2 . group2) ...)"
  (mapcar (lambda (p-group)
            ;; TODO: Do we need symbols here? Why not keep as string?
            (let ((jid (intern (map-elt p-group 'JID))))
              (cons jid (chats-app--parse-group p-group))))
          (append p-groups nil)))

(defun chats-app--parse-chat-index-entry (p-chat-entry contacts groups)
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
                   (when-let ((group (assoc (intern chat-jid) groups)))
                     (map-elt (cdr group) :name))
                 ;; For contacts, look up in contacts list
                 (when-let ((contact (assoc (intern chat-jid) contacts)))
                   (or (map-elt (cdr contact) :full-name)
                       (map-elt (cdr contact) :push-name)))))
         (identifier (when (string-match "\\([^@]+\\)@" chat-jid)
                       (match-string 1 chat-jid)))
         (display-name (or name identifier chat-jid)))
    `((:chat-jid . ,chat-jid)
      (:display-name . ,display-name)
      (:is-group . ,is-group)
      (:last-updated . ,last-updated))))

(defun chats-app--parse-chat-index (p-chat-index contacts groups)
  "Parse chat index response to list of internal chat entries.
P-CHAT-INDEX is the raw response from chat.history with chat_jid='index'.
CONTACTS is internal contacts alist (already parsed).
GROUPS is internal groups alist (already parsed).
Returns list of internal chat entry alists, filtered and sorted."
  (let* ((parsed (delq nil
                       (mapcar (lambda (p-entry)
                                 ;; Filter out status broadcasts
                                 (unless (string= (car p-entry) "status@broadcast")
                                   (chats-app--parse-chat-index-entry p-entry contacts groups)))
                               p-chat-index)))
         (sorted (sort parsed
                       (lambda (a b)
                         (string> (or (map-elt a :last-updated) "")
                                  (or (map-elt b :last-updated) ""))))))
    sorted))

;; Protocol request builders

(cl-defun chats-app--make-session-connect-request (&key token subscribe immediate)
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

(cl-defun chats-app--make-user-contacts-request (&key token)
  "Instantiate a \"user.contacts\" request to get all contacts."
  (unless token (error ":token is required"))
  `((:method . "user.contacts")
    (:params . ((token . ,token)))))

(cl-defun chats-app--make-group-list-request (&key token)
  "Instantiate a \"group.list\" request to get all groups."
  (unless token (error ":token is required"))
  `((:method . "group.list")
    (:params . ((token . ,token)))))

(cl-defun chats-app--make-chat-history-request (&key token chat-jid)
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

(cl-defun chats-app--make-chat-clear-request (&key token chat-jid)
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

(cl-defun chats-app--make-admin-users-list-request (&key admin-token)
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

(cl-defun chats-app--make-admin-add-user-request (&key admin-token
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

(cl-defun chats-app--make-session-status-request (&key token)
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

(cl-defun chats-app--make-chat-send-text-request (&key token
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

(cl-defun chats-app--make-download-image-request (&key token
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

(cl-defun chats-app--make-download-video-request (&key token
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
