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
(require 'wasabi-icon)

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

(defcustom wasabi-header-style (if (display-graphic-p) 'graphical 'text)
  "Style for wasabi buffer headers.

Can be one of:

 \='graphical: Display header with icon and styled text.
 \='text: Display simple text-only header."
  :type '(choice (const :tag "Graphical" graphical)
                 (const :tag "Text only" text))
  :group 'wasabi)

(defcustom wasabi-video-player-function nil
  "Function to open video files for viewing.

If nil, uses the default system player via `wasabi-chat--open-video-externally'.
If set to a function, it will be called with one argument: the absolute path
to the video file.

Example:
  (setq wasabi-video-player-function
        (lambda (file)
          (start-process \"mpv\" nil \"mpv\" file)))"
  :type '(choice (const :tag "Use system default" nil)
                 (function :tag "Custom function"))
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

(cl-defun wasabi--initialize (&key wasabi-buffer status-type status-message)
  "Initialize wasabi client and progress through startup sequence.

Requires the WASABI-BUFFER.

Optional STATUS-TYPE and STATUS-MESSAGE are used for progressing through init.

For silent progression, set :silent-refresh in state before calling."
  (unless wasabi-buffer
    (error ":wasabi-buffer is required"))

  (unless wasabi--state
    (setq wasabi--state (wasabi--make-state :wasabi-buffer wasabi-buffer)))

  (wasabi--log "wasabi--initialize (wasabi-buffer: %s)(status-type: %s)"
               wasabi-buffer
               (map-nested-elt (wasabi--state) '(:status :type)))

  ;; Set status if provided
  (when status-type
    (wasabi--set-status :type status-type
                        :message status-message
                        :silent (map-elt (wasabi--state) :silent-refresh)))

  ;; Silent flag no longer necessary after ready. Clear it.
  (when (eq status-type 'ready)
    (map-delete (wasabi--state) :silent-refresh))

  (cond
   ;; Step 1: Create client
   ((not (map-elt (wasabi--state) :client))
    (map-put! (wasabi--state)
              :client (acp-make-client :command (car wasabi-wuzapi-command)
                                       :command-params (cdr wasabi-wuzapi-command)
                                       :environment-variables (list (concat "WUZAPI_ADMIN_TOKEN=" wasabi--admin-token)
                                                                    (concat "TZ=" (wasabi--timezone)))
                                       :context-buffer wasabi-buffer))
    (wasabi--initialize-subscriptions)
    (wasabi--initialize :wasabi-buffer wasabi-buffer
                        :status-type 'check-user
                        :status-message (wasabi--make-loading-message)))
   ;; Step 2: Check if user exists
   ((eq (map-nested-elt (wasabi--state) '(:status :type))
        'check-user)
    (wasabi--set-status :type 'checking-user
                        :message (wasabi--make-loading-message))
    (acp-send-request :client (map-elt (wasabi--state) :client)
                      :request (wasabi--make-admin-users-list-request
                                :admin-token wasabi--admin-token)
                      :on-success (lambda (users)
                                    (wasabi--log "Found %d users" (length users))
                                    (if (seq-empty-p users)
                                        ;; Need to add user
                                        (wasabi--initialize :wasabi-buffer wasabi-buffer
                                                            :status-type 'add-user
                                                            :status-message (wasabi--make-loading-message))
                                      ;; User exists, continue
                                      (wasabi--initialize :wasabi-buffer wasabi-buffer
                                                          :status-type 'check-session-status
                                                          :status-message (wasabi--make-loading-message))))
                      :on-failure (lambda (error)
                                    (wasabi--log "Couldn't load user: %s"
                                                 (or (map-elt error 'message) "unknown"))
                                    (wasabi--set-status :type 'error
                                                        :message (wasabi--refresh-error :message "Couldn't load user")))))
   ;; Step 3: Add user if needed
   ((eq (map-nested-elt (wasabi--state) '(:status :type))
        'add-user)
    (acp-send-request :client (map-elt (wasabi--state) :client)
                      :request (wasabi--make-admin-add-user-request
                                :admin-token wasabi--admin-token
                                :name (user-login-name)
                                :token wasabi-user-token
                                :events wasabi--event-subscriptions
                                :history 100)
                      :on-success (lambda (_response)
                                    (wasabi--log "User added successfully")
                                    (wasabi--initialize :wasabi-buffer wasabi-buffer
                                                        :status-type 'check-session-status
                                                        :status-message (wasabi--make-loading-message)))
                      :on-failure (lambda (error)
                                    (wasabi--log "Couldn't add local user: %s" (map-elt error 'message))
                                    (wasabi--set-status :type 'error
                                                        :message (wasabi--refresh-error :message "Couldn't initialize user")))))
   ;; Step 4: Check session status
   ((eq (map-nested-elt (wasabi--state) '(:status :type))
        'check-session-status)
    (acp-send-request :client (map-elt (wasabi--state) :client)
                      :request (wasabi--make-session-status-request
                                :token wasabi-user-token)
                      :on-success (lambda (response)
                                    (wasabi--log "Status check response: connected=%s logged-in=%s"
                                                 (map-elt response 'connected)
                                                 (map-elt response 'loggedIn))
                                    (cond
                                     ;; Already connected and logged in - fetch data
                                     ((and (map-elt response 'connected)
                                           (map-elt response 'loggedIn))
                                      (map-put! (wasabi--state) :connected t)
                                      (wasabi--log "Already connected and logged in")
                                      (wasabi--initialize :wasabi-buffer wasabi-buffer
                                                          :status-type 'fetch-contacts
                                                          :status-message (wasabi--make-loading-message)))
                                     ;; Connected but not logged in - wait for notification
                                     ((map-elt response 'connected)
                                      (map-put! (wasabi--state) :connected t)
                                      (wasabi--log "Connected but not logged in, waiting for Connected notification")
                                      (wasabi--set-status :type 'already-connected
                                                          :message (wasabi--make-loading-message)))
                                     ;; Not connected - initiate connection
                                     (t
                                      (wasabi--log "Not connected, initiating connection")
                                      (wasabi--initialize :wasabi-buffer wasabi-buffer
                                                          :status-type 'connect-session
                                                          :status-message (wasabi--make-loading-message)))))
                      :on-failure (lambda (error)
                                    (wasabi--log "Status check failed: %s" (or (map-elt error 'message) "unknown"))
                                    (wasabi--set-status :type 'error
                                                        :message (wasabi--refresh-error :message "Status check failed")))))
   ;; Step 5: Connect to WhatsApp
   ((eq (map-nested-elt (wasabi--state) '(:status :type))
        'connect-session)
    (acp-send-request :client (map-elt (wasabi--state) :client)
                      :request (wasabi--make-session-connect-request
                                :token wasabi-user-token
                                :immediate t
                                :subscribe wasabi--event-subscriptions)
                      :on-success (lambda (_response)
                                    (wasabi--log "Connection request successful, awaiting notification")
                                    (wasabi--set-status :type 'awaiting-connection
                                                        :message (wasabi--make-loading-message)))
                      :on-failure (lambda (error)
                                    (if (and (map-elt error 'message)
                                             (string-match-p "already connected" (map-elt error 'message)))
                                        (wasabi--log "Already connected (ignored)")
                                      (wasabi--log "Connect failed: %s" (map-elt error 'message))
                                      (wasabi--set-status :type 'error
                                                          :message (wasabi--refresh-error :message "Failed to connect"))))))
   ;; Step 6: Fetch contacts
   ((eq (map-nested-elt (wasabi--state) '(:status :type))
        'fetch-contacts)
    (wasabi--log "Fetching contacts...")
    (acp-send-request :client (map-elt (wasabi--state) :client)
                      :request (wasabi--make-user-contacts-request
                                :token wasabi-user-token)
                      ;; Response is an array of contacts
                      :on-success (lambda (p-contacts)
                                    (let* ((contacts (wasabi--parse-contacts p-contacts)))
                                      ;; For debugging:
                                      ;; (when (seq-first p-contacts)
                                      ;;   (wasabi--log "Sample RAW contact: %s" (seq-first p-contacts)))
                                      (map-put! (wasabi--state) :contacts contacts)
                                      (wasabi--log "Fetched %d contacts" (length contacts))
                                      ;; For debugging:
                                      ;; (when (seq-first contacts)
                                      ;;   (wasabi--log "Sample PARSED contact: %s" (seq-first contacts)))
                                      (when (= (length contacts) 0)
                                        (wasabi--log "No contacts from backend (expected on fresh pairing)"))
                                      ;; Continue to fetch groups regardless of contact count.
                                      ;; WhatsApp Web may not provide contacts on fresh pairing.
                                      (wasabi--initialize :wasabi-buffer wasabi-buffer
                                                          :status-type 'fetch-groups
                                                          :status-message
                                                          (wasabi--make-loading-message))))
                      :on-failure (lambda (error)
                                    (wasabi--log "Failed to fetch contacts: %s" (map-elt error 'message))
                                    (wasabi--set-status :type 'error
                                                        :message (wasabi--refresh-error :message "Failed to fetch contacts")))))
   ;; Step 7: Fetch groups
   ((eq (map-nested-elt (wasabi--state) '(:status :type))
        'fetch-groups)
    (wasabi--log "Fetching groups...")
    (acp-send-request :client (map-elt (wasabi--state) :client)
                      :request (wasabi--make-group-list-request
                                :token wasabi-user-token)
                      :on-success (lambda (response)
                                    ;; Response format: {"Groups": [...]} from backend
                                    ;; Extract the Groups array:
                                    ;;
                                    ;; '((Groups . [((JID . "123@g.us") (Name . "Family"))
                                    ;;              ((JID . "456@g.us") (Name . "Work"))]))
                                    ;;
                                    ;; => [((JID . "123@g.us") (Name . "Family"))
                                    ;;     ((JID . "456@g.us") (Name . "Work"))]
                                    (let* ((p-groups (map-elt response 'Groups))
                                           (groups (wasabi--parse-groups p-groups)))
                                      (map-put! (wasabi--state) :groups groups)
                                      (wasabi--log "Fetched %d groups" (length groups))
                                      (wasabi--initialize :wasabi-buffer wasabi-buffer
                                                          :status-type 'fetch-chats
                                                          :status-message (wasabi--make-loading-message))))
                      :on-failure (lambda (error)
                                    (wasabi--log "Failed to fetch groups: %s" (map-elt error 'message))
                                    (wasabi--set-status :type 'error
                                                        :message (wasabi--refresh-error :message "Failed to fetch groups")))))

   ;; Step 8: Fetch chat index
   ((eq (map-nested-elt (wasabi--state) '(:status :type))
        'fetch-chats)
    (wasabi--log "Fetching chat index...")
    (wasabi--send-chat-history-request
     :chat-jid "index"
     :on-finished (lambda ()
                    (wasabi--log "Chat index loaded")
                    (wasabi--set-status :type 'ready :message nil))))
   ;; Expected statuse changes (nothing to do)
   ((memq status-type '(awaiting-connection qr paired already-connected ready error disconnected))
    nil)))

(defun wasabi--make-icon-message (message)
  "Make MESSAGE with icon."
  (concat
   (wasabi-icon (* 2 (wasabi--face-height-pixels 'font-lock-doc-face)))
   "\n\n"
   message))

(defun wasabi--header-graphical-p ()
  "Return non-nil if header should show graphics."
  (and (display-graphic-p)
       (eq wasabi-header-style 'graphical)))

(defun wasabi--make-loading-message ()
  "Make \"Loading...\" message."
  (if (wasabi--header-graphical-p)
      ;; Compensate with prefixed whitespace
      ;; for visual centering as the ...
      ;; makes it look off center compared
      ;; to the icon.
      (wasabi--make-icon-message "    Loading...")
    "Loading..."))

(cl-defun wasabi--send-disconnect-request (&key on-disconnected)
  "Send disconnect request.

Invokes ON-DISCONNECTED (lambda ()) on success."
  (unless (derived-mode-p 'wasabi-mode)
    (error "Not in a chats buffer"))
  (wasabi--log "Requesting to disconnected")
  (acp-send-request :client (map-elt (wasabi--state) :client)
                    :request (wasabi--make-session-disconnect-request
                              :token wasabi-user-token)
                    :on-success (lambda (_response)
                                  (wasabi--log "Disconnect: success")
                                  (if on-disconnected
                                      ;; Let callback handle status
                                      (funcall on-disconnected)
                                    ;; No callback, set disconnected status
                                    (wasabi--set-status :type 'disconnected :message "Disconnected")))
                    :on-failure (lambda (error)
                                  (wasabi--log "Disconnect: failure %s" (map-elt error 'message))
                                  (wasabi--set-status
                                   :type 'error
                                   :message (wasabi--refresh-error :message "Something is not right")))))



;; TODO: Reconsider naming and splitting into two separate requests "index" vs "jid".
(cl-defun wasabi--send-chat-history-request (&key chat-jid contact-name on-finished)
  "Fetch chat history for CHAT-JID and store in state.
CONTACT-NAME is the display name to use for the chat buffer.

When CHAT-JID is \"index\", stores normalized chat index
as alist in :chats-index:
  ((\"chat-jid-1\" . chat-metadata-1)
   (\"chat-jid-2\" . chat-metadata-2) ...)

For a specific chat JID, stores message array in :chats and opens chat buffer.

Invoke ON-FINISHED on success."
  (unless (derived-mode-p 'wasabi-mode)
    (error "Not in a chats buffer"))
  (unless chat-jid
    (error ":chat-jid is required"))
  (acp-send-request :client (map-elt (wasabi--state) :client)
                    :request (wasabi--make-chat-history-request
                              :token wasabi-user-token
                              :chat-jid chat-jid)
                    :on-success (lambda (response)
                                  (wasabi--log "Chat history response for %s: type=%s length=%s value=%S"
                                               chat-jid
                                               (type-of response)
                                               (if (listp response) (length response) "N/A")
                                               response)
                                  (cond
                                   ;; Handle "index" response: {"user-id": [...]} from backend
                                   ;; Backend returns map of user-id to chat arrays
                                   ;; Merge all users' chats (typically only one user)
                                   ((and (equal chat-jid "index") response)
                                    (let* (;; Join all the chats into a single list.
                                           ;;
                                           ;;'((user-123 . [((chat_jid . "123") (last_updated . "2025-11-19"))
                                           ;;               ((chat_jid . "456") (last_updated . "2025-11-18"))])
                                           ;;  (user-456 . [((chat_jid . "789") (last_updated . "2025-11-19"))
                                           ;;               ((chat_jid . "101") (last_updated . "2025-11-18"))]))
                                           ;;
                                           ;; =>
                                           ;;
                                           ;; '(((chat_jid . "123") (last_updated . "2025-11-19"))
                                           ;;   ((chat_jid . "456") (last_updated . "2025-11-18"))
                                           ;;   ((chat_jid . "789") (last_updated . "2025-11-19"))
                                           ;;   ((chat_jid . "101") (last_updated . "2025-11-18")))
                                           (p-chats (apply #'append (mapcar (lambda (v) (append v nil)) (map-values response))))
                                           (chats-index (wasabi--parse-chat-index
                                                         (mapcar (lambda (p-chat)
                                                                   (cons (map-elt p-chat 'chat_jid) p-chat))
                                                                 p-chats)
                                                         (map-elt (wasabi--state) :contacts)
                                                         (map-elt (wasabi--state) :groups))))
                                      (wasabi--log "Raw chats from response: %d" (length p-chats))
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
                                  (wasabi--log "Failed to fetch chat history for %s: %s" chat-jid (or (map-elt error 'message) "unknown"))
                                  (message "Failed to fetch chat history"))))

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
  "Download URL with DIRECT-PATH and decrypt an image from WhatsApp servers.

DIRECT-PATH - WhatsApp direct path to encrypted file
MEDIA-KEY - Base64 encryption key for decrypting the image
MIMETYPE - Image MIME type (e.g., \"image/jpeg\")
FILE-ENC-SHA256 - SHA256 hash of encrypted file
FILE-SHA256 - SHA256 hash of decrypted file
FILE-LENGTH - File size in bytes

Calls ON-SUCCESS with response containing decrypted image data.
Calls ON-FAILURE with error if download fails."
  (unless (derived-mode-p 'wasabi-mode 'wasabi-chat-mode)
    (error "Not in a chats buffer"))
  (unless url
    (error ":url is required"))
  (wasabi--log "Downloading image...")
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
                                      (wasabi--log "Image downloaded")))
                    :on-failure (or on-failure
                                    (lambda (error)
                                      (wasabi--log "Failed to download image: %s"
                                                   (or (map-elt error 'message) "unknown"))))))

(cl-defun wasabi--send-download-video-request (&key url direct-path media-key mimetype
                                                    file-enc-sha256 file-sha256 file-length
                                                    on-success on-failure)
  "Download URL and decrypt a video from WhatsApp servers.

DIRECT-PATH - WhatsApp direct path to encrypted file
MEDIA-KEY - Base64 encryption key for decrypting the image
MIMETYPE - Image MIME type (e.g., \"image/jpeg\")
FILE-ENC-SHA256 - SHA256 hash of encrypted file
FILE-SHA256 - SHA256 hash of decrypted file
FILE-LENGTH - File size in bytes

Calls ON-SUCCESS with response containing decrypted video data.
Calls ON-FAILURE with error if download fails."
  (unless (derived-mode-p 'wasabi-mode 'wasabi-chat-mode)
    (error "Not in a chats buffer"))
  (unless url
    (error ":url is required"))
  (wasabi--log "Downloading video")
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
                                      (wasabi--log "Video downloaded")))
                    :on-failure (or on-failure
                                    (lambda (error)
                                      (wasabi--log "Failed to download video: %s"
                                                   (or (map-elt error 'message) "unknown"))))))

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
                             (wasabi--log "Pairing successful")
                             (wasabi--initialize :wasabi-buffer (map-elt (wasabi--state) :wasabi-buffer)
                                                 :status-type 'paired
                                                 :status-message (wasabi--make-loading-message)))
                            ((equal (map-elt notification 'method) "Connected")
                             (map-put! (wasabi--state) :connected t)
                             (wasabi--log "Connected to WhatsApp")
                             ;; Resume initialization after connection
                             (let ((status-type (map-nested-elt (wasabi--state) '(:status :type))))
                               (when (memq status-type '(awaiting-connection already-connected qr paired))
                                 (wasabi--log "Resuming initialization after connection")
                                 (wasabi--initialize :wasabi-buffer (map-elt (wasabi--state) :wasabi-buffer)
                                                     :status-type 'fetch-contacts
                                                     :status-message (wasabi--make-loading-message)))))
                            ((equal (map-elt notification 'method) "LoggedOut")
                             (map-put! (wasabi--state) :connected nil)
                             (wasabi--log "Logged out, disconnecting and reconnecting for QR code")
                             ;; Disconnect first, then reconnect to get QR code
                             (wasabi--send-disconnect-request
                              :on-disconnected (lambda ()
                                                 (wasabi--log "Disconnected, now reconnecting")
                                                 (let ((wasabi-buffer (map-elt (wasabi--state) :wasabi-buffer)))
                                                   (wasabi--initialize :wasabi-buffer wasabi-buffer
                                                                       :status-type 'connect-session
                                                                       :status-message (wasabi--make-loading-message))))))
                            ((equal (map-elt notification 'method) "OfflineSyncCompleted")
                             (wasabi--log "Offline sync completed")
                             (let ((status-type (map-nested-elt (wasabi--state) '(:status :type))))
                               ;; If we're ready, re-fetch all data since WhatsApp just synced
                               (when (eq status-type 'ready)
                                 (wasabi--log "Refreshing all data after OfflineSyncCompleted")
                                 ;; Re-run the fetch sequence to pick up synced data
                                 ;; Silent mode: no visual status updates, only refresh at the end
                                 (map-put! (wasabi--state) :silent-refresh t)
                                 (wasabi--initialize :wasabi-buffer (map-elt (wasabi--state) :wasabi-buffer)
                                                     :status-type 'fetch-contacts
                                                     :status-message (wasabi--make-loading-message)))))
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
                               ;; Trigger re-fetching index to show recent
                               ;; chats and groups with latest order.
                               (wasabi--send-chat-history-request :chat-jid "index")
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
                             (wasabi--log "HistorySync: current-buffer=%s, major-mode=%s" (current-buffer) major-mode)
                             ;; If we're ready, re-fetch all data since WhatsApp just synced history
                             (let ((status-type (map-nested-elt (wasabi--state) '(:status :type))))
                               (wasabi--log "HistorySync: status-type=%s, ready?=%s" status-type (eq status-type 'ready))
                               (when (eq status-type 'ready)
                                 (wasabi--log "Refreshing all data after HistorySync")
                                 ;; Re-run the fetch sequence to pick up synced data.
                                 ;; Starting from fetch-contacts triggers the chain:
                                 ;; fetch-contacts -> fetch-groups -> fetch-chats -> ready
                                 ;; Silent mode: no visual status updates, only refresh at the end
                                 (map-put! (wasabi--state) :silent-refresh t)
                                 (wasabi--initialize :wasabi-buffer (map-elt (wasabi--state) :wasabi-buffer)
                                                     :status-type 'fetch-contacts
                                                     :status-message (wasabi--make-loading-message)))))))))

(defun wasabi--log (format-string &rest args)
  "Log a debug message to *Wasabi-Log* buffer.

FORMAT-STRING and ARGS like `message'."
  (with-current-buffer (get-buffer-create "*Wasabi-Log*")
    (goto-char (point-max))
    (insert (format-time-string "[%Y-%m-%d %H:%M:%S] "))
    (insert (apply #'format format-string args))
    (insert "\n")))

(cl-defun wasabi--refresh-error (&key message)
  "Return error MESSAGE with refresh instructions.
If refresh keybinding exists, appends \"\\n\\n<keybinding> to reload\".
Otherwise returns just the message.

MESSAGE should be a string like \"Failed to connect\"."
  (if-let ((key (car (where-is-internal 'wasabi-reload))))
      (concat message "\n\n"
              (propertize (key-description key) 'face 'help-key-binding)
              " to reload")
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

(cl-defun wasabi--make-state (&key wasabi-buffer)
  "Construct chat client state with WASABI-BUFFER.

State uses :status to track initialization progress (see `wasabi--make-status').
The :connected flag tracks WhatsApp connection state (updated by notifications)."
  (unless wasabi-buffer
    (error ":wasabi-buffer is required"))
  (list (cons :client nil)
        (cons :wasabi-buffer wasabi-buffer)
        (cons :status nil)
        ;; :connected tracks async WhatsApp connection state (set by notifications)
        (cons :connected nil)
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

TYPE can be one of:
  Initialization: loading, checking-user, checking-session-status,
  adding-user, connecting, awaiting-connection
  Authentication: qr, already-connected
  Data Loading: fetching-contacts, fetching-groups, fetching-chats
  Terminal: ready, error, disconnected

MESSAGE is displayed if present.  When TYPE is ready, the chat list is rendered."
  `((:type . ,type)
    (:message . ,message)))

(cl-defun wasabi--set-status (&key type message silent)
  "Set the current status TYPE and MESSAGE and refresh the display.

Optional SILENT suppresses visual messaging during status change."
  (wasabi--log "Status change: %s \"%s\"" type message)
  (unless (derived-mode-p 'wasabi-mode)
    (error "Not in a chats buffer"))
  (map-put! (wasabi--state) :status (wasabi--make-status :type type :message message))
  (if (eq type 'ready)
      (wasabi--update-header-line)
    (setq header-line-format nil))
  (when (or (eq type 'ready) (not silent))
    (wasabi--refresh)))

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
                    (:command wasabi-reload :description "reload")
                    (:command wasabi-quit :description "quit"))))
    (setq header-line-format
          (concat
           (when (wasabi--header-graphical-p)
             (concat
              " "
              (wasabi-icon (wasabi--face-height-pixels 'font-lock-doc-face))))
           " "
           (propertize "Recent Chats" 'face 'font-lock-doc-face)
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
  "Quit `wasabi' and disconnect from WhatsApp."
  (interactive)
  (unless (derived-mode-p 'wasabi-mode)
    (error "Not in a chats buffer"))
  (quit-restore-window (get-buffer-window (current-buffer)) 'kill))

;;;###autoload
(defun wasabi ()
  "Create or switch to the `*Wasabi*` buffer in `wasabi-mode`."
  (interactive)
  (let ((wasabi-buffer (get-buffer-create "*Wasabi*")))
    (with-current-buffer wasabi-buffer
      (unless (derived-mode-p 'wasabi-mode)
        (wasabi-mode))
      (add-hook 'kill-buffer-hook #'wasabi--clean-up nil t)
      (add-hook 'window-size-change-functions
                (lambda (_frame)
                  (with-current-buffer wasabi-buffer
                    (when (map-nested-elt (wasabi--state) '(:status :message))
                      (wasabi--refresh)))) nil t)
      (add-hook 'window-configuration-change-hook
                (lambda ()
                  (with-current-buffer wasabi-buffer
                    (when (map-nested-elt (wasabi--state) '(:status :message))
                      (wasabi--refresh)))) nil t)
      (wasabi--initialize :wasabi-buffer wasabi-buffer))
    (switch-to-buffer wasabi-buffer)
    (wasabi--refresh)))

(defun wasabi--buffer ()
  "Get the `wasabi'.

Error if not found."
  (or (get-buffer "*Wasabi*")
      (user-error "Wasabi buffer not found (start with M-x wasabi)")))

(defun wasabi-reload ()
  "Reload `wassabi' buffer."
  (interactive)
  (unless (derived-mode-p 'wasabi-mode)
    (user-error "Not in a chats buffer"))
  (wasabi--log "Refresh requested, resetting state and restarting...")
  (let ((wasabi-buffer (current-buffer)))
    (when (map-elt (wasabi--state) :client)
      (acp-shutdown :client (map-elt (wasabi--state) :client)))
    (setq wasabi--state nil)
    (wasabi--log "==== new session ====")
    (wasabi--initialize :wasabi-buffer wasabi-buffer)))

(defun wasabi-new-chat-new-number ()
  "Start a new chat with a new phone number."
  (interactive)
  (let ((current-prefix-arg t))
    (call-interactively #'wasabi-new-chat)))

(defun wasabi-new-chat (new-number)
  "Select a contact or group and open a new chat.

With prefix argument NEW-NUMBER, prompt for a phone number."
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

(defun wasabi--group-chats-by-date (chats-index)
  "Group CHATS-INDEX by date labels (Today, Yesterday, or date).
Returns list of (date-label . chats-for-that-date)."
  (let ((date-groups '())
        (today (decode-time))
        (yesterday (decode-time (time-subtract nil (* 24 60 60)))))
    (dolist (chat chats-index)
      (let* ((timestamp (when (map-elt chat :last-updated)
                          (condition-case nil
                              (parse-iso8601-time-string (map-elt chat :last-updated))
                            (error nil))))
             (date-time (when timestamp
                          (decode-time timestamp)))
             (date-label (if date-time
                             (cond
                              ;; Today
                              ((and (= (decoded-time-year date-time)
                                       (decoded-time-year today))
                                    (= (decoded-time-month date-time)
                                       (decoded-time-month today))
                                    (= (decoded-time-day date-time)
                                       (decoded-time-day today)))
                               "Today")
                              ;; Yesterday
                              ((and (= (decoded-time-year date-time)
                                       (decoded-time-year yesterday))
                                    (= (decoded-time-month date-time)
                                       (decoded-time-month yesterday))
                                    (= (decoded-time-day date-time)
                                       (decoded-time-day yesterday)))
                               "Yesterday")
                              ;; Other dates - format as "Month Day"
                              (t
                               (format-time-string "%B %e" timestamp)))
                           ;; No timestamp - use "Sometime"
                           "Sometime"))
             (date-group (map-elt date-groups date-label)))
        (if date-group
            ;; Append/modify existing group.
            (setcdr date-group (append (cdr date-group) (list chat)))
          ;; Create new group
          (push (cons date-label (list chat)) date-groups))))
    (nreverse date-groups)))

(cl-defun wasabi--format-chat-preview (&key display-name is-group last-updated)
  "Format a chat preview line for the chats list.

DISPLAY-NAME is the contact/group name.
IS-GROUP indicates if this is a group chat.
LAST-UPDATED is the ISO timestamp string."
  (let ((time-str (when last-updated
                    (condition-case nil
                        (format-time-string "%H:%M" (parse-iso8601-time-string last-updated))
                      (error "")))))
    (concat (if is-group
                (propertize "G" 'face 'success)
              " ")
            " "
            (if time-str
                (propertize time-str 'face 'font-lock-comment-face)
              "     ")
            " "
            display-name)))

(defun wasabi--refresh ()
  "Refresh the display based on current status."
  (let* ((status (map-elt (wasabi--state) :status))
         (chats-index (map-elt (wasabi--state) :chats-index))
         ;; Save position
         (saved-line (line-number-at-pos))
         (saved-col (current-column)))
    (cond
     ;; Only render chat list when status is 'ready
     ((eq (map-elt status :type) 'ready)
      (if (null chats-index)
          ;; No chats available - show empty state
          (let ((inhibit-read-only t))
            (erase-buffer)
            (wasabi--message :text
                             (concat
                              "No recent chats"
                              "\n\n"
                              (propertize "c" 'face 'help-key-binding)
                              " "
                              "to start a new chat")))
        ;; Render chat list
        (let ((sections (mapcar
                         (lambda (date-group)
                           (let* ((date-label (car date-group))
                                  (chats (cdr date-group))
                                  (chat-lines
                                   (mapcar
                                    (lambda (chat)
                                      (wasabi--add-action-to-text
                                       (wasabi--format-chat-preview
                                        :display-name (map-elt chat :display-name)
                                        :is-group (map-elt chat :is-group)
                                        :last-updated (map-elt chat :last-updated))
                                       (lambda ()
                                         (interactive)
                                         (wasabi--send-chat-history-request
                                          :chat-jid (map-elt chat :chat-jid)
                                          :contact-name (map-elt chat :display-name)))))
                                    chats)))
                             (concat (propertize date-label 'face 'bold)
                                     "\n\n"
                                     (mapconcat #'identity chat-lines "\n"))))
                         (wasabi--group-chats-by-date chats-index))))
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert "\n")
            (insert (mapconcat #'identity sections "\n\n")))
          ;; Restore point position
          (goto-char (point-min))
          (forward-line (1- saved-line))
          (move-to-column saved-col))))
     ((map-elt status :message)
      ;; Not ready yet, display centered message.
      (wasabi--message :text (map-elt status :message))))))

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
Returns alist with :full-name and :push-name.
Empty strings are converted to nil for easier fallback logic."
  (let ((full-name (map-elt p-contact 'FullName))
        (push-name (map-elt p-contact 'PushName)))
    `((:full-name . ,(and full-name (not (string-empty-p full-name)) full-name))
      (:push-name . ,(and push-name (not (string-empty-p push-name)) push-name)))))

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
Returns alist with :name.
Empty strings are converted to nil for easier fallback logic."
  (let ((name (map-elt p-group 'Name)))
    `((:name . ,(and name (not (string-empty-p name)) name)))))

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
  "Instantiate a \"user.contacts\" request to get all contacts.

Requires user TOKEN."
  (unless token (error ":token is required"))
  `((:method . "user.contacts")
    (:params . ((token . ,token)))))

(cl-defun wasabi--make-group-list-request (&key token)
  "Instantiate a \"group.list\" request to get all groups.

Requires user TOKEN."
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
                                                    proxy-config)
  "Instantiate an \"admin.users.add\" request.

Required parameters:
  ADMIN-TOKEN - Admin authentication token
  NAME - User display name
  TOKEN - User authentication token

Optional parameters:
  EVENTS - Event subscriptions (list or comma-separated string)
  HISTORY - Number of historical messages to sync
  HMAC-KEY - HMAC key for message authentication
  EXPIRATION - Token expiration time
  PROXY-CONFIG - Proxy configuration"
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
                          (proxyConfig . ,proxy-config))
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

(defun wasabi--face-height-pixels (face)
  "Get the approximate pixel height of FACE."
  (let* ((height-attr (face-attribute face :height nil 'default))
         (height-points (cond
                         ((integerp height-attr) (/ height-attr 10.0))
                         ((floatp height-attr) (* height-attr 12.0)) ; assume 12pt default
                         (t 12.0)))) ; fallback to 12pt
    ;; Convert points to pixels (assuming 96 DPI)
    (round (* height-points 96.0 (/ 1.0 72.0)))))

(provide 'wasabi)

;;; wasabi.el ends here
