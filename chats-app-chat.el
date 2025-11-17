;;; chats-app-chat.el --- Chat buffer for chats-app  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Alvaro Ramirez

;; Author: Alvaro Ramirez https://xenodium.com
;; URL: https://github.com/xenodium/chats-app
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))

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

;; chats-app-chat provides the chat buffer functionality for chats-app,
;; handling the display and interaction with individual WhatsApp
;; conversations.

;;; Code:

(eval-when-compile
  (require 'cl-lib))
(require 'map)

(declare-function chats-app--log "chats-app")
(declare-function chats-app--add-action-to-text "chats-app")
(declare-function chats-app--buffer "chats-app")
(declare-function chats-app--send-chat-send-text-request "chats-app")
(declare-function chats-app--send-chat-history-request "chats-app")
(declare-function chats-app--send-download-video-request "chats-app")
(declare-function chats-app--send-download-image-request "chats-app")

(defvar-local chats-app-chat--chat (chats-app-chat--make-chat)
  "Alist containing chat information for this buffer.
Keys:
  :chat-jid - The chat JID
  :contact-name - The contact name
  :max-sender-width - Maximum sender name width for alignment")

(defvar-local chats-app-chat--prompt-marker nil
  "Marker for the start of the prompt.")

(defvar-local chats-app-chat--input-start-marker nil
  "Marker for the start of user input.")

(cl-defun chats-app-chat--make-chat (&key chat-jid contact-name max-sender-width messages)
  "Create a chat alist with CHAT-JID, CONTACT-NAME, MAX-SENDER-WIDTH, and MESSAGES."
  (list (cons :chat-jid chat-jid)
        (cons :contact-name contact-name)
        (cons :max-sender-width (or max-sender-width 0))
        (cons :messages (or messages nil))))

(defun chats-app-chat--update-chat (key value)
  "Update KEY in chats-app-chat--chat with VALUE, preserving other keys."
  (setq chats-app-chat--chat
        (cons (cons key value)
              (assq-delete-all key chats-app-chat--chat))))

(defvar chats-app-chat-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'chats-app-chat-quit)
    (define-key map (kbd "n") #'chats-app-chat-next-message)
    (define-key map (kbd "p") #'chats-app-chat-previous-message)
    (define-key map (kbd "g") #'chats-app-chat-refresh)
    (define-key map (kbd "RET") #'chats-app-chat-send-input)
    (define-key map (kbd "C-a") #'chats-app-chat-beginning-of-line)
    map)
  "Keymap for `chats-app-chat-mode'.")

;; Parsing functions - convert protocol structures to internal format

(defun chats-app-chat--parse-content (p-message)
  "Parse displayable content from protocol P-MESSAGE structure.
Returns string like \"Hello\" or \"[image]\"."
  (cond
   ((map-elt p-message 'conversation)
    (map-elt p-message 'conversation))
   ((map-elt p-message 'extendedTextMessage)
    (map-nested-elt p-message '(extendedTextMessage text)))
   ((map-elt p-message 'imageMessage)
    (chats-app--log "Image message: %s" p-message)
    (let* ((thumbnail (map-nested-elt p-message '(imageMessage JPEGThumbnail)))
           (image-text (if thumbnail
                           (propertize "[image]" 'display
                                       (create-image (base64-decode-string thumbnail)
                                                     'jpeg t
                                                     :max-width 120 :max-height 120))
                         "[image]")))
      ;; Store metadata as text properties
      (add-text-properties 0 (length image-text)
                           `(image-url ,(map-nested-elt p-message '(imageMessage URL))
                                       image-direct-path ,(map-nested-elt p-message '(imageMessage directPath))
                                       image-media-key ,(map-nested-elt p-message '(imageMessage mediaKey))
                                       image-mimetype ,(map-nested-elt p-message '(imageMessage mimetype))
                                       image-file-enc-sha256 ,(map-nested-elt p-message '(imageMessage fileEncSHA256))
                                       image-file-sha256 ,(map-nested-elt p-message '(imageMessage fileSHA256))
                                       image-file-length ,(map-nested-elt p-message '(imageMessage fileLength))
                                       image-width ,(map-nested-elt p-message '(imageMessage width))
                                       image-height ,(map-nested-elt p-message '(imageMessage height))
                                       image-thumbnail ,thumbnail)
                           image-text)
      ;; Add action to view image on RET
      (setq image-text (chats-app--add-action-to-text
                        image-text
                        (lambda ()
                          (interactive)
                          (chats-app-chat-view-image-at-point))))
      (concat image-text
              (when-let ((caption (map-nested-elt p-message '(imageMessage caption))))
                (concat "\n" caption)))))
   ((map-elt p-message 'videoMessage)
    (chats-app--log "Video message: %s" p-message)
    (let* ((thumbnail (map-nested-elt p-message '(videoMessage JPEGThumbnail)))
           (video-text (if thumbnail
                           (propertize "[video]" 'display
                                       (create-image (base64-decode-string thumbnail)
                                                     'jpeg t
                                                     :max-width 120 :max-height 120))
                         "[video]")))
      ;; Store metadata as text properties
      (add-text-properties 0 (length video-text)
                           `(video-url ,(map-nested-elt p-message '(videoMessage URL))
                             video-direct-path ,(map-nested-elt p-message '(videoMessage directPath))
                             video-media-key ,(map-nested-elt p-message '(videoMessage mediaKey))
                             video-mimetype ,(map-nested-elt p-message '(videoMessage mimetype))
                             video-file-enc-sha256 ,(map-nested-elt p-message '(videoMessage fileEncSHA256))
                             video-file-sha256 ,(map-nested-elt p-message '(videoMessage fileSHA256))
                             video-file-length ,(map-nested-elt p-message '(videoMessage fileLength))
                             video-seconds ,(map-nested-elt p-message '(videoMessage seconds))
                             video-width ,(map-nested-elt p-message '(videoMessage width))
                             video-height ,(map-nested-elt p-message '(videoMessage height)))
                           video-text)
      ;; Add action to play video on RET
      (setq video-text (chats-app--add-action-to-text
                        video-text
                        (lambda ()
                          (interactive)
                          (chats-app-chat-play-video-at-point))))
      (concat video-text
              (when-let ((caption (map-nested-elt p-message '(videoMessage caption))))
                (concat "\n" caption)))))
   ((map-elt p-message 'documentMessage) "[document]")
   ((map-elt p-message 'audioMessage) "[audio]")
   ((map-elt p-message 'stickerMessage)
    ;; (message "[sticker]\n\n%s" p-message)
    "[sticker]")
   ((map-elt p-message 'reactionMessage)
    ;; (message "[reaction]\n\n%s" p-message)
    "[reaction]")
   (t
    ;; (message "[unknown]\n\n%s" p-message)
    "[unknown]")))

(cl-defun chats-app-chat--parse-sender-name (p-data p-sender-jid &key contacts contact-name)
  "Parse sender name from P-DATA and P-SENDER-JID.
CONTACTS is the internal contacts alist for name resolution.
CONTACT-NAME is an optional fallback name."
  (cond
   ((map-nested-elt p-data '(Info IsFromMe))
    "Me")
   ((and p-sender-jid contacts
         (assoc (intern p-sender-jid) contacts))
    (let* ((contact (assoc (intern p-sender-jid) contacts))
           (full-name (map-elt (cdr contact) :full-name))
           (push-name (map-elt (cdr contact) :push-name)))
      (or (and full-name (not (string-empty-p full-name)) full-name)
          (and push-name (not (string-empty-p push-name)) push-name))))
   (t
    (let ((push-name (map-nested-elt p-data '(Info PushName))))
      (or (and push-name (not (string-empty-p push-name)) push-name)
          (and contact-name (not (string-empty-p contact-name)) contact-name)
          (and p-sender-jid
               (if (string-match "\\([^@]+\\)@" p-sender-jid)
                   (match-string 1 p-sender-jid)
                 p-sender-jid))
          "Unknown")))))

(cl-defun chats-app-chat--parse-message (p-message &key chat-jid contact-name contacts reactions)
  "Parse a protocol message (from database) into internal display format.
Returns alist with :sender-name, :timestamp, :content, :message-id, and :reactions.
Returns nil for reaction messages (they're handled separately).
CONTACTS should be internal contacts alist for sender name resolution.
REACTIONS is a hash table of message-id -> list of reactions."
  (let* ((data-json (map-elt p-message 'data_json))
         (timestamp (map-elt p-message 'timestamp))
         (msg-id (map-elt p-message 'message_id)))
    (if (and data-json (not (string-empty-p data-json)))
        ;; Parse from data_json (preferred - has full info)
        (let ((p-data (json-parse-string data-json :object-type 'alist
                                         :null-object nil
                                         :false-object nil)))
          ;; Skip reaction messages - they're already in reactions.
          (unless (map-nested-elt p-data '(Message reactionMessage))
            (let* ((p-sender-jid (map-nested-elt p-data '(Info Sender)))
                   (p-sender-name (chats-app-chat--parse-sender-name p-data p-sender-jid
                                                                      :contacts contacts
                                                                      :contact-name contact-name)))
              (if (and msg-id reactions (map-elt reactions msg-id))
                  `((:message-id . ,msg-id)
                    (:sender-name . ,p-sender-name)
                    (:timestamp . ,(map-nested-elt p-data '(Info Timestamp)))
                    (:content . ,(chats-app-chat--parse-content (map-elt p-data 'Message)))
                    (:reactions . ,(reverse (map-elt reactions msg-id))))
                `((:message-id . ,msg-id)
                  (:sender-name . ,p-sender-name)
                  (:timestamp . ,(map-nested-elt p-data '(Info Timestamp)))
                  (:content . ,(chats-app-chat--parse-content (map-elt p-data 'Message))))))))
      ;; Fallback: parse from basic fields (outgoing messages without data_json)
      (let* ((is-from-me (string= (map-elt p-message 'sender_jid) "me"))
             (sender-name (if is-from-me "Me" (or contact-name chat-jid)))
             (content (or (map-elt p-message 'text_content) "[message]"))
             (reactions (when (and msg-id reactions)
                          (map-elt reactions msg-id))))
        (if reactions
            `((:message-id . ,msg-id)
              (:sender-name . ,sender-name)
              (:timestamp . ,timestamp)
              (:content . ,content)
              (:reactions . ,(reverse reactions)))
          `((:message-id . ,msg-id)
            (:sender-name . ,sender-name)
            (:timestamp . ,timestamp)
            (:content . ,content)))))))

(cl-defun chats-app-chat--parse-notification (&key p-message p-info contact-name chat-jid contacts)
  "Parse protocol notification MESSAGE and INFO into internal message format.
Returns alist with :sender-name, :timestamp, :content.
For reaction messages, also includes :is-reaction, :target-id, and :emoji."
  (if-let ((reaction-msg (map-elt p-message 'reactionMessage)))
      ;; This is a reaction
      (let* ((target-id (map-nested-elt reaction-msg '(key ID)))
             (emoji (map-elt reaction-msg 'text))
             (sender-jid (map-elt p-info 'Sender))
             (sender-name (if (map-elt p-info 'IsFromMe)
                              "Me"
                            (or contact-name
                                (map-elt p-info 'PushName)
                                (when sender-jid
                                  (if (string-match "\\([^@]+\\)@" sender-jid)
                                      (match-string 1 sender-jid)
                                    sender-jid))
                                chat-jid))))
        `((:is-reaction . t)
          (:target-id . ,target-id)
          (:emoji . ,emoji)
          (:sender . ,sender-name)))
    ;; Regular message
    (let* ((is-from-me (map-elt p-info 'IsFromMe))
           (sender-name (if is-from-me
                            "Me"
                          (or contact-name
                              (map-elt p-info 'PushName)
                              (when-let ((sender (map-elt p-info 'Sender)))
                                (if (string-match "\\([^@]+\\)@" sender)
                                    (match-string 1 sender)
                                  sender))
                              chat-jid)))
           (content (chats-app-chat--parse-content p-message))
           (timestamp (map-elt p-info 'Timestamp)))
      `((:sender-name . ,sender-name)
        (:timestamp . ,timestamp)
        (:content . ,content)))))

(cl-defun chats-app-chat--parse-reactions (p-messages &key contacts)
  "Parse reactions from P-MESSAGES and return a hash map of message-id -> reactions.
Each reaction is an alist with :emoji and :sender keys.
CONTACTS is used to resolve sender names."
  (let ((reactions (make-hash-table :test 'equal)))
    (dolist (p-msg (append p-messages nil))
      (when-let* ((data-json (map-elt p-msg 'data_json))
                  ((not (string-empty-p data-json)))
                  (p-data (json-parse-string data-json :object-type 'alist
                                             :null-object nil
                                             :false-object nil))
                  ((map-nested-elt p-data '(Message reactionMessage)))
                  (p-target-id (map-nested-elt p-data '(Message reactionMessage key ID)))
                  (p-sender-jid (map-nested-elt p-data '(Info Sender)))
                  (p-sender-name (chats-app-chat--parse-sender-name p-data p-sender-jid
                                                                     :contacts contacts)))
        (map-put! reactions p-target-id
                  (cons `((:emoji . ,(map-nested-elt p-data '(Message reactionMessage text)))
                          (:sender . ,p-sender-name))
                        (map-elt reactions p-target-id)))))
    reactions))

(cl-defun chats-app-chat--parse-messages (p-messages &key chat-jid contact-name contacts)
  "Parse array of protocol messages into list of internal display messages.
Returns list of message alists, sorted by timestamp (oldest first).
Messages with reactions will have a :reactions field."
  (let* ((reactions (chats-app-chat--parse-reactions p-messages :contacts contacts))
         (parsed (delq nil
                       (mapcar (lambda (p-msg)
                                 (chats-app-chat--parse-message p-msg
                                                                :chat-jid chat-jid
                                                                :contact-name contact-name
                                                                :contacts contacts
                                                                :reactions reactions))
                               (append p-messages nil)))))
    (sort parsed
          (lambda (a b)
            (string< (map-elt a :timestamp)
                     (map-elt b :timestamp))))))

(defun chats-app-chat--calculate-max-sender-width (messages)
  "Calculate maximum sender name width from internal MESSAGES for alignment."
  (if (null messages)
      0
    (apply #'max
           (mapcar (lambda (msg)
                     (string-width (map-elt msg :sender-name)))
                   messages))))

;; UI functions

(defun chats-app-chat--setup-prompt ()
  "Set up the read-only prompt at the end of the buffer."
  (goto-char (point-max))
  (let ((inhibit-read-only t)
        (prompt-start (point)))
    ;; Ensure we're on a new line
    (unless (bolp)
      (insert "\n"))
    (insert "> ")
    (setq chats-app-chat--prompt-marker (copy-marker prompt-start))
    (setq chats-app-chat--input-start-marker (point-marker))
    (set-marker-insertion-type chats-app-chat--input-start-marker nil)
    (set-marker-insertion-type chats-app-chat--prompt-marker t)
    (put-text-property prompt-start (point) 'read-only t)
    (put-text-property prompt-start (point) 'rear-nonsticky '(read-only))
    (put-text-property prompt-start (point) 'front-sticky '(read-only))))

(defun chats-app-chat--get-prompt-input ()
  "Get the current input text after the prompt."
  (when chats-app-chat--input-start-marker
    (buffer-substring-no-properties chats-app-chat--input-start-marker (point-max))))

(defun chats-app-chat--clear-prompt-input ()
  "Clear the input area after the prompt."
  (when chats-app-chat--input-start-marker
    (delete-region chats-app-chat--input-start-marker (point-max))))

(define-derived-mode chats-app-chat-mode fundamental-mode "ChatsApp"
  "Major mode for displaying individual chat conversations.

\\{chats-app-chat-mode-map}"
  (setq-local inhibit-read-only nil))

(defun chats-app-chat--in-input-area-p ()
  "Return non-nil if point is in the input area."
  (and chats-app-chat--input-start-marker
       (>= (point) chats-app-chat--input-start-marker)))

(defun chats-app-chat-beginning-of-line ()
  "Like `move-beginning-of-line' but prompt-aware."
  (interactive)
  (if (and (chats-app-chat--in-input-area-p)
           chats-app-chat--input-start-marker)
      (if (= (point) chats-app-chat--input-start-marker)
          ;; Already at input start, go to real beginning
          (move-beginning-of-line 1)
        ;; Go to input start
        (goto-char chats-app-chat--input-start-marker))
    ;; Not in input area, use default behavior
    (move-beginning-of-line 1)))

(defun chats-app-chat-quit ()
  "Quit the chat buffer."
  (interactive)
  (unless (derived-mode-p 'chats-app-chat-mode)
    (error "Not in a chat buffer"))
  (if (chats-app-chat--in-input-area-p)
      (self-insert-command 1)
    (quit-restore-window (get-buffer-window (current-buffer)) 'kill)))

(defun chats-app-chat-send-input ()
  "Send the current input as a message."
  (interactive)
  (unless (chats-app-chat--in-input-area-p)
    (user-error "Press RET after > prompt to send a message"))
  (when (string-empty-p (string-trim (chats-app-chat--get-prompt-input)))
    (user-error "Nothing to send"))
  (unless chats-app-chat--chat
    (error "No chat information available"))
  (unless (map-elt chats-app-chat--chat :chat-jid)
    (error "No chat JID available"))
  (let ((text (string-trim (chats-app-chat--get-prompt-input)))
        (chat-jid (map-elt chats-app-chat--chat :chat-jid))
        (chat-buffer (current-buffer)))
    (chats-app-chat--clear-prompt-input)
    (message "Sending...")
    (with-current-buffer (chats-app--buffer)
      (chats-app--send-chat-send-text-request
       :phone chat-jid
       :body text
       :on-failure (lambda (error)
                     (message "Failed to send")
                     (chats-app--log "Failed to send message: %s"
                                     (or (map-elt error 'message)
                                         "unknown error"))
                     ;; Restore cleared input
                     (with-current-buffer chat-buffer
                       (goto-char (point-max))
                       (insert text)))
       :on-success (lambda (response)
                     (message "Sent")
                     ;; Response Timestamp is Unix timestamp (integer),
                     ;; convert to ISO 8601 string.
                     (let* ((timestamp-str (format-time-string "%Y-%m-%dT%H:%M:%S%z" (map-elt response 'Timestamp)))
                            (message `((:sender-name . "Me")
                                       (:timestamp . ,timestamp-str)
                                       (:content . ,text))))
                       (with-current-buffer chat-buffer
                         (chats-app-chat--append-message message))
                       (with-current-buffer chat-buffer
                         (goto-char (point-max)))))))))

(defun chats-app-chat-refresh ()
  "Refresh the current chat buffer by fetching new messages."
  (interactive)
  (if (chats-app-chat--in-input-area-p)
      (self-insert-command 1)
    (unless chats-app-chat--chat
      (error "No chat information available"))
    (let ((chat-jid (or (map-elt chats-app-chat--chat :chat-jid)
                        (error "No chat JID available")))
          (contact-name (map-elt chats-app-chat--chat :contact-name)))
      (with-current-buffer (chats-app--buffer)
        (chats-app--send-chat-history-request
         :chat-jid chat-jid
         :contact-name contact-name
         :on-finished (lambda ()
                        (message "Refreshed")))))))

(defun chats-app-chat-next-message ()
  "Jump to the next message (sender line)."
  (interactive)
  (unless (derived-mode-p 'chats-app-chat-mode)
    (error "Not in a chat buffer"))
  (if (chats-app-chat--in-input-area-p)
      (self-insert-command 1)
    ;; First, skip past the current sender if we're on one
    (let ((start-pos (save-excursion
                       (end-of-line)
                       (if (get-text-property (point) 'chats-app-sender)
                           (or (next-single-property-change (point) 'chats-app-sender)
                               (point-max))
                         (point)))))
      ;; Then find the next sender
      (let ((pos (next-single-property-change start-pos 'chats-app-sender)))
        (if (and pos (get-text-property pos 'chats-app-sender))
            (progn
              (goto-char pos)
              (beginning-of-line))
          ;; If at last message, bump to prompt.
          (goto-char (point-max)))))))

(defun chats-app-chat-previous-message ()
  "Jump to the previous message (sender line)."
  (interactive)
  (unless (derived-mode-p 'chats-app-chat-mode)
    (error "Not in a chat buffer"))
  (if (chats-app-chat--in-input-area-p)
      (self-insert-command 1)
    ;; First, skip to the start of current sender if we're in the middle of one
    (let ((start-pos (if (get-text-property (point) 'chats-app-sender)
                         (or (previous-single-property-change (point) 'chats-app-sender)
                             (point-min))
                       (point))))
      ;; Then find the previous sender
      (let ((pos (previous-single-property-change start-pos 'chats-app-sender)))
        (if pos
            ;; Move to the start of that sender region
            (let ((sender-start (or (previous-single-property-change pos 'chats-app-sender)
                                    (point-min))))
              (progn
                (goto-char (if (get-text-property sender-start 'chats-app-sender)
                               sender-start
                             pos))
                (beginning-of-line)))
          (message "No previous message"))))))

(defun chats-app-chat--refresh (messages)
  "Refresh the current chat buffer with internal MESSAGES.
MESSAGES is a list of already-parsed internal message alists."
  (unless (derived-mode-p 'chats-app-chat-mode)
    (error "Not in a chat buffer"))
  (map-put! chats-app-chat--chat :messages messages)
  (map-put! chats-app-chat--chat
            :max-sender-width (chats-app-chat--calculate-max-sender-width messages))
  (let ((inhibit-read-only t))
    (erase-buffer)
    (if (null messages)
        (let ((start (point)))
          (insert (propertize "\n[no chat history found]\n\n"
                              'face 'font-lock-comment-face))
          (put-text-property start (point) 'read-only t))
      (chats-app-chat--render-messages messages)
      (let ((start (point)))
        (insert "\n\n")
        (put-text-property start (point) 'read-only t)))
    (chats-app-chat--setup-prompt)
    (goto-char (point-max))))

(cl-defun chats-app-chat--render-message (&key sender-name timestamp content max-sender-width reactions)
  "Render a single internal message.
SENDER-NAME is the display name of the sender.
TIMESTAMP is the ISO8601 timestamp string.
CONTENT is the display content (already parsed, may include text properties).
MAX-SENDER-WIDTH is used for padding alignment.
REACTIONS is a list of reaction alists with :emoji and :sender keys."
  (let* ((col1-width max-sender-width)
         (is-from-me (string= sender-name "Me"))
         (sender (propertize sender-name
                             'face `(:inherit ,(if is-from-me
                                                   'font-lock-variable-name-face
                                                 'font-lock-function-name-face) :box nil)
                             'chats-app-sender t))
         (sender-padding (make-string (max 0 (- (or max-sender-width 0)
                                                (string-width sender))) ?\s))
         (time (when timestamp
                 (propertize (format-time-string "%H:%M" (parse-iso8601-time-string timestamp))
                             'face 'font-lock-comment-face))))
    ;;
    ;; Intended layout per message:
    ;;
    ;; Mateo 15:32
    ;;       Off to granny's
    ;;
    ;; With reactions:
    ;;
    ;; Mateo 15:32
    ;;       Off to granny's
    ;;       ❤️ George
    ;;       ❤️ Paul
    ;;
    (concat sender-padding sender " " (or time "")
            "\n" (make-string col1-width ?\s) " "
            (string-replace "\n" (concat "\n " (make-string col1-width ?\s)) content)
            ;; Add reactions below the message
            (when reactions
              (concat "\n"
                      (mapconcat
                       (lambda (reaction)
                         (let ((emoji (map-elt reaction :emoji))
                               (reaction-sender (map-elt reaction :sender)))
                           (concat (make-string col1-width ?\s) " "
                                   emoji
                                   " "
                                   (propertize reaction-sender
                                               'face 'font-lock-comment-face))))
                       reactions
                       "\n"))))))

(defun chats-app-chat--render-messages (messages)
  "Render internal format MESSAGES to current buffer.
MESSAGES is a list of alists with :sender-name, :timestamp, :content."
  (let* ((max-sender-width (map-elt chats-app-chat--chat :max-sender-width))
         (message-lines
          (mapcar
           (lambda (msg)
             (chats-app-chat--render-message
              :sender-name (map-elt msg :sender-name)
              :timestamp (map-elt msg :timestamp)
              :content (map-elt msg :content)
              :max-sender-width max-sender-width
              :reactions (map-elt msg :reactions)))
           messages)))
    (let ((start (point)))
      (insert (mapconcat #'identity message-lines "\n\n"))
      (put-text-property start (point) 'read-only t))))

(defun chats-app-chat--append-message (message)
  "Append a single internal MESSAGE to current chat buffer.
MESSAGE is an alist with :sender-name, :timestamp, :content.
Updates :messages list and :max-sender-width in chat state."
  (unless (derived-mode-p 'chats-app-chat-mode)
    (error "Not in a chat buffer"))
  (unless message
    (error "message is required"))
  (let ((inhibit-read-only t)
        (saved-input nil))
    (save-excursion
      (when chats-app-chat--prompt-marker
        ;; Save any user input before deleting the prompt
        (setq saved-input (chats-app-chat--get-prompt-input))
        ;; Delete the existing prompt
        (delete-region chats-app-chat--prompt-marker (point-max)))
      (goto-char (point-max))
      (let* ((start (point))
             (sender-width (string-width (map-elt message :sender-name)))
             (old-max-width (or (map-elt chats-app-chat--chat :max-sender-width) 0))
             (new-max-width (max old-max-width sender-width))
             (updated-messages (append (map-elt chats-app-chat--chat :messages)
                                       (list message))))
        ;; Update chat state with new messages and max-width
        (chats-app-chat--update-chat :max-sender-width new-max-width)
        (chats-app-chat--update-chat :messages updated-messages)
        ;; Render the message
        (insert (chats-app-chat--render-message
                 :sender-name (map-elt message :sender-name)
                 :timestamp (map-elt message :timestamp)
                 :content (map-elt message :content)
                 :max-sender-width (map-elt chats-app-chat--chat :max-sender-width)
                 :reactions (map-elt message :reactions)))
        (insert "\n\n")
        (put-text-property start (point) 'read-only t))
      (chats-app-chat--setup-prompt)
      ;; Restore saved input
      (when (and saved-input (not (string-empty-p saved-input)))
        (goto-char (point-max))
        (insert saved-input)))))

(cl-defun chats-app-chat--add-reaction (&key target-id emoji sender)
  "Add a reaction to an existing message with TARGET-ID.
EMOJI is the reaction emoji, SENDER is the name of who reacted.
Finds the message in :messages, updates it, and re-renders just that message."
  (unless (derived-mode-p 'chats-app-chat-mode)
    (error "Not in a chat buffer"))
  (if-let* ((target-idx (seq-position (map-elt chats-app-chat--chat :messages)
                                      target-id
                                      (lambda (msg id) (string= (map-elt msg :message-id) id))))
            (target-msg (nth target-idx (map-elt chats-app-chat--chat :messages)))
            (updated-msg (cons `(:reactions . ,(append (map-elt target-msg :reactions)
                                                       (list `((:emoji . ,emoji) (:sender . ,sender)))))
                               (assq-delete-all :reactions (copy-alist target-msg))))
            (updated-messages (append (seq-take (map-elt chats-app-chat--chat :messages) target-idx)
                                      (list updated-msg)
                                      (seq-drop (map-elt chats-app-chat--chat :messages) (1+ target-idx)))))
      (progn
        (chats-app-chat--update-chat :messages updated-messages)
        (let ((inhibit-read-only t))
          (save-excursion
            (goto-char (point-min))
            (dotimes (_ target-idx)
              (re-search-forward "\n\n" nil t))
            (when-let ((msg-start (point))
                       (msg-end (when (re-search-forward "\n\n" nil t)
                                  (match-beginning 0))))
              (delete-region msg-start msg-end)
              (goto-char msg-start)
              (insert (chats-app-chat--render-message
                       :sender-name (map-elt updated-msg :sender-name)
                       :timestamp (map-elt updated-msg :timestamp)
                       :content (map-elt updated-msg :content)
                       :max-sender-width (map-elt chats-app-chat--chat :max-sender-width)
                       :reactions (map-elt updated-msg :reactions)))))))
    (chats-app--log "Could not find message with ID %s to add reaction" target-id)))

(cl-defun chats-app-chat--start (&key chat-jid messages contact-name)
  "Create and display a chat buffer for CHAT-JID.
MESSAGES is a list of already-parsed internal message alists.
CONTACT-NAME is the display name of the contact (or nil if not available).
Displays messages in a two-column format: sender | message."
  (unless chat-jid
    (error ":chat-jid is required"))
  ;; TODO: Consolidate buffer creation logic.
  (let ((chat-buffer (get-buffer-create (format "*ChatsApp: %s*" (or contact-name chat-jid)))))
    (with-current-buffer chat-buffer
      (unless (derived-mode-p 'chats-app-chat-mode)
        (chats-app-chat-mode))
      (setq chats-app-chat--chat (chats-app-chat--make-chat :chat-jid chat-jid
                                                            :contact-name contact-name))
      (chats-app-chat--refresh messages)
      (goto-char (point-max)))

    (switch-to-buffer chat-buffer)))

(defun chats-app-chat-play-video-at-point ()
  "Download and play the video at point using external player."
  (interactive)
  (unless (get-text-property (point) 'video-url)
    (user-error "No video at point"))
  (let* ((url (get-text-property (point) 'video-url))
         (direct-path (get-text-property (point) 'video-direct-path))
         (media-key (get-text-property (point) 'video-media-key))
         (mimetype (get-text-property (point) 'video-mimetype))
         (file-enc-sha256 (get-text-property (point) 'video-file-enc-sha256))
         (file-sha256 (get-text-property (point) 'video-file-sha256))
         (file-length (get-text-property (point) 'video-file-length))
         ;; Check if file already exists
         (file-id (if file-sha256
                      (replace-regexp-in-string "[^a-zA-Z0-9]" "" file-sha256)
                    (format "%d" (random 1000000))))
         (extension (cond
                     ((string-match "video/mp4" mimetype) ".mp4")
                     ((string-match "video/quicktime" mimetype) ".mov")
                     ((string-match "video/x-matroska" mimetype) ".mkv")
                     ((string-match "video/webm" mimetype) ".webm")
                     (t ".mp4")))
         (temp-file (expand-file-name (concat "chatsapp-video-" file-id extension)
                                      temporary-file-directory)))
    (if (file-exists-p temp-file)
        ;; File already downloaded, just open it
        (chats-app-chat--open-video-externally temp-file)
      ;; Download the video
      (message "Downloading video...")
      (with-current-buffer (chats-app--buffer)
        (chats-app--send-download-video-request
         :url url
         :direct-path direct-path
         :media-key media-key
         :mimetype mimetype
         :file-enc-sha256 file-enc-sha256
         :file-sha256 file-sha256
         :file-length file-length
         :on-success (lambda (response)
                       (message "Downloading video... done")
                       (chats-app-chat--save-and-play-video
                        :data-url (map-elt response 'Data)
                        :mimetype mimetype
                        :file-sha256 file-sha256))
         :on-failure (lambda (error)
                       (message "Failed to download video: %s"
                                (or (map-elt error 'message) "unknown"))))))))

(cl-defun chats-app-chat--save-and-play-video (&key data-url mimetype file-sha256)
  "Save video to temporary file and open with external player.
DATA-URL is the base64-encoded data URL from the backend.
MIMETYPE is the video MIME type.
FILE-SHA256 is used to create a unique filename."
  (unless data-url
    (error ":data-url is required"))
  ;; Extract base64 data from data URL
  (unless (string-match "data:[^;]+;base64,\\(.*\\)" data-url)
    (error "Invalid data URL format"))
  (let* ((base64-data (match-string 1 data-url))
         (video-data (base64-decode-string base64-data))
         ;; Use fileSHA256 (base64) as unique identifier, sanitize for filename
         (file-id (if file-sha256
                      (replace-regexp-in-string "[^a-zA-Z0-9]" "" file-sha256)
                    (format "%d" (random 1000000))))
         ;; Determine extension from mimetype
         (extension (cond
                     ((string-match "video/mp4" mimetype) ".mp4")
                     ((string-match "video/quicktime" mimetype) ".mov")
                     ((string-match "video/x-matroska" mimetype) ".mkv")
                     ((string-match "video/webm" mimetype) ".webm")
                     (t ".mp4")))
         (temp-file (expand-file-name (concat "chatsapp-video-" file-id extension)
                                      temporary-file-directory)))
    ;; Write video data to temp file
    (with-temp-file temp-file
      (set-buffer-multibyte nil)
      (insert video-data))
    (chats-app-chat--open-video-externally temp-file)))

(defun chats-app-chat--open-video-externally (file-path)
  "Open video FILE-PATH with system default player."
  (cond
   ;; macOS
   ((eq system-type 'darwin)
    (start-process "open-video" nil "open" file-path))
   ;; Linux
   ((eq system-type 'gnu/linux)
    (start-process "open-video" nil "xdg-open" file-path))
   ;; Windows
   ((memq system-type '(windows-nt ms-dos))
    (start-process "open-video" nil "cmd" "/c" "start" "" file-path))
   ;; Fallback
   (t
    (browse-url-of-file file-path))))

(defun chats-app-chat-view-image-at-point ()
  "View the full image at point in a *ChatsApp photo* buffer."
  (interactive)
  (unless (get-text-property (point) 'image-url)
    (user-error "No image at point"))
  (message "Downloading image...")
  (let ((url (get-text-property (point) 'image-url))
        (direct-path (get-text-property (point) 'image-direct-path))
        (media-key (get-text-property (point) 'image-media-key))
        (mimetype (get-text-property (point) 'image-mimetype))
        (file-enc-sha256 (get-text-property (point) 'image-file-enc-sha256))
        (file-sha256 (get-text-property (point) 'image-file-sha256))
        (file-length (get-text-property (point) 'image-file-length))
        (width (get-text-property (point) 'image-width))
        (height (get-text-property (point) 'image-height)))
    (with-current-buffer (chats-app--buffer)
      (chats-app--send-download-image-request
       :url url
       :direct-path direct-path
       :media-key media-key
       :mimetype mimetype
       :file-enc-sha256 file-enc-sha256
       :file-sha256 file-sha256
       :file-length file-length
       :on-success (lambda (response)
                     (message "Downloading image... done")
                     (chats-app-chat--display-image-in-buffer
                      :data-url (map-elt response 'Data)
                      :mimetype (map-elt response 'Mimetype)
                      :width width
                      :height height))
       :on-failure (lambda (error)
                     (message "Failed to download image: %s"
                              (or (map-elt error 'message) "unknown")))))))

(cl-defun chats-app-chat--display-image-in-buffer (&key data-url mimetype width height)
  "Display image in *ChatsApp photo* buffer.
DATA-URL is the base64-encoded data URL from the backend.
MIMETYPE is the image MIME type."
  (unless data-url
    (error ":data-url is required"))
  ;; Extract base64 data from data URL (format: "data:image/jpeg;base64,...")
  (unless (string-match "data:[^;]+;base64,\\(.*\\)" data-url)
    (error "Invalid data URL format"))
  (let* ((base64-data (match-string 1 data-url))
         (image-data (base64-decode-string base64-data))
         (image-type (cond
                      ((string-match "image/jpeg" mimetype) 'jpeg)
                      ((string-match "image/png" mimetype) 'png)
                      ((string-match "image/gif" mimetype) 'gif)
                      ((string-match "image/webp" mimetype) 'imagemagick)
                      (t 'imagemagick)))
         (image (create-image image-data image-type t))
         (photo-buffer (get-buffer-create "*ChatsApp photo*")))
    (with-current-buffer photo-buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (fundamental-mode)
        (setq buffer-read-only t)
        (local-set-key (kbd "q") #'quit-window)
        (insert (propertize "q" 'face 'help-key-binding) " to close")
        (insert "\n\n")
        (insert (propertize "🌄" 'display image))
        (insert "\n")
        (goto-char (point-min))))
    (switch-to-buffer photo-buffer)))

(provide 'chats-app-chat)
;;; chats-app-chat.el ends here
