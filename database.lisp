;; Database interaction

(in-package #:zoglog)

(defvar *database-name* "zoglog")
(defvar *database-user* "zoglog")
(defvar *database-password* "zoglog")
(defvar *database-host* "localhost")
(defvar *database-port* 5432)

(defparameter *db-reconnect-timeout* 10)
(defparameter *db-reconnect* t)

(defmacro with-db (&body body)
  `(handler-bind
       ((postmodern:database-connection-error
         (lambda (c)
           (vom:error "Database error '~a'" c)
           (when *db-reconnect*
             (sleep *db-reconnect-timeout*)
             (invoke-restart :reconnect)))))
     (postmodern:with-connection
         `(,*database-name*
           ,*database-user*
           ,*database-password*
           ,*database-host*
           :port ,*database-port*
           :pooled-p t)
       ,@body)))

(defclass event ()
  ((id :accessor id :col-type serial :initarg :id :primary-key t)
   (date :accessor date :col-type timestamp
         :initarg :date :initform (local-time:now))
   (server :accessor server :col-type text :initarg :server)
   (channel :accessor channel :col-type text :initarg :channel)
   (nick :accessor nick :col-type (or postmodern:db-null text) :initarg :nick)
   (host :accessor host :col-type (or postmodern:db-null text) :initarg :host)
   (message-type :accessor message-type :col-type text
                 :initarg :message-type)
   (message :accessor message
            :col-type (or postmodern:db-null text) :initarg :message))
  (:documentation "Dao class for irc message.")
  (:metaclass postmodern:dao-class)
  (:table-name events)(:keys id))

(defclass server ()
  ((id :accessor id :col-type serial :initarg :id :initform nil)
   (name :accessor name :col-type name :initarg :name))
  (:documentation "Dao class for servers.")
  (:metaclass postmodern:dao-class)
  (:table-name servers)(:keys id))

(defclass channels ()
  ((id :accessor id :col-type serial :initarg :id :initform nil)
   (name :accessor name :col-type name :initarg :name)
   (server-id :accessor server-id :initarg :server-id :col-type integer))
  (:documentation "Dao class for channels.")
  (:metaclass postmodern:dao-class)
  (:table-name channels)(:keys id))

(postmodern:deftable channels
  (postmodern:!dao-def)
  (postmodern:!foreign 'servers 'server-id 'id
                       :on-delete :cascade :on-update :cascade))()

(defun table-exists (name)
  "Check if table exists."
  (postmodern:query (:select t :from 'pg-tables :where (:= 'tablename name))))

(defun extension-exists (name)
  "Check if PostgreSQL extension exists."
  (postmodern:query (:select t :from 'pg-available-extensions
                             :where (:= 'name name))))

(defun init-db ()
  "Create tables and indexes if they don't exist."
  (with-db
    (unless (table-exists "events")
      (postmodern:execute (postmodern:dao-table-definition 'event))
      (postmodern:query (:create-index 'events_serv_chan_date_idx :on "events"
				       :fields 'server 'channel 'date))
      (postmodern:query (:create-index 'events_nick_idx :on "events"
				       :fields 'nick))
      ;; Create trigram extension, often fails when user has no
      ;; superuser rights
      (ignore-errors (postmodern:query
                      (:raw "CREATE EXTENSION IF NOT EXISTS pg_trgm")))
      (when (extension-exists "pg_trgm")
        (postmodern:query
         (:raw (format nil "CREATE INDEX events_message_trgm_idx ~
                            ON events USING GIN (message gin_trgm_ops)")))))
    (unless (table-exists "servers")
      (postmodern:execute (postmodern:dao-table-definition 'server))
      (postmodern:query (:create-unique-index 'servers_name_idx :on "servers"
					      :fields 'name)))
    (unless (table-exists "channels")
      (postmodern:create-table 'channels))))

(defun disconnect-db ()
  "Disconnect from database."
  (postmodern:clear-connection-pool)
  (postmodern:disconnect-toplevel))

(defun update-db-channels (server channels)
  "Save server and channels to database if needed."
  (with-db
    (let ((server-id (postmodern:query (:select 'id :from 'servers
						:where (:= 'name server))
				       :single)))
      (when (not server-id)
	(setf server-id (postmodern:query
			 (:insert-into 'servers
				       :set 'name server
				       :returning 'id)
			 :single)))
      (loop
	 for ch in channels
	 do
	   (let ((ch-id (postmodern:query
			 (:select 'id :from 'channels
				  :where (:and (:= 'name ch)
					       (:= 'server-id server-id)))
			 :single)))
	     (when (not ch-id)
	       (postmodern:query (:insert-into 'channels
					       :set 'name ch
					       'server-id server-id))))))))
    

;; Query functions

(defun get-all-channels ()
  "Get all channels with server names instead of ids."
  (with-db
    (postmodern:with-column-writers ('server 'server-id)
      (postmodern:query-dao
       'channels
       (:order-by (:select 'channels.* (:as 'servers.name 'server)
			   :from 'channels
			   :left-join 'servers
			   :on (:= 'channels.server-id 'servers.id))
		  'servers.name 'channels.name)))))

(defun channel-exists-p (server-name channel-name)
  "Get all channels with server names instead of ids."
  (with-db
    (postmodern:query (:select t
			       :from 'channels
			       :left-join 'servers
			       :on (:= 'channels.server-id 'servers.id)
			       :where (:and (:= 'servers.name '$1)
					    (:= 'channels.name '$2)))
		      server-name channel-name :single)))

(defun date-to-pg (timestamp)
  "Convert date to postgresql format."
  (local-time:format-timestring nil
                                timestamp
                                :timezone local-time:+utc-zone+))

(defvar *default-log-limit* 80)

(defun get-log-records (&key server
                          channel
                          date-from
                          date-to
                          nick
			  host
                          message-type
                          message
                          to-id
                          from-id
                          limit
                          sort)
  "Get events DAO list from database."
  (unless limit
    (setf limit *default-log-limit*))
  (with-db
    (postmodern:query-dao
     'event
     (:limit
      (:order-by 
       (:select '* :from 'events
                :where (:and
			(:raw (if server
				  (postmodern:sql (:= 'server server))
				  "'t'"))
			(:raw (if channel
				  (postmodern:sql (:= 'channel channel))
				  "'t'"))
			(:raw (if date-from
				  (postmodern:sql (:>= 'date (date-to-pg
                                                              date-from)))
				  "'t'"))
			(:raw (if date-to
				  (postmodern:sql (:<= 'date (date-to-pg
                                                              date-to)))
				  "'t'"))
			(:raw (if from-id
				  (postmodern:sql (:>= 'id from-id))
				  "'t'"))
			(:raw (if to-id
				  (postmodern:sql (:<= 'id to-id))
				  "'t'"))
			(:raw (if nick
				  (postmodern:sql (:= 'nick nick))
				  "'t'"))
			(:raw (if host
				  (postmodern:sql
				   (:ilike 'host (concatenate
						  'string "%" host "%")))
				  "'t'"))
			(:raw (if message
				  (postmodern:sql
				   (:ilike 'message (concatenate 'string
								 "%"
								 message
								 "%")))
				  "'t'"))
			(:raw (if message-type
				  (postmodern:sql
				   (:= 'message-type message-type))
				  "'t'"))))
       (:raw (if (eq sort 'asc)
                 (postmodern:sql 'date)
                 (postmodern:sql (:desc 'date)))))
      limit))))

(defun get-id-before-date (&key server
                             channel
                             date-from
                             date-to
                             nick
                             host
                             message)
  "Get id of first event before DATE-TO."
  (with-db
    (postmodern:query
     (:limit
      (:order-by
       (:select 'id :from 'events
                :where (:and
                        (:raw (if server
                                  (postmodern:sql (:= 'server server))
                                  "'t'"))
                        (:raw (if channel
                                  (postmodern:sql (:= 'channel channel))
                                  "'t'"))
                        (:raw (if date-from
                                  (postmodern:sql (:>= 'date (date-to-pg
                                                              date-from)))
                                  "'t'"))
                        (:raw (if date-to
                                  (postmodern:sql (:< 'date (date-to-pg
                                                             date-to)))
                                  "'t'"))
                        (:raw (if nick
                                  (postmodern:sql (:= 'nick nick))
                                  "'t'"))
                        (:raw (if host
                                  (postmodern:sql
                                   (:ilike 'host (concatenate
                                                  'string "%" host "%")))
                                  "'t'"))
                        (:raw (if message
                                  (postmodern:sql
                                   (:ilike 'message (concatenate 'string
                                                                 "%"
                                                                 message
                                                                 "%")))
                                  "'t'"))))
       (:desc 'date))
      1) :single)))

(defun get-nicks (&key server channel)
  "Get all user nicknames for channel."
  (with-db
    (postmodern:query (:order-by (:select 'nick :distinct
                                          :from 'events
                                          :where (:and (:= 'server '$1)
                                                       (:= 'channel '$2)))
                                 'nick)
                      server channel :column)))

(defun get-context-start (&key server channel event-id size)
  "Get first event for for in-context display."
  (with-db
    (car
     (last
      (postmodern:query
       (:limit (:order-by (:select 'id
				   :from 'events
				   :where (:and (:= 'server '$1)
						(:= 'channel '$2)
						(:<= 'id '$3)))
			  (:desc 'id))
	       size)
       server channel event-id :column)))))

(defun get-db-message-stats (&key server channel)
  "Get PRIVMSG and NOTICE messages count for every nick on channel."
  (with-db
    (postmodern:query
     (:select 'nick
              (:as (:count (:case
                               ((:in 'message-type
                                     (:set "PRIVMSG" "NOTICE" "ACTION"))
                                'id)))
                   'count)
              :from 'events
              :where (:and (:= 'server '$1)
                           (:= 'channel '$2))
              :group-by 'nick)
     server channel)))
