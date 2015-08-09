;;; Database interaction

(in-package #:zoglog)

(defvar *db-connection* nil)
(defvar *default-database* "zoglog")
(defvar *database-user* "zoglog")
(defvar *database-password* "zoglog")

(defun connect-db (&optional (database *default-database*)
                     (database-user *database-user*)  
                     (database-password *database-password*)
                     (host "localhost"))
  "Start the database connection. Reconnects if there is an
unconnected database in *database* which matches the database
parameter in the function, it will be reconnected. Returns boolean on
whether the global *database* is now connected."
  (unless postmodern:*database*
    (setf postmodern:*database*
          (postmodern:connect database database-user database-password
                              host :pooled-p t))))

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

(defun init-db ()
  "Create tables and indexes if they don't exist."
  (unless (table-exists "events")
    (postmodern:execute (postmodern:dao-table-definition 'event))
    (postmodern:query (:create-index 'events_serv_chan_date_idx :on "events"
				     :fields 'server 'channel 'date))
    (postmodern:query (:create-index 'events_nick_idx :on "events"
				     :fields 'nick)))
  (unless (table-exists "servers")
    (postmodern:execute (postmodern:dao-table-definition 'server))
    (postmodern:query (:create-unique-index 'servers_name_idx :on "servers"
					    :fields 'name)))
  (unless (table-exists "channels")
    (postmodern:create-table 'channels)))

(defun disconnect-db ()
  "Disconnect from database."
  (postmodern:clear-connection-pool)
  (postmodern:disconnect-toplevel))

(defun update-db-channels (server channels)
  "Save server and channels to database if needed."
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
					     'server-id server-id)))))))
    
