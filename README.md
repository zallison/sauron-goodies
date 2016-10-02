# sauron-goodies
Goodies for the Sauron logger for emacs

# Description
A collection of useful packages and functions for use with the [Sauron](https://github.com/djcb/sauron) logging system for emacs.

# Packages
* sauron-tail
	* tails a command or a file selectively logging events
	* filter functions accept a LINE and return:
		* t - insert the line
		* nil - don't insert the line
		* plist - insert a log message using the information in the plist
	* filter functions can customize log messages, add callbacks, or fire external events
	* helper functions to generate filters:
		* sauron-tail-match-p
		* sauron-tail-match
		* sauron-tail-highlight

* sauron-mu4e
	* notify on email that is "interesting"
	* adds callback to open message in mu4e
	* formatter function available to custom log message

* sauron-zeroconf
	* monitors zeroconf (avahi/bonjour) announcements and logs them to sauron
	* used to monitor network device availablity
	* add callback to open detailed information about the service
	* if there is a possible link to be made it will be added to the log
	* filter function can return t to log the event, nil to not log the event.

# Examples

* sauron-mu4e
	* Just load it!
	```elisp
	(require 'sauron-mu4e "path/to/sauron-mu4e.el")
	(add-to-list 'sauron-modules 'sauron-mu4e)
	(sauron-start)
	```

	* optionally add your own formatter
	```elisp
	(defun my-formatter (msg)
		...
		logmsg)
	(setq sauron-mu4e-formatter-function #'my-formatter)
	```

* sauron-zeroconf
	* Just load it!
	```elisp
	(require 'sauron-mu4e "path/to/sauron-zeroconf.el")
	(add-to-list 'sauron-modules 'sauron-zeroconf)
	(sauron-start)
	```
	* optionally add a filter
	```elisp
	(defun my-sauron-zeroconf-filter (process)
	  (let ((host (zeroconf-service-host process))
			(type (zeroconf-service-type process)))
		(cond
		  ;; The Apple TV is spammy
		  ((string-match-p host "Apple-TV.local") nil)
		  (t t))))
	(setq sauron-zeroconf-filter-function #'my-sauron-zeroconf-filter
	```
* sauron-tail
	* Load it, then tail it when to do
	```elisp
	(require 'sauron-tail "path/to/sauron-tail.el")
	(add-to-list 'sauron-modules 'sauron-tail)
	(sauron-start)
	```
   * tail /var/log/messages
	   ```elisp
	   (sauron-tail-file "/var/log/messages")
	   ```
   * tail /var/log/messages giving a better prefix
	   ```elisp
	   (sauron-tail-file "/var/log/messages" :prefix "messages")
	   ```
   * tail /var/log/messages and only log lines matching "NetworkManager"
	   ```elisp
	   (sauron-tail-file "/var/log/messages"
						 :filter (sauron-tail-match-p "NetworkManager"))
	   ```
   * tail the command journalctl, match lines matching NetworkManager, only printing the match
	   ```elisp
	   (sauron-tail-command "journalctl -f"
							:filter (sauron-tail-match "NetworkManager.*"))
	   ```
   * tail the command journalctl, match lines matching NetworkManager, only printing adress
	   ```elisp
	   (sauron-tail-command "journalctl -f"
							:filter (sauron-tail-match "NetworkManager.*\\(address.*\\)"))
	   ```

   * tail the command journalctl, match lines matching NetworkManager, highlighting the  adress
	   ```elisp
	   (sauron-tail-command "journalctl -f"
							:filter (sauron-tail-highlight "NetworkManager.*\\(address.*\\)"))
	   ```
   * tail the command journalctl, include all lines, and highlighting the  adress
	   ```elisp
	   (sauron-tail-command "journalctl -f"
							:filter (sauron-tail-highlight "NetworkManager.*\\(address.*\\)")
							:all)
	   ```
