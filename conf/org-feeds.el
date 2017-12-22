;http://d.hatena.ne.jp/tamura70/20100225/org
(defun org-feed-parse-rdf-feed (buffer)
  "Parse BUFFER for RDF feed entries.
Returns a list of entries, with each entry a property list,
containing the properties `:guid' and `:item-full-text'."
  (let (entries beg end item guid entry)
    (with-current-buffer buffer
      (widen)
      (goto-char (point-min))
      (while (re-search-forward "<item[> ]" nil t)
	(setq beg (point)
	      end (and (re-search-forward "</item>" nil t)
		       (match-beginning 0)))
	(setq item (buffer-substring beg end)
	      guid (if (string-match "<link\\>.*?>\\(.*?\\)</link>" item)
		       (org-match-string-no-properties 1 item)))
	(setq entry (list :guid guid :item-full-text item))
	(push entry entries)
	(widen)
	(goto-char end))
      (nreverse entries))))

(setq org-feed-default-template "\n* %h-%U\n %description\n %a")
;(setq org-feed-retrieval-method 'wget)
(setq org-feed-retrieve-method 'curl)
(setq org-feed-alist nil)
(add-to-list 'org-feed-alist
      '("POSTD" "http://postd.cc/feed/"
        "~/org/feeds.org" "POSTD"
        :parse-feed org-feed-parse-rdf-feed))
(add-to-list 'org-feed-alist
      '("GIGAZINE" "http://gigazine.net/index.php?/news/rss_2.0/"
         "~/org/feeds.org" "GIGAZINE"
         :parse-feed org-feed-parse-rdf-feed))
