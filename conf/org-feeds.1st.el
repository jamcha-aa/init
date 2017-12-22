(setq org-feed-default-template "\n* %h-%U\n %description\n %a")
(setq org-feed-retrieval-method 'wget)
(setq org-feed-alist
      '(("POSTD" "http://postd.cc/feed/"
         "~/org/feeds.org" "POSTD")
        ("GIGAZINE" "http://gigazine.net/index.php?/news/rss_2.0/"
         "~/org/feeds.org" "GIGAZINE")))
