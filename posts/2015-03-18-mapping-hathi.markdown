---
layout: post
title:  "Mapping the open Hathi Corpus"
date:   2015-03-18 22:21:00
author: Ben Schmidt
categories: HathiTrust
...

Adding geography to the Hathi Trust catalog may give some interesting results. Here I'll just show a couple very basic maps that give a sense of the broad overview of places to be in the HathiTrust Bookworm, as derived by the [Bookworm-geolocator plugin](http://github.com/bmschmidt/bookworm-geolocator).

You may see errors: that's all for the good, because this is a preliminary draft. At the repo there's a public spreadsheet that can be edited to change where we should assume a book printed in, say "Springfield," should be dropped on the map. (There's not yet a 'certainty' field, but there may be at some point).


<!--more-->


As with all work like this, the point is not whether you can find a few niggling errors: it's whether the overall sweep tells you anything useful about the distribution of print.

For the time being, the answer to that may well be "no," because we're suffering from another of the frequent problems plaguing text analysis: the relatively small number of errors that are wrong are very wrong indeed.

```bookworm
{
    "database": "catalogworm",
    "plotType": "map",
    "projection": "azimuthal",
    "method": "return_json",
    "search_limits": {
        "date_year": {
            "$gte": 1700,
            "$lte": 1922
        }
    },
    "aesthetic": {
        "size": "TotalTexts",
        "point": "publication_place_geo",
        "label": "publication_place_toponymName"
    }
}
```

Here's a second example: colorized by the percentage of texts that are in English. (The asterisk syntax lets us drop the requirement that books be in English from the comparison set.) For this one, we'll just look at Europe between 1700 and 1922, animated.

```bookworm
{
    "database": "catalogworm",
    "plotType": "map",
    "projection": "europe",
    "method": "return_json",
    "search_limits": {
        "*languages": ["English"],
        "date_year": {"$gte": 1700, "$lte": 1922}
    },
    "aesthetic": {
        "color": "TextPercent",
        "size": "TotalTexts",
        "point": "publication_place_geo",
        "time": "date_year",
        "label": "publication_place_toponymName"
    },
    "scaleType": "linear"
}
```


