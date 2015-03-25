---
layout: post
title:  "Dissertation Timespans"
date:   2015-03-29 22:21:00
author: Ben Schmidt
categories: 
 - bookworm
 - Jstor
...

I wrote a few posts back in 2013 about the changing time spans covered in history dissertations. Some of that data has since played a role in a fairly bitter exchange in the *American Historical Review* about the relative merits of short-term and long-term history, including a number of forays into other data sets. Anticipating this and wanting a nice collection of texts for teaching, I recently got a big collection of reviews in the *American Historical Review* (and a literary studies journal, to boot). All participants now seem to agree that the time span of history dissertations has been increasing since around the mid-1970s. Can this data tell us anything more about just why that happened?

<!--more-->

Here's a chart that simply shows, broken up into decades, the raw number of reviews falling into the three major lengths of time covered.  

```bookworm
{
"database": "jstor",
"plotType": "streamgraph",
"method": "return_json",
"search_limits": {
 "journaltitle":["The American Historical Review"],
 "span":{"$gt":0},
},
"aesthetic": {
 "y": "TextCount",
 "fill": "*generalBin",
 "x": "decade"
}
}
```


It's kind of crazy to compare the historical profession from before 1975 to that after. We all still keep some reflexive idea that the field "professionalized" in the late 19th century with the creation of the AHA, the major departments, and the like: but that explosion in book reviews is just one of several things that should remind us that the 1970s were at least as important in turning history from an avocation into an occupation.

```bookworm
{
"database": "jstor",
"plotType": "streamgraph",
"method": "return_json",
"search_limits": {
 "journaltitle":["The American Historical Review"],
 "span":{"$gt":0},
 "publication_year":{"$gte":1970}
},
"aesthetic": {
 "y": "TextCount",
 "fill": "*generalBin",
 "x": "publication_year"
}
}
```

