---
layout: post
title:  "Visualizing the Bookworm API"
date:   2015-03-28 22:21:00
author: Ben Schmidt
categories: 
 - bookworm
 - pandoc
 - HathiTrust
...




```json
{"question":"Will this format nicely?",
"answer":"in a way, but less than I'd hope.",
"but":"Maybe not?"}
```


```bookworm
{
 "database": "federalist",
 "plotType": "barchart",
 "search_limits": {
  "word": ["test"]
 },
 "aesthetic": {
  "x": "WordsPerMillion",
  "y": "author"
 }
}
```
