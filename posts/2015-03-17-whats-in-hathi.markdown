---
layout: post
title:  "Hathi Trust: Metadata Bookworm"
date:   2015-03-17 22:21:00
author: Ben Schmidt
categories: 
 - bookworm
 - explorations
 - HathiTrust
...

This post does two things:

1. Gives an overview of the metadata in the HathiTrust Bookworm, for future reference and debugging.
2. Test drives out a new blogging framework for embedding Bookworm plots using Jekyll. The stylesheets are still a little funky. For now, plots need to be clicked to be load; but once they are, they're fully dynamic as far as Bookworm allows.

<!--more-->

First a note: these are not yet standalone charts, because the bookworm API is considerably more 

<script>
//The juice for that is here--a global function that given the id of an svg, blows it up to an appropriate size, sets some listeners for click events, and pulls in the data on click.
bookwormArea = function(id,query,width,height) {
	width = width || 900
	height = height || 500
	var parent = d3.select("#"+ id).style("width",width).style("height",height).style("background","white")//.style("background-opacity",.1)
	parent.append("text").attr("transform","translate(25," + height/2 + ")").text("click to load plot").style("opacity",1).style("font-size","96").style("font-color","red");
	var strung = JSON.stringify(query).match(/.{1,80}/g);
	strung.forEach(function(d,i) {
		parent.append("text").text(d).attr("y",35 + 14*(i+1)).style("font-family","monospace")
	})
	parent.on("click",function() {
	parent.on("click","").style("background","").style("opacity",1).selectAll("text").remove()
	var worm = new Bookworm(query);
	worm.silent=true
	worm(parent)
})
}
</script>

## Hathi Metadata



Let's take a first pass at seeing what sort of stuff is in the metadata for the HTRC bookworm, using the [code that Colleen Fallaw wrote and Peter Organisciak and I have modified a bit.]()

First off: what are the top libraries in the collection? 

<svg id="ex1">
</svg>
<script>
bookwormArea("ex1",{ "database": "catalogworm", "plotType": "barchart", "method":"return_json", "words_collation": "Case_Sensitive", "search_limits": { }, "aesthetic": { "x" : "TextCount","y":"htsource" } })
</script>

It's a little indulgent, but a streamgraph isn't the worst way to look at how these libraries change over time.

The important thing to keep in mind here is that several major contributors (Harvard, Wisconsin, Princeton) drop out entirely in 1922. This will have real effects on the contents of the database. There are also several interesting effects in the public domain era; around 1972, contributions from the two remaining largest libraries, Michigan and California, decrease significantly. Since 1980, Penn State plays a surprisingly (to me) important role.

There are also three abberational spikes: 1800, 1900, and 1817. The first two have to do with libraries assigning 1700 and 1800 as default dates. There may be some ways to construct corpora that avoid the worst effects of this (by avoiding certain libraries and genres). The third is the Congressional serial set.

<svg id="streamLibraries">
</svg>
<script>
bookwormArea("streamLibraries", { "database": "catalogworm", "plotType": "streamgraph", "method":"return_json", "words_collation": "Case_Sensitive", "search_limits": { "decade":{"$gte":1750}}, "aesthetic": { "x" : "date_year","y":"TextCount","fill":"htsource" } })
</script>

##Authorship

Using the "authors" fields highlights some of the important changes. Before 1922, the major authors are canonical figures; after, they are agencies of the US government. Note that both of these charts show overall volume; so while there are roughly as many GAO documents in the 1980s as there are Shakespeare plays in the 1880s, those GAO docs will be far more important for search results. 

<svg id="streamAuthors">
</svg>
<script>
bookwormArea("streamAuthors", { "database": "catalogworm", "plotType": "streamgraph", "method":"return_json", "words_collation": "Case_Sensitive", "search_limits": { "mainauthor__id":{"$lte":15,"$gte":2},"decade":{"$gte":1750}}, "aesthetic": { "x" : "decade","y":"TextCount","fill":"mainauthor" } })
</script>


##Genre fields

Next up: what about the fields that we have?

<svg id="fields">
</svg>
<script>
bookwormArea("fields",{ "database": "catalogworm", "plotType": "barchart", "method":"return_json", "words_collation": "Case_Sensitive", "search_limits": { }, "aesthetic": { "x" : "TextCount","y":"lc_classes" }})
</script>

There are too many of these to plot easily in a streamgraph, so we'll use a basic heatmap. (Another advantage is that although I haven't enable click-to-view on streamgraphs yet, you can click on any cell to see some example samples.) Hover over to see the values.

<svg id="fieldmap">
</svg>
<script>
bookwormArea("fieldmap",{ "database": "catalogworm", "plotType": "heatmap", "method":"return_json", "words_collation": "Case_Sensitive", "search_limits": { "decade":{"$gte":1700}, "lc_classes__id":{"$gte":2}}, "aesthetic": { "x":"decade","color" : "TextCount","y":"lc_classes" }})
</script>

This tells us mostly that there is a lot of literature produced before 1920. That's not especially surprising, so I'll take advantage of the new crosstabulation features from the API to sum across columns and colorize by percentage of texts: so we can see what are the most common fields for each decade.

<svg id="fieldmap2">
</svg>
<script>
bookwormArea("fieldmap2",{ "database": "catalogworm", "plotType": "heatmap", "method":"return_json", "words_collation": "Case_Sensitive", "search_limits": { "decade":{"$gte":1700}, "lc_classes__id":{"$gte":2}}, "aesthetic": { "x":"decade","color" : "TextPercent","y":"*lc_classes" }},900,500)
</script>

It's also interesting to note that with 29 books, the most represented field of publication for the 2010s is "Military Science." 


But more usefully, this turns out to be a fairly effective way to find serials. Most problematic serial publications will be concentrated in a particular year and shelf location. So we can identify them by making one much longer chart and clicking on any of the red bands which represent dominance by a particular category in that year. Usually, that shows an aberration.

<svg id="fieldmap3">
</svg>
<script>
bookwormArea("fieldmap3",{ "database": "catalogworm", "plotType": "heatmap", "method":"return_json", "words_collation": "Case_Sensitive", "search_limits": { "decade":{"$gte":1700}, "lc_classes__id":{"$gte":2}}, "aesthetic": { "x":"date_year","color" : "TextPercent","y":"*lc_classes" }},2400,500)
</script>


Some that immediately spring out:

1. 1767, Science: dozens of copies of "The Nautical almanac and astronomical ephemeris for the year .."
2. 1817, Law: The US Congressional Serial Set. (Also Political Science--between the two fields, this quintuples the number of books in the corpus for the year 1817).
3. 1731, General Works: The "Gentleman's Magazine"
4. 1923, Agriculture: "Soil Survey"

This might suggest we may want to revisit the issue of the "serial killer" algorithm from the Google Ngram viewer. But in fact, we already have information on format easily accessible in the metadata.

<svg id="formats">
</svg>
<script>
bookwormArea("formats",{ "database": "catalogworm", "plotType": "heatmap", "method":"return_json", "words_collation": "Case_Sensitive", "search_limits": { "decade":{"$gte":1700}, "lc_classes__id":{"$gte":2}}, "aesthetic": { "x":"date_year","color" : "TextPercent","y":"*format" }},1400,500)
</script>

If we restrict the sample set to books in the query, the problem vanishes.

<svg id="fieldmap4">
</svg>
<script>
bookwormArea("fieldmap4",{ "database": "catalogworm", "plotType": "heatmap", "method":"return_json", "words_collation": "Case_Sensitive", "search_limits": {"format":"book", "decade":{"$gte":1700,"$lte":2009}, "lc_classes__id":{"$gte":2}}, "aesthetic": { "x":"date_year","color" : "TextPercent","y":"*lc_classes" }},2400,500)
</script>
