---
layout: post
title:  Writing up text analysis for immediate interaction <em>and</em> long-term persistence.
date:   2015-04-20
author: Ben Schmidt
categories: 
 - bookworm
 - pandoc
 - HathiTrust
...

Though more and more outside groups are starting to adopt Bookworm for their own
projects, I haven't yet written quite as much as I'd like about how it should work.
This blog is attempt to rectify that, and begin to explain how a combination of **blogging
software**, **interactive textual visualizations**, and a
**exploratory data analysis API for bag-of-words models** can make it possible to quickly and
usefully share texts through a Bookworm installation.

But this is a difficult task: so for a first post, I want to introduce some elements of the API and talk about why I think a platform like this is valuable for exploring a large collection of texts visually and quantitatively.

<!--more-->

This is going to be a somewhat technical post (I'll try to keep the really technical stuff in the footnotes), but let me start by showing what we're headed.

> Note: even if you don't care about the infrastructure and code that makes up much of this post, you may want to scroll down for a couple of the interactives: language use by popular directors, and a chart that shows the vocabulary used to describe liberal and conservative faculty members on their teaching evaluations.

I want to start thinking about how to reconcile two somewhat distinct traditions in Digital Humanities text-oriented projects.^[Obviously I would never say that these are the **only** two traditions in DH, just the two that I myself sometimes feel torn between. Bookworm itself occupies a space in a larger ecosystem of DH projects targeted at exploring textual collections like Voyant and Wordseer.] One is computationally inclined, shies in the direction of writing articles, performs sometimes elaborate computations, and views reproducible research (where possible) as a matter of sharing code on Github.^[This is a tradition with deep roots in Humanities Computing in literature departments; leading current practitioners are people like Franco Moretti, Matt Jockers, Ted Underwood, and Andrew Piper.] The other is more interested in presenting individual artifacts through robust platforms like Omeka for images or resilient sites for displaying TEI archives.^[Again, a few names at random: Sheila Brennan, Sharon Leon, Tom Scheinfeldt, much of the work at the UVA Scholars Lab.] The latter rarely tries to make broader arguments about its aggregate sources; the former rarely makes its underlying texts accessible except as a few examples. ^[One notable counterexample is the [DFR topic browser that Andrew Goldstone created as part of his collaboration with Ted Underwood on topic modeling literary scholarship.]](http://agoldst.github.io/dfr-browser/demo/).

I am more a member of the former camp--for whom both sustainability and broad accessibility are awkward questions to be dealt with later. I've been more interested in the amazing new things we can do with million-document collections.

But the time has come to think through, a little more, how to keep some of these elements around for the long term. The big question this platform struggles with is much broader: how can a platform for textual visualization and criticism be:

1. Richly interactive for years through access to terabytes of information on a non-cachable [deep web](http://en.wikipedia.org/wiki/Deep_Web) backend, so long as the computers are plugged in and given minimal maintenance.
2. Capable of persisting for decades even **off the original server** with its data backend, through locally cached copies on places like the Internet Archive and Github.
3. As little reliant on the distributed architecture of the Internet era as possible. Large portions of the web would break if Google or JQuery started hosting their minified javascripts at a slightly different URL.

So those are the goals: the platform consists of three parts.

1. Something established: the Bookworm platform for text analysis and visualization, currently used as a research and discovery platform by organizations like the [Hathi Trust Research Center](http://bookworm.htrc.illinois.edu/) and the [Yale University Libraries](http://bookworm.library.yale.edu/collections/vogue/).
2. Something less established but you may have seen: my D3-reimagining of the Bookworm front-end to enable many new forms of visualizations, like [animated maps of the State of the Union](http://www.theatlantic.com/features/archive/2015/01/mapping-the-state-of-the-union/384576/) or dotplots of [differences in vocabulary used in course evaluations of male and female professors](http://benschmidt.org/profGender).
3. Something entirely new: A [hakyll](http://jaspervdj.be/hakyll/)-based blogging platform to easily share investigations on a Bookworm server. This may not be for everyone, though I hope some other places may want to adopt it. In other cases, rather than hakyll it might make sense to use a local CMS of any sort with embedded Bookworm elements. The basic principles will be the same.^[I'm using Hakyll chiefly because it means I get to easily use Pandoc for transformations from Markdown, and because it means I can distribute straightforward binaries for the Bookworm elements. The Pandoc filters are easily extricable from the overall Hakyll framework.]

Last fall, I released a bookworm for
[looking at trends in language use across 80,000 movies and TV episodes](http://movies.benschmidt.org/). That contained a single interactive chart without much text for
exploring a vast database.

What I'm going to be talking about here is a different way for the
people who want to maintain a Bookworm installation like that to
explore their data. Instead of presenting just the data visualization 
as a blank canvas, I'm going to be talking about a way to write up
what's in that sort of database in a more narrative, but still
interactive way. So for example: if I want to make a point about how much different
directors use the word "love," I can custom-write a query and present
you with a barchart to look at it. 

### Relative usage of the word "love" in the films of several prolific directors 
```{.bookworm width=600 height=350 filters="word:textArray" id="directors"}
{"database":"movies",
"plotType":"barchart",
"search_limits":{
"medium":["movie"],
"word":["love"],
"director":["Allen, Woody","Ford, John (I)","Eastwood, Clint (I)","Huston, John (I)","Herzog, Werner (I)","Spielberg, Steven","Scorsese, Martin","Wilder, Billy","Preminger, Otto","Altman, Robert (I)","Hawks, Howard","Soderbergh, Steven","Nichols, Mike (I)","Burton, Tim (I)","Coppola, Francis Ford","Sirk, Douglas","Nolan, Christopher (I)","Bigelow, Katherine (I)"]
},
"aesthetic":{
  "x":"WordsPerMillion","y":"director"
 }
 }
```

But unlike most inline graphics on a blog, this is powerfully
interactive; you can click on any bar to see all of the underlying
movies, for example.

Plus, since I've added a text box, you can search for anything you want. And a little javascript on my end adds more options. For instance: who do you think uses the word "death" the most? You can go back up to the text box and run an entirely different search: or just click on of the buttons below.

<button onclick="fixDirectors('death')">Change to 'death'</button><button onclick="fixDirectors('money')">Change to 'money'</button>
<script>
// This sort of special code isn't the easiest thing in the world, but it's also not the hardest. If you're looking in here, check out these comments to see how it works.
//first, obviously, I just created some HTML elements inside my markdown and bound them to a function. 

function fixDirectors(word) {
// I used the id attribute on my code block to name the bookworm block `directors`; the
// bookworm element is hard-bound to the node so we can get at it programatically.
var worm = d3.select("#directors").node().__bookworm__
//Once we have the bookworm element, we can change the search limits by operating on the query.
worm.query.search_limits.word[0] = word;
// Then we just update the plot: it already knows what SVG element it's bound to,
// so the transitions are clean.
worm.updatePlot()
// But we have to mop up that text block to change the word.
d3.select("#directors").selectAll("input").node().value = word
}
</script>


This is a fun trick: but it's also critically important for better critical engagement of research in the humanities. **Facilitating this kind of open-ended interaction allows  investigation on claims we make.** Digital Humanists sometimes take to mean "reproducible research" as putting code on GitHub. That's great and important; but it's also a huge barrier to pass, and often an unnecessary one. You can learn a lot about a source by exposing different elements for research interactively, and that means that domain experts who will never in a million years run our LAMP-stack architecture can just put some queries in and see what the results are; they can also click through to the texts to see if the words mean the things I say they do.

## The Bookworm Visualization API.

I've already described Bookworm's core API
[elsewhere.](http://bookworm-project.github.io/Docs/API.html),
but let me just briefly give a simple example. Let's say that you want to know how
many times each of the authors of the Federalist Papers uses the word
"upon." If you have a bookworm instance that includes all the
Federalist Papers (which is easy: it's the
[default test suite](https://github.com/bmschmidt/federalist-bookworm)), 
you would express that this way: limit your search to the word "upon,"
specify that you want responses grouped by the name of the author,
and then ask for a counttype of "wordcount."

```json
{ "database": "federalist",
  "search_limits": {"word": ["upon"]},
  "groups":["author"],
  "counttype":["WordCount"],
  "method":"json"}
```

Deployed against a Bookworm endpoint, you'll get the following results back:

```json
{"DISPUTED": [3], "MADISON": [7], "HAMILTON": [374], "COAUTHORED": [2], "JAY": [1]} 
```

That is: Hamilton uses the word "upon" 374 times: the other two authors use
about 10 times at most.

### Stacking Visualization on the Basic API

It's ridiculous, nowadays, to display that sort of information as a
table. While the API returns numbers, we also want visualization to be
a first-class result. The Bookworm visualization API used here is essentially a lightweight
set of additions to that core API to include a vocabulary for
making a number of charts usefuly specifically for text analysis in
the presence of metadata.

It borrows from the "graphics of graphics" described by Leland Wilkinson and popularized
by Hadley Wickham, while also including all of the rich filtering,
algorithmic, and search tools from Bookworm's own API.
It adds two new core elements (along with several chart-specific ones I'll get to later):

1. `plotType`, by which you specify one of several plots you wish to
create, and
2. `aesthetic`, in which you create an aesthetic mapping of variables.

The API format for a chart just substitutes in those two new fields.^[You'll see that we no longer need to specify the `groups` and
`counttype` field, because the Javascript API implementation automatically determines that
`WordCount` is a grouping element and `author` is a categorical
variable. (Note that I say "Javascript API implementation:" though some
charts require more specific calls, this should be generally
deployable on a variety of non-javascript platforms, particularly the `ggvis` package for R).] Here is the API specification to request a barplot of that same data.

```json
{ "database": "federalist",
 "plotType": "barchart",
 "search_limits": {
  "word": ["upon"]},
 "aesthetic": {  "x": "WordCount",  "y": "author" }
}
```

How do you embed such a chart? In any website where you have the
Bookworm D3 library and associated libraries (most notably the core D3
library) loaded, you can just load up the query and deploy it against
a Bookworm item.^[Note that this will only work when the webserver and
the bookworm server are the same: for server load reasons, I haven't
yet implemented support for a format like JSONP]

#### Embedding responsive Bookworm visualization with simple javascript.
```javascript

// Define a query
var query = { "database": "federalist",
 "plotType": "barchart",
 "search_limits": {
  "word": ["upon"]},
 "aesthetic": {  "x": "WordCount",  "y": "author" }
}

// Initialize the Bookworm with a query
var newBookworm = Bookworm(query)

// Create a D3 selection with an SVG
var svg = d3.select("body").append("svg");

// Stamp the SVG with the chart that you want.
newBookworm(svg);
```

If you want to build a robust, powerful visualization and you know 
some javascript, this provides a relatively easy way to do so without even knowing D3 at all.^[Although D3 must be loaded, the only actually D3 you have to do is a single line like `d3.select("#my-svg")`; exactly the same selector format as in jQuery, just slightly more verbose.]

You can just dynamically manipulate the bookworm element's `query`
object to create dynamic charts that are fully updateable with things like the
button above.

#### Embedded visualizations without javascript coding.

In some cases, even that will be too hard: the digital humanities tend
to lean on python and R as their default languages, and javascript
(particularly seen through D3's worldview) can be difficult. I myself
don't want to have to fire up javascript just to make a simple chart.
Since the API is so expressive, it's easy to just write these
visualizatons as special blocks inside markdown.
The current website
uses a hakyll site (the haskell jekyll equivalent) in which you can
simply drop in your queries as markdown blocks.^[At heart this is just
Pandoc--if anyone out there wants to do this as HTML without Hakyll, I'm happy to help figure out 
the Haskell code or binaries and HTML template necessary to compile visualizations straight from Markdown to
HTML, though you'll still probably have to be self-hosting.] Here's what that block
looks like as Markdown, with the height and width specified in advance:

	```{.bookworm width=400 height=300}
	{ "database": "federalist",
	"plotType": "barchart",
	"search_limits": {
	"word": ["upon"]},
	"aesthetic": {  "x": "WordCount",  "y": "author" }
	}
	```

And here's the navigable element it automatically produces in the
browser, giving access to both the raw API call (so that others can
see the exact definition you're working with), an interactive SVG
which you can click to see and read individual documents (so that
domain experts who are **not** coding experts can explore your data).

```{.bookworm default="code" width=400 height=300}
	{ "database": "federalist",
	"plotType": "barchart",
	"search_limits": {
	"word": ["upon"]},
	"aesthetic": {  "x": "WordCount",  "y": "author" }
	}
```

One of the most important elements of a Bookworm is that it frequently
needs to be *interactive*. You don't just want one chart: you want the
potential to create millions with clean transitions between them.
That's possible in this through the addition of
**filters** to your markdown writeup.^[This is a wrapper around the core Bookworm JS
functionality of `bookworm.addFilters()` that enables simple
deployment of all sorts of restrictions through HTML GUI elements.] Filters are HTML elements bound to the
Bookworm view that turn it into an interactive element on the
page. See this footnote for a fuller description. [^1]

[^1]: To add a filter, I'm currently experiment with a css-derived
      key-value scheme where you specify the element and the type of filter you
      want.`textArray`, here, is just a fancy name for a standard input box.
	  For categorical variables,
      you'd want to be able to limit from a dropdown menu.

           ```{.bookworm filters="word:textArray" width=600 height=300}
              { "database": "federalist",
				"plotType": "barchart",
				"search_limits": {
				"word": ["upon"]},
				"aesthetic": {  "x": "WordCount",  "y": "author" }
				} 
	       ```
	(You also see that I'm using 'width' and 'height' to manually specify
    the size of the HTML element.)

The final element looks like this. If you type elements into the
box, the bars will automatically update to reflect whatever word
you've chosen.

```{.bookworm default="code" filters="word:textArray" width=600 height=300}
	{ "database": "federalist",
	"plotType": "barchart",
	"search_limits": {
	"word": ["upon"]},
	"aesthetic": {  "x": "WordCount",  "y": "author" }
	}
```

Bar charts are a classic, and useful, way to plot: but they are only
one of the many visualizations built in to Bookworm. To keep it on the federalist papers, here's what
that would look like. (Remember, you can toggle over to "code" if you
want to see the exact parameters of the call.



## Relative use of Congress (x-axis) by Federalist Paper Number (y-axis)
```{.bookworm default="SVG" filters="word:textArray" width=500 height=800}
	{ "database": "federalist",
	"plotType": "pointchart",
	"search_limits": {
	"word": ["Congress"]},
	"aesthetic": {  "x": "WordsPerMillion",  "y": "title" , "color":"author" }
	}
```

Extremely different charts are easily specified by this. The most popular
single browser so far probably has been my
[RateMyProfessor browser](http://benschmidt.org/profGender), using
which uses paired dots to represent usage rates in reviews of male and
female professors. The create call for this is relatively simple. Just click on "interactive SVG" to see the actual chart.

## Rate My Professor: Language use by gender and department.
```{.bookworm default="code" filters="word:textArray" width=700 height=400}
	{ "database": "RMP",
	"plotType": "pointchart",
	"search_limits": {"department__id":{"$lte":20},
	"word": ["funny"]},
	"aesthetic": {  "x": "WordsPerMillion",  "y": "department" , "color":"gender" }
	}
```

But a powerful API means that there are many, many more ways to look at this. Just a line of difference makes this a comparison of Democrats and Republicans:

## Rate My Professor: Language use by political party and department
```{.bookworm default="code" filters="word:textArray"}
	{ "database": "RMP",
	"plotType": "pointchart",
	"search_limits": {"department__id":{"$lte":20},"party":["D","R"],
	"word": ["bow tie"]},
	"aesthetic": {  "x": "WordsPerMillion",  "y": "department" , "color":"party" }
	}
```

A fully-featured interactive chart-creation engine, I'm convinced, would be a less-than-ideal outcome. (Not for lack of trying: I've had versions percolating for two or three years now.) It would be easy to add a dropdown to shift from one of these to the other; but ridiculous to allow a dropdown where the dots could be dozens of different schools. This chart kind of lets you know that Valencia college has a lot of online education: but it's far from a good way to demonstrate it.


## A terrible chart possible under the API: word usage in reviews at the top 20 schools.
```{.bookworm default="code" filters="word:textArray"}
	{ "database": "RMP",
	"plotType": "pointchart",
	"search_limits": {"department__id":{"$lte":20},"school__id":{"$lte":20},
	"word": ["online"]},
	"aesthetic": {  "x": "WordsPerMillion",  "y": "department" , "color":"school" }
	}
```

A sufficiently powerful API will always allow this sort of junk to be created. I increasingly think that some human thought **should** go into deciding what a useful version of a visualization like this should be. Part of persuading people about conclusions on large data sets should be giving them tools to see if your interpretation holds up under scrutiny.

## Sustainability

If you're coming to this post from the world of data visualization,
rather than Digital Humanities, you may notice that by having a number
of separate svg items stack atop each other I'm violating the current
spirit of the times, which prefers to have a single SVG that is updated inline in
response to scroll events.

1. For this form, narrative text comes first and foremost. Bookworm is
great at showing charts; only text is any good at explaining the most
interesting ones.
2. Pragmatically: I haven't yet mastered the entire art of event listeners that will
be necessary to make this work.
3. Most importantly, though, I want a format that can at least gesture
towards **preservability**.

These interactive graphics are served off a custom API using a running MySQL instance that can be terabytes large. There's good reason to be hopeful that, when a Bookworm instance is useful, we can find ways to make it persist for years. But problems happen--when both Erez and I moved institutions in 2013 in a period when the Bookworm project was not receiving any immediate grant funding, it took about 2 or 3 months to restore the largest installation.


In addition to these full measures, I want to take half-measures as well as a failsafe plan. The current state of the Internet Archive cannot cache all the dark content on one of our servers. But it can cache a webpage, with narrative, with a bunch of static PNGs. This site uses a static page-generation framework to make it much easier to cache versions of the page. It also includes a number of scripts for automatically rendering PNGs of the state for every image in the text and embedding it directly in, so that at least the graphical element of the page can remain for as long as web archiving practices persists. These are far less powerful and useful than the interactive images: but at least they mean that a Bookworm page like this continues to meet the basic threshold for sustainability of, say, a pdf on archive.org **even as** it allows much greater interactivity.

PNGs aren't the be-all and end-all. If anyone cares about the Internet in 30 years as much as we care about 80s Nintendo games now, there's good reason to think that arbitrary HTML5/javascript will still run **if the various off-site links still work**. I'm assuming the underlying database is gone: but by caching the data from the calls saved explicitly specified, we can ensure that at least some of the most interesting statistics on any given blog post persist. It also means, after a little bit more work to the Bookworm-D3 plugin, that we'll be able to .

In some cases, we may even be able to cache the entirety of every search an individual might run. The [State of the Union Interactives](http://www.theatlantic.com/features/archive/2015/01/the-language-of-the-state-of-the-union/384575/) Mitch Fraas and I built for the Atlantic adopt this strategy: Chris Barna figured out a great system where the front-end Bookworm infrastructure is hosted on the Atlantic site, and he simply cached every possible query including search results that might come through the Atlantic's servers and pulled it from my local State of the Union server. As a result, that Atlantic site now has no Bookworm backend at all, though it successfully uses the API. As a general strategy, this will work **only when** there aren't fantastically rich calls being generated. (Anything with an open-entry text box is unlikely to work.)

But even for open-text Bookworms like my [Rate My Professor browser](http://benschmidt.org/profGender), there's some hope for a useful long-term persistence. Thanks to query caching, I now have a list of the tens of thousands of most popular search terms for faculty members. Even if I choose to retire the completely interactive portion of the web site, I can still keep a static one running that will include almost all the entries that actually human beings tend to search for.


In essence, this mimics a sort of process of peer review--there is open interaction and comment for the first few years of a project, but then it gets locked into a more fixed form for the long term unless someone (the author, the press, the library) keeps investing energy to keep it up-to-date. The question of who will store it remains: but while it's easy to get all Mad Max about the dismal future of digital sources, I tend to be on the more optimistic side. But maybe I'm wrong!




