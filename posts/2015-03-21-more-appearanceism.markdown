---
layout: post
title:  Are student evaluations getting more superficial?
date:   2015-04-21
author: Ben Schmidt
categories: 
 - Rate My Professor
...

Just a day after launching this blog (RSS feed, by the way, is now up
[here](http://bookworm.benschmidt.org/atom.xml))
I came across a perfect little example question to look
at. [The Guardian ran an article about appearance on teaching evaluations](http://www.theguardian.com/higher-education-network/2015/apr/21/students-dont-rate-me-on-my-appearance-but-on-my-teaching?CMP=share_btn_tw)
that touches on some issues that my
[Rate My Professor Bookworm](http://benschmidt.org/profGender) can answer, with a few new interactive charts.

<!--more-->

I found two interesting things about the article. One was the
observation that "the issue is not just limited to female academics:"
men and women *both* complain about assessments of their appearance 
inappropriately showing up in their evaluations.

This, for me, was one of the most interesting features of the Rate My Professor set: while 
terms about *intellect* and *behavior* were often highly gendered, appearance often showed a relatively even split. (As always, [many caveats apply to this data](http://benschmidt.org/2015/02/06/rate-my-professor/), but I believe it's the best available for really fine-grained analysis of language in teaching reviews.)

Here's the chart for "hottest/hot." You can change the terms yourself, or use the buttons below to find a few other words. This is just the same as my [original site](http://benschmidt.org/profGender), so further exploration is better done there.

## Rate My Professor: word usage by gender about physical appearance.
```{.bookworm default="SVG" filters="word:textArray" width=700 height=400 id=appearances}
	{ "database": "RMP",
	"plotType": "pointchart",
	"search_limits": {"department__id":{"$lte":25},"word": ["hottest","hot"]},
	"aesthetic": {  "x": "WordsPerMillion",  "y": "department" , "color":"gender" }
	}
```
<button onclick="fixDirectors('hottest,hot')">Change to 'hottest/hot'</button>
<button onclick="fixDirectors('sexy')">Change to 'sexy'</button>
<button onclick="fixDirectors('hair,haircut,face')">Change to 'hair/haircut/face'</button>
<button onclick="fixDirectors('clothes,clothing,shirt,pants,skirt,dress,dresses,dressed,blouses,blouse,wore,wears')">Change to a whole bunch of clothing words</button>
<script>
function fixDirectors(word) {
var worm = d3.select("#appearances").node().__bookworm__
//Once we have the bookworm element, we can change the search limits by operating on the query.
worm.query.search_limits.word = word.replace(", ",",").split(",");
worm.updatePlot()
d3.select("#appearances").selectAll("input").node().value = word
}
</script>

Most of these show an even split, or show more men than women being described in terms of their clothing. (I'm open to the idea that maybe I'm just putting the wrong words in here. At some point I'll go through the more than a million queries I've received on the main browser so far and see what people are actually searching for: but the beauty of this blog format is that you can find a counterexample here and post it in the comments. I don't doubt that there are **some** areas of physical description where female professors suffer far more than men.)

That's not to say that it doesn't matter--the structures of contemporary society are such that a student making claims about the appearance of a younger female professor is undermining her authority (or at least *attempting* to undermine authority), while the same claim about an older male professor carries no charge. A profession in which complaints about appearance are made equally about genders may very well have worse effects for women than men.

So that's why the following claim in the Guardian article is even more important.

> There was a consensus among those who responded that these issues
>  had intensified over the past few years. As students begin to think
>  of themselves as consumers, rather than learners, their attitudes
>  towards those who teach them is changing.

I didn't release a linechart browser of the Rate My Professor data over time because, well, it's kind of a boring question. But with [this new blogging software](http://bookworm.benschmidt.org/posts/2015-03-24-new-formats.html), it's easy enough to do. And at least in Rate My Professor, mentions of physical attractiveness are on the decline.

## Rate My Professor: relative usage of "sexy", "sexiest", "hot", and "hottest" over time
```{.bookworm default="SVG" filters="word:textArray" width=600 height=500 id=overTime}
{
    "database": "RMP",
    "plotType": "linechart",
    "search_limits": {
        "word": ["sexy","sexiest","hot","hottest"],
        "date_year": {
            "$gte": 2005,
            "$lte": 2014
        }
    },
    "aesthetic": {
        "y": "WordsPerMillion",
        "x": "date_year",
        "color": "gender"
    },
    "method": "return_json",
    "counttype": ["WordsPerMillion"],
    "groups": ["date_year", "gender"]
}
```
<button onclick="fixDirectors2('hair,haircut,face')">Change to 'hair/haircut/face'</button>
<button onclick="fixDirectors2('clothes,clothing,shirt,pants,skirt,dress,dresses,dressed,blouses,blouse,wore,wears')">Change to a whole bunch of clothing words</button>
<script>
function fixDirectors2(word) {
var worm = d3.select("#overTime").node().__bookworm__
//Once we have the bookworm element, we can change the search limits by operating on the query.
worm.query.search_limits.word = word.replace(", ",",").split(",");
worm.updatePlot()
d3.select("#overTime").selectAll("input").node().value = word
}
</script>

## But...

But, one of the problems with the Rate My Professors data is that it's a changing entity and not representative of course evaluations as a whole.

Back in the mid-2000s, at the peak of its usage, RateMyProfessors.com was a pioneer in encouraging students to rate faculty on their physical attributes. (No official evaluation site I know of has yet adopted the chili pepper on their forms). It's possible that has decreased over time, but that the criteria it drummed up have migrated into official teaching only in the last ten years.

Still, I think as with many complaints about how kids nowadays just aren't what they used to be, there's reason to be skeptical. Maybe in recent **decades** things have gotten worse. But years? I wouldn't bet on it.
