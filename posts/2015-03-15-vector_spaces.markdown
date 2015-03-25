---
layout: post
title:  "Vector Spaces: an interactive explanation"
date:   2015-03-15 22:21:00
author: Ben Schmidt
categories: teaching
---

The fundamental modeling tool behind much text analysis is the "vector space" model.
The underlying idea is that any document can be broken into an
arbitrary set of words, and we can plot it in space. Here's an explanation with some interactive charts to
help show how they work.

<!--more-->

A note on reading. These are memory-chomping interactive charts, so they don't load with the page. You have to click on them (or the grey box under the code that describes them) and then they'll load.

We saw this in class last week in R, but I thought it might be helpful
to have a dynamic version so that you can see just what this rotation
looks like.

The classic document set in machine learning is the Federalist papers,
so we'll start with them. (Next week, we'll be reading some of the
more substantial findings that move beyond simple vector space
models).

The vector space model is easiest to understand in two dimensions, so
we'll start there. This is a plot showing each of the federalist
papers (by number) and colorized by author. The x-axis shows how many
times each paper uses the word "Congress"; the y-axis shows how many
times it uses the word "President".

Far off to the right you can see
[Federalist no. 40](http://www.constitution.org/fed/federa40.htm),
which uses the word "Congress" 17 times. (Although it is not in reference to the proposed congress under the constitution, but instead
to the convention that wrote that the constitution itself).


<div>
<svg id="ex1">
</svg>
</div>

-----

<script>

bookwormArea("ex1",{
    "database": "federalist",
    "plotType": "vectorspace",
    "method": "return_json",
    "words_collation": "Case_Sensitive",
    "search_limits": {
        "word": ["President", "Congress"]
    },
    "aesthetic": {
        "variable": "WordCount",
        "dimensions": "*unigram",
        "label": "fedNumber"
    },
    "counttype": ["WordCount"],
    "groups": ["*unigram", "author", "fedNumber"],
    "weights": {
        "Congress": {
            "x": 1,
            "y": 0
        },
        "President": {
            "x": 0,
            "y": 1
        }
    }
},550,450)


</script>

# Multiple words on the same axis.

It's easy to see one way to improve this: instead of just displaying
one word on each axis, we can add some together. 

<div>
<svg id="ex2">
</svg>
</div>

<script>

bookwormArea("ex2",{
    "database": "federalist",
    "plotType": "vectorspace",
    "method": "return_json",
    "words_collation": "Case_Sensitive",
    "search_limits": {
        "word": ["President", "Congress","legislature","senate","house","executive","presidency","legislative"]
    },
    "aesthetic": {
        "variable": "WordCount",
        "dimensions": "*unigram",
        "label": "fedNumber"
    },
    "counttype": ["WordCount"],
    "groups": ["*unigram", "author", "fedNumber"],
    "weights": {
        "Congress": {
            "x": 1,
            "y": 0
        },"legislature":{"x":1,"y":0},"senate":{"x":1,"y":0},"house":{"x":1,"y":0},"legislative":{"x":1,"y":0},
        "President": {
            "x": 0,
            "y": 1
        },"executive":{"x":0,"y":1},"presidency":{"x":0,"y":1}
    }
},550,450)


</script>


The red dots below give the *weights* for each direction. You can drag
them to make any individual element represent something in
particular. Try, for instance, dragging "Senate" all the way to the left: what happens?

## Stopwords and authorship.

These topical words can tell you things about the materials covered:
it's probably obvious, though, that this doesn't seem to be telling
you much about the authors (represented here by colors). 

It turns out that so called "stop words" are the best at investigating
authorship. You can see the most famous example below: papers by
Alexander Hamilton tend use the word "upon" quite often, while papers by James
Madison very rarely do.

```bookworm
{
    "database": "federalist",
    "plotType": "vectorspace",
    "words_collation": "Case_Sensitive",
    "search_limits": {
        "word": ["on","upon"]
    },
    "aesthetic": {
        "variable": "WordCount",
        "dimensions": "*unigram",
        "color": "author",
        "label": "fedNumber"
    },
    "weights": {
        "on": {"x": 1,"y": 0},
        "upon": {"x": 0,  "y": 1}
    }
}	
```


A longer list of stopwords gives you some more room for manipulation. 

<div>
<svg id="ex8">
</svg>
</div>


-----

<script>

bookwormArea("ex8",{
    "database": "federalist",
    "plotType": "vectorspace",
    "method": "return_json",
    "words_collation": "Case_Sensitive",
    "search_limits": {
        "word": ["on","upon","as","our"]
    },
    "aesthetic": {
        "variable": "WordCount",
        "dimensions": "*unigram",
        "color": "author",
        "label": "fedNumber"
    },
    "counttype": ["WordCount"],
    "groups": ["*unigram", "author", "fedNumber"],
    "weights": {
        "on": {
            "x": 1,
            "y": 0
		},        "upon": {
            "x": 0,
            "y": 1
        },"as":{
            "x": 0,
            "y": -.3
        },"our" :{
            "x": 0,
            "y": .5
        }
    }
},550,450)
</script>

You can manipulate this chart at
[the site linked to right here.](http://benschmidt.org/D3/#%7B%22database%22%3A%22federalist%22%2C%22plotType%22%3A%22vectorspace%22%2C%22method%22%3A%22return_json%22%2C%22words_collation%22%3A%22Case_Sensitive%22%2C%22search_limits%22%3A%7B%22word%22%3A%5B%22on%22%2C%22upon%22%2C%22as%22%2C%22our%22%5D%7D%2C%22aesthetic%22%3A%7B%22variable%22%3A%22WordCount%22%2C%22dimensions%22%3A%22*unigram%22%2C%22color%22%3A%22author%22%2C%22label%22%3A%22fedNumber%22%7D%2C%22counttype%22%3A%5B%22WordCount%22%5D%2C%22groups%22%3A%5B%22*unigram%22%2C%22author%22%2C%22fedNumber%22%5D%7D). Try
dragging until you get the points into a configuration like this: it
gives a pretty good discrimination in two dimensions between the two authors.

![Image of an optimal loading.](/bestMatch.png)

And add in some more elements until you get a really complicated
version going.

There are some obvious extensions that we'll talk about more in class:
most importantly, *scaling* the results so that different lengths
don't overly impact the size, and a few techniques for automatically
finding a good set of weights, instead of specifying them
manually. (Although we watched John Tukey do it in class, it's not
actually something that's become widespread).



[Continuing online](https://github.com/HumanitiesDataAnalysis/HDA15/blob/master/Problem_Sets/8-Vector_Spaces.Rmd)
