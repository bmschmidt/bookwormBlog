---
layout: post
title: Story Time.
date:   2015-05-11
author: Ben Schmidt
categories: 
 - bookworm
 - screenworm
...



Here are some interactives I've made in preparation for my talk at the
Literary Lab at Stanford on Tuesday on plot arcs in television shows
based on underlying language.

This is sort of in lieu of a handout for the talk, so some elements may not make
much sense if you aren't in the room.

These extend two more detailed posts on my Sapping Attention blog; one giving a
[methodology for topics on TV shows](http://sappingattention.blogspot.com/2014/12/typical-tv-episodes-visualizing-topics.html),
and a second describing
[using principal components as a way to visualize archetypal plots as movements in multidimensional space](http://sappingattention.blogspot.com/2014/12/fundamental-plot-arcs-seen-through.html).

For both of these, I felt that I was bumping up against the
possibilities of writing on blogger; the underlying data is much, much
richer and well worth exploring. So this post is where I'm gathering
together some of the transformations I used there.

<!--more-->

To review: it is possible to see patterns in the movement of
individual words in texts across screen time (movies or TV shows cut
into equal sized parts). These patterns, in fact, are very strong. So
here's an example that you can explore yourself: I'm showing "love
you," (as in, "I love you"), but you can use the box below to search
for **any** word or phrase. You'll quickly see just how prevalent they
are; let me know in the comments or some other way if you find any
interesting ones.

## Interactive choice of word by sixth of movie.
```{.bookworm2 height="300" width="600" filters="word:textArray"}
{   "database": "screenworm",
    "plotType": "linechart",
    "search_limits": {
		"movieYear":{"$gte":1830,"$lte":2022},
		"word":["love you"],
		"6th":[1,2,3,4,5,6]
    },"words_collation":"Case_Sensitive",
    "aesthetic": {
        "x": "6th",
        "y" : "WordsPerMillion","color":"medium"
    }}
```


## By decade on x axis, trends are less strong.
```{.bookworm2 height=300 width=600 filters="word:textArray"}
{   "database": "screenworm",
    "plotType": "linechart",
    "search_limits": {
		"movieYear":{"$gte":1930,"$lte":2015},
		"word":["love you"],
		"6th":[1,2,3,4,5,6]
    },"words_collation":"Case_Sensitive",
    "aesthetic": {
        "x": "decade",
        "y" : "WordsPerMillion","color":"medium"
    }}
```



## Topic Model

I've also run a topic model (now somewhat improved from the one used
for my posts in December.)

Since I've got a really rich database under here, it's possible to look at these trends individually by TV show. (The topic numbers are not completely correct here).




## View the trajectory of any topic in any TV show
```{.bookworm2 height=300 width=600 filters="*topic_label:dropdown;*TV_show:dropdown"}
{   "database": "screenworm",
    "plotType": "linechart",
    "search_limits": {
		"movieYear":{"$gte":1830,"$lte":2022},
		"6th":[1,2,3,4,5,6],
		"*TV_show":["Seinfeld"],
		"*topic_label":["call phone Hello number called message calling"]
    },"words_collation":"Case_Sensitive",
    "aesthetic": {
        "x": "6th",
        "y" : "WordsPerMillion"
    }}
```




But the aggregate results are if anything, more interesting.

The topics uncovered by topic models show these trends, if anything,
more strongly.^[I can imagine a phenomenon where short topics take up the beginning and end.]
Here's a dropdown to explore any of the various included topics by twelfth of a movie. 
<span color="red">Note: my new subdivision algorithm only assigns a twelfth to episodes over 24 minutes long (under, and the smallest chunk is a sixth), so this doesn't include most sitcoms.</span>

## See how topics are distributed across the duration of scripts by medium.
```{.bookworm2 height=300 width=600 filters="*topic_label:dropdown"}
{   "database": "screenworm",
    "plotType": "linechart",
    "search_limits": {
		"movieYear":{"$gte":1830,"$lte":2022},
		"12th":[1,2,3,4,5,6,7,8,9,10,11,12],
		"*topic_label":["call phone Hello number called message calling"]
    },"words_collation":"Case_Sensitive",
    "aesthetic": {
        "x": "12th",
        "y" : "WordsPerMillion","color":"medium"
    }}
```

One of the appeals of TV shows and movies is that they have far, far better metadata than any other type of fiction I've seen. For example, they are tagged by genre. For the most part, these patterns hold for all genres (showing here just the 4 most common; I tag each one as belonging solely to the *first* genre in IMDB, because for some popular movie there are very long lists of tags.)

## Trends of topics in screen time (by twelfth of film/show)
```{.bookworm2 height=300 width=600 filters="*topic_label:dropdown"}
{   "database": "screenworm",
    "plotType": "linechart",
    "search_limits": {
		"movieYear":{"$gte":1830,"$lte":2022},"medium":["TV show"],"primary_genre__id":[1,2,3,4,5],
		"12th":[1,2,3,4,5,6,7,8,9,10,11,12],
		"*topic_label":["sorry life wasn wrong fault done love"]
    },"words_collation":"Case_Sensitive",
    "aesthetic": {
        "x": "12th",
        "y" : "WordsPerMillion","color":"primary_genre"
    }}
```


But there are some exceptions; a topic we might call "politeness" is equally concentrated
from about the second act of everything but comedies, but increases
over the course of comedic shows.^[I divide by sixth instead of twelfth so that 24-minute sitcoms will appear.]Again, if you find interesting ones, do let me know.

# "Politeness topic" differs in behavior between comedies and other genres (screen time in 6ths, TV shows only)
```{.bookworm2 height=300 width=600 filters="*topic_label:dropdown"}
{   "database": "screenworm",
    "plotType": "linechart",
    "search_limits": {
		"movieYear":{"$gte":1830,"$lte":2022},"medium":["TV show"],"primary_genre__id":[1,2,3,4,5],
		"6th":[1,2,3,4,5,6],
		"*topic_label":["sorry Thank please Please Sorry talk Excuse"]
    },"words_collation":"Case_Sensitive",
    "aesthetic": {
        "x": "6th",
        "y" : "WordsPerMillion",
		"color":"primary_genre"
    }}
```


# "Politeness topic" *doesn't* differ in movies
```{.bookworm2 height=300 width=600 filters="*topic_label:dropdown" id="differentGenres1"}
    {   "database": "screenworm",
    "plotType": "linechart",
    "search_limits": {
		"movieYear":{"$gte":1830,"$lte":2022},"medium":["movie"],"primary_genre__id":[1,2,3,4,5],
		"6th":[1,2,3,4,5,6],
		"*topic_label":["sorry Thank please Please Sorry talk Excuse"]
    },"words_collation":"Case_Sensitive",
    "aesthetic": {
        "x": "6th",
        "y" : "WordsPerMillion","color":"primary_genre"
    }}
```

<button onclick="fixDirectors('bomb gas power building air water work')">Change to 'bomb gas power building air water work'</button>
<script>
function fixDirectors(word) {
var worm = d3.select("#differentGenres1").node().__bookworm__
worm.query.search_limits.topic_label = word.replace(", ",",").split(",");
worm.updatePlot()
d3.select("#differentGenres1").selectAll("select").node().value = word
}
</script>




## Trends of topics in screen time, movies only, by time period (by twelfth of film/show)
```{.bookworm2 height=300 width=600 filters="*topic_label:dropdown"}
{   "database": "screenworm",
    "plotType": "linechart",
    "search_limits": {
		"movieYear":{"$gte":1830,"$lte":2022},"medium":["TV show"],
		"12th":[1,2,3,4,5,6,7,8,9,10,11,12],"medium":["movie"],
		"*topic_label":["sorry life wasn wrong fault done love"]
    },"words_collation":"Case_Sensitive",
    "aesthetic": {
        "x": "12th",
        "y" : "WordsPerMillion","color":"period"
    }}
```




One can also divide up any show into chunks. These are the charts I presented as static images for my first post. The alignment of the legend still isn't right; I need to take a bounding box on it, I guess.

## View the trajectory of any topic in any TV show; y-axis is total number of words
```{.bookworm2 height=300 width=900 filters="*TV_show:dropdown"}
{   "database": "screenworm",
    "plotType": "streamgraph",
    "search_limits": {
		"movieYear":{"$gte":1830,"$lte":2022},
		"6th":[1,2,3,4,5,6],
		"*TV_show":["Seinfeld"]
    },
    "aesthetic": {
    	"fill":"*topic_label",
        "x": "6th",
        "y" : "WordCount"
    }}
```

# Weighted vectors

```{.bookworm2 height=600 width=600 filters="topic:textArray"}
{   "database": "screenworm",
    "plotType": "vectorspace",
    "method": "return_json",
    "words_collation": "Case_Sensitive",
    "search_limits": {"12th":[1,2,3,4,5,6,7,8,9,10,11,12],	"primary_genre__id":[1,2,3,4,5],
		"topic":[1,2,3,4,5,6,7,8,9,10]
    },
    "aesthetic": {
        "variable": "WordsPerMillion",
        "dimensions": "*topic_label",
        "label": "12th","color":"primary_genre"
    }}
```


The higher-order stuff from my second post is also possible on here,
including sharing rotatable principal components analysis. Click
through to the query on the tab below to see the rotations I'm using
(x is the first principal component, and y is the second; or you can
drag around any arbitrary set of elements.

```{.bookworm2 height=600 width=600}
{   "database": "screenworm",
    "plotType": "vectorspace",
    "method": "return_json",
    "words_collation": "Case_Sensitive",
    "search_limits": {"12th":[1,2,3,4,5,6,7,8,9,10,11,12],	"primary_genre__id":[1,2,3,4,5]
    },
    "aesthetic": {
        "variable": "WordsPerMillion",
        "dimensions": "*topic_label",
        "label": "12th","color":"primary_genre"
    },
"weights":{
 "dead body death die died alive ghost": {
 "x": 0.044491,
"y": 0.013857 
},
"animals animal bear food wild hunting lion": {
 "x": -0.018399,
"y": -0.019428 
},
"team Thank challenge vote game Chef win": {
 "x": 0.074495,
"y": 0.15647 
},
"train plane hotel bus flight ticket please": {
 "x": -0.028279,
"y": 0.090951 
},
"look picture face pictures Look eyes photo": {
 "x": -0.034293,
"y": -0.021619 
},
"Madame de Monsieur French dear course evening": {
 "x": -0.019179,
"y": 0.0066835 
},
"boat ship sea fish Captain island water": {
 "x": -0.0042115,
"y": 0.025112 
},
"huh big old look sure fun guess": {
 "x": -0.05122,
"y": 0.031226 
},
"Earth space planet universe stars years energy": {
 "x": 0.023666,
"y": 0.096243 
},
"minutes hour half hours day days ago": {
 "x": -0.023301,
"y": 0.038853 
},
"dear darling Mother Mama Papa Uncle Hello": {
 "x": 0.0065562,
"y": 0.037022 
},
"Please hurt please scared calm away stay": {
 "x": 0.069761,
"y": -0.049246 
},
"computer system security tape video code camera": {
 "x": -0.033573,
"y": -0.09085 
},
"York city City Street town West place": {
 "x": -0.065658,
"y": 0.14006 
},
"Dr. believe case death saw evidence Professor": {
 "x": -0.017697,
"y": -0.010185 
},
"bomb gas power building air water work": {
 "x": 0.015357,
"y": -0.081111 
},
"music play sing band rock singing playing": {
 "x": -0.040747,
"y": 0.029498 
},
"clean smell water wash use bath bathroom": {
 "x": -0.084825,
"y": -0.024774 
},
"drink wine beer drunk bottle drinking glass": {
 "x": -0.039097,
"y": -0.063645 
},
"ancient world years century th history city": {
 "x": -0.034927,
"y": 0.10835 
},
"girl girls boy look name beautiful pretty": {
 "x": -0.074516,
"y": -0.010039 
},
"door open Open key room inside locked": {
 "x": 0.041803,
"y": -0.055928 
},
"Let's move Move hand hands Hold Give": {
 "x": 0.06581,
"y": 0.023136 
},
"friend friends name meet talk met mine": {
 "x": -0.034123,
"y": -0.061503 
},
"night sleep bed wake morning sleeping asleep": {
 "x": -0.015909,
"y": -0.02054 
},
"power magic evil world blood demon witch": {
 "x": 0.025394,
"y": 0.046333 
},
"truth believe lie true lying telling story": {
 "x": 0.15098,
"y": -0.094267 
},
"years world land water sea ago life": {
 "x": -0.0121,
"y": 0.18605 
},
"gun shot shoot kill guns fire Let's": {
 "x": 0.13387,
"y": -0.014026 
},
"sir plane minutes pilot fly air flying": {
 "x": 0.022565,
"y": 0.04109 
},
"bad news luck Thanks sure won look": {
 "x": 0.028365,
"y": 0.0075733 
},
"sir Colonel Captain men General Lieutenant Sergeant": {
 "x": -0.0226,
"y": 0.026576 
},
"sex women woman men love sexual girls": {
 "x": -0.066804,
"y": -0.069847 
},
"jump feet top high side bridge goes": {
 "x": 0.0092217,
"y": -0.029026 
},
"look wear dress wearing shoes clothes suit": {
 "x": -0.079517,
"y": -0.023972 
},
"Master Brother master Let's Chinese Please Kong": {
 "x": -0.010676,
"y": 0.012046 
},
"night tonight dinner tomorrow party Friday week": {
 "x": -0.09979,
"y": 0.0068415 
},
"head cut hand leg arm blood body": {
 "x": -0.031148,
"y": -0.043366 
},
"Dr. blood surgery heart patient hospital doctor": {
 "x": -0.11184,
"y": -0.049953 
},
"read book write letter wrote writing story": {
 "x": -0.041954,
"y": 0.047954 
},
"love heart kiss Love life loved happy": {
 "x": 0.18349,
"y": 0.092356 
},
"game play ball team playing win football": {
 "x": -0.016148,
"y": 0.033472 
},
"ain em ya gotta goin wanna doin": {
 "x": -0.0032505,
"y": 0.042891 
},
"II Iike aII wiII Iove wouId teII": {
 "x": 0.023689,
"y": -0.025351 
},
"married wife wedding husband love marriage woman": {
 "x": 0.025333,
"y": 0.032381 
},
"film movie show TV movies scene play": {
 "x": -0.031257,
"y": 0.17183 
},
"car drive road driving truck cars driver": {
 "x": -0.043034,
"y": 0.004275 
},
"house room place live living apartment move": {
 "x": -0.10696,
"y": 0.018138 
},
"morning day tomorrow today night work late": {
 "x": -0.10104,
"y": 0.12431 
},
"remember seen gone saw looking place look": {
 "x": 0.015414,
"y": -0.080811 
},
"call phone Hello number called message calling": {
 "x": -0.025894,
"y": -0.067336 
},
"Captain ship sir planet Doctor power Commander": {
 "x": 0.029283,
"y": 0.065396 
},
"school class teacher college high kids year": {
 "x": -0.16524,
"y": 0.14518 
},
"President president vote Minister Mr. government United": {
 "x": -0.0093512,
"y": 0.10141 
},
"Let's hell Damn boss bastard bitch Shut": {
 "x": 0.0022336,
"y": -0.039567 
},
"human body world science different work years": {
 "x": 0.010589,
"y": -0.028952 
},
"money business company sell market buy deal": {
 "x": -0.082932,
"y": 0.0078392 
},
"doctor Dr. Doctor hospital sick patient medical": {
 "x": -0.034908,
"y": -0.086854 
},
"war army War Hitler soldiers men country": {
 "x": -0.0045024,
"y": 0.12428 
},
"God Lord Father church pray thou priest": {
 "x": 0.079795,
"y": 0.079217 
},
"ain horse town horses boys men boy": {
 "x": -0.069092,
"y": -0.013026 
},
"sir course Thank dear London quite Ah": {
 "x": -0.049005,
"y": 0.0036803 
},
"Agent agent security FBI team CIA agents": {
 "x": -0.082763,
"y": 0.077114 
},
"years old ago year remember day months": {
 "x": -0.036024,
"y": 0.14976 
},
"country world government power freedom society war": {
 "x": 0.036635,
"y": 0.13872 
},
"heart love eyes life world soul away": {
 "x": 0.12637,
"y": 0.19771 
},
"talk talking Look crazy understand Listen problem": {
 "x": 0.035516,
"y": -0.18963 
},
"Mr. Mrs. Thank sir please Hello name": {
 "x": -0.0095883,
"y": -0.041914 
},
"question answer questions understand point word course": {
 "x": -0.030581,
"y": -0.080382 
},
"life work problem self talk relationship problems": {
 "x": -0.026917,
"y": -0.084699 
},
"work job working office boss day company": {
 "x": -0.16012,
"y": 0.091401 
},
"eat food hungry chicken cook eating dinner": {
 "x": -0.12328,
"y": -0.078636 
},
"police case Chief Police officer arrest cop": {
 "x": -0.013754,
"y": 0.029858 
},
"wanna gotta Look huh talk look Let's": {
 "x": 0.0075663,
"y": -0.071179 
},
"Majesty men Lord sword lord Highness war": {
 "x": 0.022569,
"y": 0.04596 
},
"woman poor won away old child wife": {
 "x": 0.069631,
"y": -0.11132 
},
"Hi Hello Thank Bye Thanks meet bye": {
 "x": -0.046204,
"y": 0.12101 
},
"leave won away stay Please care promise": {
 "x": 0.19638,
"y": -0.056173 
},
"kill killed die dead death life killing": {
 "x": 0.34309,
"y": -0.045452 
},
"court case Mr. trial law lawyer Honor": {
 "x": 0.19698,
"y": -0.04845 
},
"God cool Whoa Look look dude Wow": {
 "x": -0.12614,
"y": 0.02732 
},
"LOOK LET'S LOVE GIVE MR. SORRY SURE": {
 "x": 0.020411,
"y": 0.051088 
},
"Dad Mom dad mom Daddy father kids": {
 "x": -0.0069531,
"y": 0.095722 
},
"plan trust sure won safe work give": {
 "x": 0.072157,
"y": -0.18977 
},
"fucking fuck shit Fuck ass Shit bitch": {
 "x": 0.034948,
"y": -0.14086 
},
"life world live dream day die future": {
 "x": 0.34365,
"y": 0.37649 
},
"stuff look weird pretty sure thinking talking": {
 "x": 0.017984,
"y": -0.1385 
},
"bit mate bloody Mum round look eh": {
 "x": -0.048383,
"y": 0.066707 
},
"father mother brother family daughter sister child": {
 "x": 0.0094385,
"y": 0.012229 
},
"sorry Thank please Please Sorry talk Excuse": {
 "x": 0.022672,
"y": -0.12942 
},
"saw came night took remember wasn couldn": {
 "x": 0.15173,
"y": -0.10617 
},
"money pay give buy paid dollars cash": {
 "x": -0.083894,
"y": -0.035766 
},
"murder body blood killer victim killed crime": {
 "x": -0.1872,
"y": -0.095524 
},
"lt's lt lf ls ln brother give": {
 "x": 0.031145,
"y": -0.031273 
},
"name ago night call card check Mr.": {
 "x": -0.12733,
"y": -0.33326 
},
"sorry life wasn wrong fault done love": {
 "x": 0.43312,
"y": -0.18446 
},
"mind wouldn sure haven isn won course": {
 "x": -0.0010934,
"y": -0.19521 
},
"understand course matter believe wish accept order": {
 "x": 0.043199,
"y": -0.045659 
},
"kid hell ain Look money huh drugs": {
 "x": -0.097339,
"y": -0.17507 
},
"Mm Um hmm sorry um God love": {
 "x": -0.25081,
"y": 0.084348 
} 
}
}
```




```{.bookworm2 height=600 width=600 filters="medium:dropdown"}
{   "database": "screenworm",
    "plotType": "vectorspace",
    "method": "return_json",
    "words_collation": "Case_Sensitive",
    "search_limits": {"12th":[1,2,3,4,5,6,7,8,9,10,11,12],"medium":"movie"
    },
    "aesthetic": {
        "variable": "WordsPerMillion",
        "dimensions": "*topic_label",
        "label": "12th","color":"period"
    },
"weights":{
 "dead body death die died alive ghost": {
 "x": 0.044491,
"y": 0.013857 
},
"animals animal bear food wild hunting lion": {
 "x": -0.018399,
"y": -0.019428 
},
"team Thank challenge vote game Chef win": {
 "x": 0.074495,
"y": 0.15647 
},
"train plane hotel bus flight ticket please": {
 "x": -0.028279,
"y": 0.090951 
},
"look picture face pictures Look eyes photo": {
 "x": -0.034293,
"y": -0.021619 
},
"Madame de Monsieur French dear course evening": {
 "x": -0.019179,
"y": 0.0066835 
},
"boat ship sea fish Captain island water": {
 "x": -0.0042115,
"y": 0.025112 
},
"huh big old look sure fun guess": {
 "x": -0.05122,
"y": 0.031226 
},
"Earth space planet universe stars years energy": {
 "x": 0.023666,
"y": 0.096243 
},
"minutes hour half hours day days ago": {
 "x": -0.023301,
"y": 0.038853 
},
"dear darling Mother Mama Papa Uncle Hello": {
 "x": 0.0065562,
"y": 0.037022 
},
"Please hurt please scared calm away stay": {
 "x": 0.069761,
"y": -0.049246 
},
"computer system security tape video code camera": {
 "x": -0.033573,
"y": -0.09085 
},
"York city City Street town West place": {
 "x": -0.065658,
"y": 0.14006 
},
"Dr. believe case death saw evidence Professor": {
 "x": -0.017697,
"y": -0.010185 
},
"bomb gas power building air water work": {
 "x": 0.015357,
"y": -0.081111 
},
"music play sing band rock singing playing": {
 "x": -0.040747,
"y": 0.029498 
},
"clean smell water wash use bath bathroom": {
 "x": -0.084825,
"y": -0.024774 
},
"drink wine beer drunk bottle drinking glass": {
 "x": -0.039097,
"y": -0.063645 
},
"ancient world years century th history city": {
 "x": -0.034927,
"y": 0.10835 
},
"girl girls boy look name beautiful pretty": {
 "x": -0.074516,
"y": -0.010039 
},
"door open Open key room inside locked": {
 "x": 0.041803,
"y": -0.055928 
},
"Let's move Move hand hands Hold Give": {
 "x": 0.06581,
"y": 0.023136 
},
"friend friends name meet talk met mine": {
 "x": -0.034123,
"y": -0.061503 
},
"night sleep bed wake morning sleeping asleep": {
 "x": -0.015909,
"y": -0.02054 
},
"power magic evil world blood demon witch": {
 "x": 0.025394,
"y": 0.046333 
},
"truth believe lie true lying telling story": {
 "x": 0.15098,
"y": -0.094267 
},
"years world land water sea ago life": {
 "x": -0.0121,
"y": 0.18605 
},
"gun shot shoot kill guns fire Let's": {
 "x": 0.13387,
"y": -0.014026 
},
"sir plane minutes pilot fly air flying": {
 "x": 0.022565,
"y": 0.04109 
},
"bad news luck Thanks sure won look": {
 "x": 0.028365,
"y": 0.0075733 
},
"sir Colonel Captain men General Lieutenant Sergeant": {
 "x": -0.0226,
"y": 0.026576 
},
"sex women woman men love sexual girls": {
 "x": -0.066804,
"y": -0.069847 
},
"jump feet top high side bridge goes": {
 "x": 0.0092217,
"y": -0.029026 
},
"look wear dress wearing shoes clothes suit": {
 "x": -0.079517,
"y": -0.023972 
},
"Master Brother master Let's Chinese Please Kong": {
 "x": -0.010676,
"y": 0.012046 
},
"night tonight dinner tomorrow party Friday week": {
 "x": -0.09979,
"y": 0.0068415 
},
"head cut hand leg arm blood body": {
 "x": -0.031148,
"y": -0.043366 
},
"Dr. blood surgery heart patient hospital doctor": {
 "x": -0.11184,
"y": -0.049953 
},
"read book write letter wrote writing story": {
 "x": -0.041954,
"y": 0.047954 
},
"love heart kiss Love life loved happy": {
 "x": 0.18349,
"y": 0.092356 
},
"game play ball team playing win football": {
 "x": -0.016148,
"y": 0.033472 
},
"ain em ya gotta goin wanna doin": {
 "x": -0.0032505,
"y": 0.042891 
},
"II Iike aII wiII Iove wouId teII": {
 "x": 0.023689,
"y": -0.025351 
},
"married wife wedding husband love marriage woman": {
 "x": 0.025333,
"y": 0.032381 
},
"film movie show TV movies scene play": {
 "x": -0.031257,
"y": 0.17183 
},
"car drive road driving truck cars driver": {
 "x": -0.043034,
"y": 0.004275 
},
"house room place live living apartment move": {
 "x": -0.10696,
"y": 0.018138 
},
"morning day tomorrow today night work late": {
 "x": -0.10104,
"y": 0.12431 
},
"remember seen gone saw looking place look": {
 "x": 0.015414,
"y": -0.080811 
},
"call phone Hello number called message calling": {
 "x": -0.025894,
"y": -0.067336 
},
"Captain ship sir planet Doctor power Commander": {
 "x": 0.029283,
"y": 0.065396 
},
"school class teacher college high kids year": {
 "x": -0.16524,
"y": 0.14518 
},
"President president vote Minister Mr. government United": {
 "x": -0.0093512,
"y": 0.10141 
},
"Let's hell Damn boss bastard bitch Shut": {
 "x": 0.0022336,
"y": -0.039567 
},
"human body world science different work years": {
 "x": 0.010589,
"y": -0.028952 
},
"money business company sell market buy deal": {
 "x": -0.082932,
"y": 0.0078392 
},
"doctor Dr. Doctor hospital sick patient medical": {
 "x": -0.034908,
"y": -0.086854 
},
"war army War Hitler soldiers men country": {
 "x": -0.0045024,
"y": 0.12428 
},
"God Lord Father church pray thou priest": {
 "x": 0.079795,
"y": 0.079217 
},
"ain horse town horses boys men boy": {
 "x": -0.069092,
"y": -0.013026 
},
"sir course Thank dear London quite Ah": {
 "x": -0.049005,
"y": 0.0036803 
},
"Agent agent security FBI team CIA agents": {
 "x": -0.082763,
"y": 0.077114 
},
"years old ago year remember day months": {
 "x": -0.036024,
"y": 0.14976 
},
"country world government power freedom society war": {
 "x": 0.036635,
"y": 0.13872 
},
"heart love eyes life world soul away": {
 "x": 0.12637,
"y": 0.19771 
},
"talk talking Look crazy understand Listen problem": {
 "x": 0.035516,
"y": -0.18963 
},
"Mr. Mrs. Thank sir please Hello name": {
 "x": -0.0095883,
"y": -0.041914 
},
"question answer questions understand point word course": {
 "x": -0.030581,
"y": -0.080382 
},
"life work problem self talk relationship problems": {
 "x": -0.026917,
"y": -0.084699 
},
"work job working office boss day company": {
 "x": -0.16012,
"y": 0.091401 
},
"eat food hungry chicken cook eating dinner": {
 "x": -0.12328,
"y": -0.078636 
},
"police case Chief Police officer arrest cop": {
 "x": -0.013754,
"y": 0.029858 
},
"wanna gotta Look huh talk look Let's": {
 "x": 0.0075663,
"y": -0.071179 
},
"Majesty men Lord sword lord Highness war": {
 "x": 0.022569,
"y": 0.04596 
},
"woman poor won away old child wife": {
 "x": 0.069631,
"y": -0.11132 
},
"Hi Hello Thank Bye Thanks meet bye": {
 "x": -0.046204,
"y": 0.12101 
},
"leave won away stay Please care promise": {
 "x": 0.19638,
"y": -0.056173 
},
"kill killed die dead death life killing": {
 "x": 0.34309,
"y": -0.045452 
},
"court case Mr. trial law lawyer Honor": {
 "x": 0.19698,
"y": -0.04845 
},
"God cool Whoa Look look dude Wow": {
 "x": -0.12614,
"y": 0.02732 
},
"LOOK LET'S LOVE GIVE MR. SORRY SURE": {
 "x": 0.020411,
"y": 0.051088 
},
"Dad Mom dad mom Daddy father kids": {
 "x": -0.0069531,
"y": 0.095722 
},
"plan trust sure won safe work give": {
 "x": 0.072157,
"y": -0.18977 
},
"fucking fuck shit Fuck ass Shit bitch": {
 "x": 0.034948,
"y": -0.14086 
},
"life world live dream day die future": {
 "x": 0.34365,
"y": 0.37649 
},
"stuff look weird pretty sure thinking talking": {
 "x": 0.017984,
"y": -0.1385 
},
"bit mate bloody Mum round look eh": {
 "x": -0.048383,
"y": 0.066707 
},
"father mother brother family daughter sister child": {
 "x": 0.0094385,
"y": 0.012229 
},
"sorry Thank please Please Sorry talk Excuse": {
 "x": 0.022672,
"y": -0.12942 
},
"saw came night took remember wasn couldn": {
 "x": 0.15173,
"y": -0.10617 
},
"money pay give buy paid dollars cash": {
 "x": -0.083894,
"y": -0.035766 
},
"murder body blood killer victim killed crime": {
 "x": -0.1872,
"y": -0.095524 
},
"lt's lt lf ls ln brother give": {
 "x": 0.031145,
"y": -0.031273 
},
"name ago night call card check Mr.": {
 "x": -0.12733,
"y": -0.33326 
},
"sorry life wasn wrong fault done love": {
 "x": 0.43312,
"y": -0.18446 
},
"mind wouldn sure haven isn won course": {
 "x": -0.0010934,
"y": -0.19521 
},
"understand course matter believe wish accept order": {
 "x": 0.043199,
"y": -0.045659 
},
"kid hell ain Look money huh drugs": {
 "x": -0.097339,
"y": -0.17507 
},
"Mm Um hmm sorry um God love": {
 "x": -0.25081,
"y": 0.084348 
} 
}
}
```
