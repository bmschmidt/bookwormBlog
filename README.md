
This is a robust blogging template for sharing the results of a bookworm installation.

Rather than just throw up a few links on twitter, or struggle with screenshotting them, this lets you write posts directly in markdown and
structure bookworm charts as bookworm API calls for full documentability, extensibility, and interactivity. Just by including a Bookworm codeblock in your markdown like this:

   ```bookworm width=500 height=1200
   {"search_limits":{"word"},"aesthetic":{"x":"year","y":"WordsPerMillion"},"plotType":"linechart"}
   ```

You'll get an svg panel running a live version of the chart. This is made easy through the Bookworm D3.js API, which extends the normal bookworm API to include elements from the grammar of graphics.


It will work fine as another blog as well not focused on bookworm, but I'd really think of it primarily as a way to engage in public, exploratory data analysis in a way where your conclusions are immediately shareable and manipulable by anyone out there.

I hope to set up two: one on my personal server, at 

I'm using Bootstrap on top of hakyll--which is a slightly odd combination--because hakyll gives the easiest access to full pandoc templates (and is much faster than jekyll to boot), while bootstrap means that it will be much easier to drop in user-oriented controls where useful (including things like changing the word searched for.).

Future plans
------------

1. I'd really like to also spin up a phantomjs workflow which will cache local png versions of each chart as well, solving some of the important persistence issues that face database-oriented services like this one. To keep this fully in Haskell, rather than use a Make architecture we'll probably call phantomjs directly from the haskell interpreter on encountering a code block.


LICENSE
-------


Anything in pages/ or posts/ is copyright Ben Schmidt, all rights reserved.
The material in js/ and css/ comes under a variety of licenses.
Materials specific to the *creation of this site:* including the contents of templates/ site.hs, are under the MIT license.


Acknowledgements.
===================

Many of those elements are taken from [Stephen Diehl's Hakyll Bootstrap template.](https://github.com/sdiehl/hakyll-bootstrap); he should be acknowledged in the supporting apparatus of any derivative works.
