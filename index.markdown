---
layout: default
title: Your New Jekyll Site
---

# umx

umx is an R package relentlessly focussed on making [Structural Equation Modeling](http://en.wikipedia.org/wiki/Structural_equation_modeling) in [OpenMx](http://openmx.psyc.virginia.edu) **quicker**, *easier*, and **more powerful**. 

It's easy to learn and easy to get print-ready results from models. Sweet huh?

### Install it now and run a model!

You can install the package in [R](http://cran.r-project.org) as follows:

```S
{% highlight R %}

install.packages("devtools")
library("devtools")
install_github("umx", username = "tbates")
library("umx")
?umx
# on windows you might need
# install_github("umx", username = "tbates", args = "--no-multiarch")
# On the newest version of devtools, this will work
# install_github("tbates/umx")
{% endhighlight %}
```

<div id="home">
  <h1>Blog Posts</h1>
  <ul class="posts">
    {% for post in site.posts %}
      <li><span>{{ post.date | date_to_string }}</span> &raquo; <a href="{{ post.url }}">{{ post.title }}</a></li>
    {% endfor %}
  </ul>
</div>

Test

View the Project on [GitHub: tbates/umx](https://github.com/tbates/umx)

Feel free to use, and improve: Log suggestions here using the Github comments, wiki, or git.

### Contributors

* [Tim Bates](tim.bates@ed.ac.uk)
* Michael Culbertson (modification index [code](http://openmx.psyc.virginia.edu/thread/1019) (based on functions in [John Fox's](http://socserv.mcmaster.ca/jfox/Misc/sem/SEM-paper.pdf) [SEM](http://cran.r-project.org/web/packages/sem) package)
* Ryne Estabrook ([code](http://openmx.psyc.virginia.edu/thread/718) to Standardize RAM models