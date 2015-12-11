# DDAR
Development for Discrete Data Analysis with R web site

## Requirements ##

PHP 5.4 or greater
Web server (e.g. Apache/Nginx)

## Overview ##

This is a super-simple custom php/html framework. The layout uses Bootstrap to
form the basis for a responsive web site that will scale seemlessly from
desktop to mobile. A mobile widths, the nav collapses into a drop down suitable
for use on mobile devices.

## Installation ##

This project requires composer to set up project dependencies

[https://getcomposer.org/](https://getcomposer.org/)

```
curl -sS https://getcomposer.org/installer | php
composer install
```

## Site Map ##

This site is made up of the following files:

```
- index.php
- images/
- pages/
	- authors.html
	- book.html
	- home.html
	- materials.html
	- chapters/
		- ch01.html
		- ch01/
- scripts/
- src/
- styles/
```

## Explanation of files ##

`index.php` is the main framework entry point. Think of this as a very basic
controller in the model-view-controller pattern. index.php handles the setup of
the html document, including the navigation and dynamic inclusion of the pages
and chapters.

`images/` directory contains the global site images

`pages/` directory contains the main pages and the chapter pages. The direct
child pages  in this directory can be updated when needed. They are however
hard-coded into the nav in the index.php page. So adding any new pages will
require you to add them to the index.php navigation markup.

`pages/chapters/` directory contains the chapter files. This part of the
navigation is dynamically generated by the index.php file. When listing
chapters, the index.php script scans the 'pages/chapters/' directory and looks
for any files of the form (eg) `ch01.html` and then adds that to the chapter
list in the drop down menu.

`pages/chapters/ch01/` directory is where you would store all the images and
assets for the ch01.html file. Ideally you would keep assets for the chapters
contained in this way so that there would be a 'ch01.html' file and its
corresponding 'ch01/' directory where its assets would live.

`scripts/` directory contains the global site scripts

`src/` directory contains any php libraries or helper functions used in the
index.php page

`styles/` directory contains a 'main.css' file which is used for the global
styles. It also contains a 'page.css' file where you should put page-specific
css styles, and a 'chapters.css' file where you should put chapter-specific css
styles.

## Usage ##

### Main site entry point page ###

`index.php`

This page has been designed to set up the scaffolding for the site and
dynamically add nav items and page content.

The index.php page will also remember what page the user was on. For instance,
if the user hits the reload button on a particular page, the index.php will
load that page (i.e. /#home, will load the home page).  The way it does this
is by adding a url fragment to the url (i.e. #home).  There is a hard coded
list of valid page fragments in scripts/main.js file that will also need to
be updated.

### Chapter pages ###

`/pages/chapters/*.html`

The idea here is that you will continue to render the chapter files from
markdown using whatever process you have in place. The only caveat is that the
rendered .html files should ideally not contain `<html>`, `<head>`, or
`<body>` tags. They should just contain the markup inside the `<body>` tag.

You just need to add chapter pages to the `pages/chapters/` directory and the
site navigation will scale accordingly. Since the navigation looks to this
folder for files, it will build out all the Chapter sub pages in the navigation
automatically.

### Styles and Markup ###

You can change the markup of any of the pages to suit. Keep in mind that since
this site uses the Bookstrap framework, you should use Bootstrap markup and
classes whereever possible. Its not imperative that you do, but highly
suggested since using the Bootstrap styles will keep the content responsive.

See this link for details:
[http://getbootstrap.com/](http://getbootstrap.com/)
