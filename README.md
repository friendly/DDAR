# DDAR
Development for the **Discrete Data Analysis with R** web site

## Requirements ##

PHP 5.3.10 or greater (5.4+ preferred)

Web server (e.g. Apache)

## Overview ##

This is a simple custom php/html framework. A high-level overview of how it works,
as follows:

> All site URLs are routed to the `index.php` file.  Based on the URL's path,
> the routing mechanism determines which static .html files
> to serve and loads it in place inside an outer html wrapper page.

## Installation ##

### Apache Web Server ###

As described above, all URLs are routed to the `index.php` file. Apache needs
a few things in place to make this happen:

#### Step 1 ####
You will need to enable **url rewriting** in your Apache `httpd.conf` file, or
add it via a rewriting package.

#### Step 2 ####

If you are using a ``` <VirtualHost>``` directive to define your web site, make
sure it resembles the following.

*Note*, the ``` <Directory>``` block. This allows the project to define an
`.htaccess` file that sets up URL rewriting.

``` apacheconf
<VirtualHost *:80>
	DocumentRoot "/path/to/your/web/directory"
	ServerName web
	<Directory "/path/to/your/web/directory">
		AllowOverride All
		Options FollowSymLinks
	</Directory>
</VirtualHost>
```

If you are not using a virtual host file and are serving a single
web site, then look for the ``` apacheconf <Directory>``` block in Apache's
`httpd.conf` file. Update as follows:

``` apacheconf
AllowOverride All
Options FollowSymLinks
```

#### .htaccess ####

You will also need to add an `.htaccess` file to your site root. This will setup
URL rewriting.

``` apacheconf
RewriteEngine on
RewriteCond %{REQUEST_FILENAME} !-f
RewriteRule . index.php [L]
```

**Note:** If you are using an `Alias` directive, you will need to add a
`RewriteBase` directive to the `.htaccess` file that points to the URL-path
of your Alias.

### Directory permissions ###

After installing the website, please make both the `/scripts/bootstrap`
and `/styles/bootstrap` directories writable by the web server.

## Composer package dependencies ##

### External Libraries ###

- [AltoRouter](http://altorouter.com)
- [Bootstrap](http://getbootstrap.com/)
- [DotEnv](https://github.com/vlucas/phpdotenv)


This project requires [Composer](https://getcomposer.org/) to set up
project dependencies.

### To install composer: ###

``` bash
curl -sS https://getcomposer.org/installer | php -- --filename=composer
```

`composer` is then executable (by php) in the current directory.

#### On project installation ####

Run the following composer commands to install the dependencies:

``` bash
composer install
composer run-script compile
```

#### On project updates ####

Run the following composer commands to update the dependencies:

``` bash
composer update
composer run-script compile
```

**Note:** `run-script compile` copies Bootstrap files to
`{styles,scripts}/bootstrap`.


## Environment variable ##

You will need to create a `.env` file in the project's root directory with
the following environment variable that defines which sub directory your
site lives in.

The following example defined the site being served from the root directory.

``` bash
BASEPATH="/"
```

If you are serving this project from a subdirectory of the website's root,
change the above to point to the location of the subdirectory from the
web root, i.e.

``` bash
BASEPATH="/subdirectory"
```

**Important:** Do not include a trailing slash if you are declaring a sub directory.

**Note:** If you are serving images in .html files, you will need to manually update
their paths to be relative to the sub directory.

## Site Map ##

This site is made up of the following initial files:

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

## Usage ##

### index.php ###

> This is the main framework entry point. This file handles the setup of the
> html document, including the navigation and dynamic inclusion of the pages
> and chapters.
>
> **Note:** All static files in this file, are served via a `urlfor` helper
> function. This helper sets the basepath for any scripts or pages linked to
> from this page. If you plan on adding any files or pages, you will need to
> use this helper as well.

### images/ ###
> This directory contains the global site images

### pages/ ###
> This directory contains the main pages and the chapter pages. The main pages
> in this directory can be updated when needed. They are however hard-coded
> into the navigation in the index.php page. So adding any new pages will
> require you to add them to the index.php navigation markup.

### pages/chapters/ ###
> This directory contains the chapter files. This part of the navigation is
> dynamically generated by the `index.php` file. When listing chapters, the
> `index.php` script scans the `pages/chapters/` directory, looks for any files
> of the form (e.g.) `ch01.html`, and then adds that to the chapter list in the
> chapter navigation drop down menu.

### /pages/chapters/*.html  ###

> This directory is where you would store all the images and assets for the any
> chapter files. Ideally you would keep assets for the chapters contained in
> this way so that there would be a `ch01.html` file and its corresponding
> `ch01/` directory where its assets would live.

> The idea here is that you will continue to render the chapter files from
> markdown using whatever process you have in place. The only caveat is that
> the rendered .html files should ideally not contain `<html>`, `<head>`, or
> `<body>` tags. They should only contain the markup inside the `<body>` tag.

### scripts/ ###
> This directory contains the global site javascript files

### src/ ###
> This directory contains any php libraries or helper functions used in the
> `lindex.php` page. It contains a custom URL Router and some helpers.

### styles/ ###
> This directory contains a `main.css` file which is used for the global
> styles. It also contains a `page.css` file (where you should put
> page-specific css styles), and a `chapters.css` file (where you should put
> chapter-specific css styles).

> You can change the markup of any of the pages to suit. Keep in mind that since
> this site uses the Bootstrap framework, you should use Bootstrap markup and
> classes wherever possible. Its not imperative that you do, but highly
> suggested since using the Bootstrap styles will keep the content responsive.
>
> Read the Bootstrap documentation for full details:
> [http://getbootstrap.com/](http://getbootstrap.com/)
