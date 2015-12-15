<?php
require_once 'src/helpers.php';
?>
<!DOCTYPE html>
<html lang="en">
<head>
	<meta charset="utf-8">
	<meta http-equiv="X-UA-Compatible" content="IE=edge">
	<meta name="viewport" content="width=device-width, initial-scale=1">
	<title>DDAR</title>
	<link rel="stylesheet" href="/vendor/twitter/bootstrap/dist/css/bootstrap.min.css"/>
	<link rel="stylesheet" href="/vendor/twitter/bootstrap/dist/css/bootstrap-theme.min.css"/>
	<link rel="stylesheet" href="styles/main.css"/>
	<link rel="stylesheet" href="styles/pages.css"/>
	<link rel="stylesheet" href="styles/chapters.css"/>
	<script src="https://ajax.googleapis.com/ajax/libs/jquery/1.11.3/jquery.min.js"></script>
	<script src="/vendor/twitter/bootstrap/dist/js/bootstrap.min.js"></script>
	<script src="scripts/main.js"></script>
</head>
<body>
<div class="jumbotron">
	<div class="container"></div>
</div>
<nav class="navbar navbar-default">
	<div class="container-fluid">
		<div class="navbar-header">
			<button type="button" class="navbar-toggle collapsed" data-toggle="collapse" data-target="#bs-example-navbar-collapse-1" aria-expanded="false">
				<span class="sr-only">Toggle navigation</span>
				<span class="icon-bar"></span>
				<span class="icon-bar"></span>
				<span class="icon-bar"></span>
			</button>
		</div>
		<div class="collapse navbar-collapse" id="bs-example-navbar-collapse-1">
			<ul class="nav navbar-nav">
				<li><a class="page" data-url="pages/home.html" href="#home">Home</a></li>
				<li class="dropdown">
					<a href="#" class="content-page dropdown-toggle" data-toggle="dropdown" role="button" aria-haspopup="true" aria-expanded="false">Content<span class="caret"></span></a>
					<ul class="dropdown-menu">
					<?php
                        $webpath = 'pages/chapters';
						$dir = realpath(__DIR__ . '/' . $webpath);
						echo getNavContentPagesHtml($dir, $webpath);
					?>
					</ul>
				</li>
				<li><a class="page" data-url="pages/using.html" href="#using">Using the book</a></li>
				<li><a class="page" data-url="pages/other.html" href="#other">Other materials</a></li>
				<li><a class="page" data-url="pages/authors.html" href="#authors">Authors</a></li>
			</ul>
		</div><!-- /.navbar-collapse -->
	</div><!-- /.container-fluid -->
</nav>
<div class="container">
	<div class="row">
		<div class="col-md-4">
			<div class="page-content"></div>
		</div>
	</div>
</div>
</body>
</html>
