<?php
require_once 'src/router.php';
require_once 'src/helpers.php';
?>
<!DOCTYPE html>
<html lang="en">
<head>
	<meta charset="utf-8">
	<meta http-equiv="X-UA-Compatible" content="IE=edge">
	<meta name="viewport" content="width=device-width, initial-scale=1">
	<title>DDAR</title>
	<link rel="stylesheet" href="/styles/bootstrap/bootstrap.min.css"/>
	<link rel="stylesheet" href="/styles/bootstrap/bootstrap-theme.min.css"/>
	<link rel="stylesheet" href="/styles/main.css"/>
	<link rel="stylesheet" href="/styles/pages.css"/>
	<link rel="stylesheet" href="/styles/chapters.css"/>
	<script src="https://ajax.googleapis.com/ajax/libs/jquery/1.11.3/jquery.min.js"></script>
	<script src="/scripts/bootstrap/bootstrap.min.js"></script>
	<script src="/scripts/main.js"></script>
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
				<li><a class="page" href="/pages/home">Home</a></li>
				<li class="dropdown">
					<a href="#" class="content-page dropdown-toggle" data-toggle="dropdown" role="button" aria-haspopup="true" aria-expanded="false">Content<span class="caret"></span></a>
					<ul class="dropdown-menu">
					<?php
                        $path = 'pages/chapters';
						echo getChapterPageNavHtml($path);
					?>
					</ul>
				</li>
				<li><a class="page" href="/pages/using">Using the book</a></li>
				<li><a class="page" href="/pages/other">Other materials</a></li>
				<li><a class="page" href="/pages/authors">Authors</a></li>
			</ul>
		</div><!-- /.navbar-collapse -->
	</div><!-- /.container-fluid -->
</nav>
<div class="container">
	<div class="row">
		<div class="col-md-offset-1 col-md-10">
			<div class="page-content">
				<?php echo $content; ?>
			</div>
		</div>
	</div>
</div>
</body>
</html>
