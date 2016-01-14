<?php
require 'vendor/autoload.php';
require 'src/router.php';
require 'src/helpers.php';
try {
    $dotenv = new Dotenv\Dotenv(__DIR__);
    $dotenv->load();
} catch(Exception $e) {
    //assumes sys ENV instead
}
$router = new DDARRouter(basepath());
$router->load();
?>
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="utf-8">
    <meta http-equiv="X-UA-Compatible" content="IE=edge">
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <title>DDAR</title>
    <link rel="stylesheet" href="<?php echo urlfor('/styles/bootstrap/bootstrap.min.css'); ?>"/>
    <link rel="stylesheet" href="<?php echo urlfor('/styles/bootstrap/bootstrap-theme.min.css'); ?>"/>
    <link rel="stylesheet" href="<?php echo urlfor('/styles/main.css'); ?>"/>
    <link rel="stylesheet" href="<?php echo urlfor('/styles/pages.css'); ?>"/>
    <link rel="stylesheet" href="<?php echo urlfor('/styles/chapters.css'); ?>"/>
    <script src="https://ajax.googleapis.com/ajax/libs/jquery/1.11.3/jquery.min.js"></script>
    <script src="<?php echo urlfor('/scripts/bootstrap/bootstrap.min.js'); ?>"></script>
    <script src="<?php echo urlfor('/scripts/main.js'); ?>"></script>
</head>
<body data-basepath="<?php echo $router->getBasepath(); ?>">
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
                <li><a class="page" href="<?php echo urlfor('/pages/home'); ?>">Home</a></li>
                <li class="dropdown">
                    <a href="#" class="content-page dropdown-toggle" data-toggle="dropdown" role="button" aria-haspopup="true" aria-expanded="false">Content<span class="caret"></span></a>
                    <ul class="dropdown-menu">
                    <?php
                    $path = 'pages/chapters';
                    echo renderChapterNavigation($path);
                    ?>
                    </ul>
                </li>
                <li class="dropdown">
                    <a href="#" class="content-page dropdown-toggle" data-toggle="dropdown" role="button" aria-haspopup="true" aria-expanded="false">Using<span class="caret"></span></a>
                    <ul class="dropdown-menu">
                    <li><a class="page" href="<?php echo urlfor('/pages/using'); ?>#r-packages">R Packages</a></li>
                    <li><a class="page" href="<?php echo urlfor('/pages/using'); ?>#data-sets-by-package">Data Sets by Package</a></li>
                    <li><a class="page" href="<?php echo urlfor('/pages/using'); ?>#r-code">R Code</a></li>
                    </ul>
                </li>
                <li><a class="page" href="<?php echo urlfor('/pages/other'); ?>">Other materials</a></li>
                <li><a class="page" href="<?php echo urlfor('/pages/authors'); ?>">Authors</a></li>
            </ul>
        </div><!-- /.navbar-collapse -->
    </div><!-- /.container-fluid -->
</nav>
<div class="container">
    <div class="row">
        <div class="page-content col-md-12">
            <?php echo $router->match(); ?>
        </div>
    </div>
</div>
</body>
</html>
