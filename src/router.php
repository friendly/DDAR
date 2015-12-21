<?php
require_once 'src/helpers.php';

// catch-all route
$router = new AltoRouter();
$basepath = rtrim(getenv('BASEPATH'), '/');
if (!empty($basepath)) {
    $router->setBasePath(getenv('BASEPATH'));
}
$router->map('GET', '/[*:path]', function(){});
$match = $router->match();
$content = getPageContent($match);
