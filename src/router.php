<?php
require 'vendor/autoload.php';

// catch-all route
$router = new AltoRouter();
$router->map('GET', '[*:path]', function(){});

$match = $router->match();
$path = rtrim($match['params']['path'], '/');

// check for / home
if (empty($path)) {
    $filepath = '/pages/home.html';
} else {
    $filepath = $path . '.html';
}
// load the requested page or 404
$rootpath = realpath(__DIR__ . '/../');
if (!is_file($rootpath . '/' . $filepath)) {
    header($_SERVER['SERVER_PROTOCOL'] . ' 404 Not Found', true, 404);
    $filepath = '/pages/error.html';
}
$content = file_get_contents($rootpath . '/' . $filepath);
